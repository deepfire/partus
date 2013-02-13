import cl
import primitives as p

import ast
import builtins
import operator
import types

from cl import error, list_, list__, gensym, gensymname, intern, append, identity, consp, reduce, progv as _progv
from cl import attrify_args, dprintf, gensym_tn, make_keyword_tn
from cl import typep, the, or_t, eql_t, string_t, pyseq_t, pytuple_t, pylist_t, symbol_t
from cl import symbol_value, symbol_name, symbol_package, package_name
from cl import consify_linear, xmap_to_vector, validate_function_args, validate_function_keys
from cl import _if, _primitive, _list
from cl import ir_funcall, ir_apply, ir_cl_call
from cl import t, nil

from primitives import machine, prim, det, indet, body, stmt, expr
from primitives import expr_spill, maybe_expr_spill, efless, potconst, const, literal
from primitives import defmachine, defprim, maybe, satisfies
from primitives import exprp, genname

## Entire registry, essentially.  Automatable?  Worth it?
from primitives import string, symbol, literal_list
from primitives import name, integer, float_num
from primitives import add, sub, mul, div, mod, shl, shr, logior, logxor, logand, lognot, floor, expt
from primitives import eq, lt, le, gt, ge
from primitives import not_
from primitives import cons, car, cdr, rplaca, rplacd, vector, index
from primitives import assign, return_, progn, if_, loop, unwind_protect, catch, throw, resignal
from primitives import function, funcall, apply
from primitives import let, let_, progv
from primitives import special_ref, special_setq

NoneType = type(None)

for x in ["SETQ"]:
        globals()[x.lower()] = intern(x.lower())

def py_error(kind, control, *args):
        error("While PY-lowering %s: " + control, kind, *args)

###
### Python AST
###
def ast_compiled_name(name, *body, function = nil, **keys):
        mod, globals, locals = py_compile_and_load(*body, **keys)
        return locals[function or name]


###
### Python runtime
###
def py_compile_and_load(*body, modname = "", filename = "", lineno = 0, **keys):
        return load_code_object_as_module(
                modname,
                pyb.compile(ast.fix_missing_locations(ast_module(list(body), lineno = lineno)), filename, "exec"),
                register = nil,
                filename = filename,
                **keys)

###
### Python IR -geared primitive extensions
###
def find_indet_method_pool(indet_name):
        return p.__primitives_by_pyname__[indet_name].methods

def defmethod(prim, *tags):
        def do_defmethod(method):
                ## Unregistered Issue METHOD-NEED-DIFFERENTIATION
                primitive_method_pool = find_indet_method_pool(method.__name__)
                for tag in tags: ## HOW DO YOU FUCKING ACCESS THE CLASS OF THE METHOD IT IS BEING DEFINED ON?  NO WAI.
                        primitive_method_pool[tag].add(method)
                return method
        return do_defmethod

def identity_method(*keys):
        return (identity, keys) # just a marker, to be processed by defprim

def defstrategy(*ign, test = lambda *_, **__: True, keys = None, xform = None):
        assert(not ign)
        assert(keys or xform)
        return (defstrategy, test, (xform  if xform                  else
                                    keys   if isinstance(keys, list) else
                                    [keys]))

def defpy(cls):
        def maybe_process_as_strategy(cls, name, spec):
                def strategyp(x): return (isinstance(x, tuple) and x and x[0] is defstrategy and x[1:]
                                          #         See defstrategy above, for the definition of x[1:]
                                          or (None, None))
                # dprintf("trying to process %s as strategy in %s", spec, cls)
                test, xform_or_keys = strategyp(spec)
                # Due to the definition of defstrategy, test cannot be a false-equivalent.
                if test and xform_or_keys:
                        # dprintf("  DEFPY %s, setting strategy %s to %s", cls, name, (test, xform_or_keys))
                        # implementation strategy
                        cls.help_strategies.append((name, test, xform_or_keys))
                        cls.help_strategies.sort()
                        return t
        def primitive_add_method_keys(primitive_method_pool, method, keys):
                for key in keys:
                        primitive_method_pool[key].add(method)
        clsname = cls.__name__
        prim_maybe_cls = getattr(p, clsname, None)
        this, cls = cls, (prim_maybe_cls if isinstance(prim_maybe_cls, type) else
                          globals().get(clsname, None))
        if not cls:
                error("In DEFPY %s: unknown primitive.", clsname.upper())
        # dprintf("DEFPY: found %s (of type %s) for method stuffing", cls, type(cls))
        class_key_supers = cls.__mro__[0:cls.__mro__.index(prim)]
        help_stdmethod = cls.__dict__.get("help", None)
        # dprintf("DEFPY %s/%x:   %s", cls, id(cls), cls.__dict__.items())
        for n, method_spec in this.__dict__.items():
                if (n in ("__module__", "__locals__", "__doc__", "__new__",
                          "methods", "help_strategies", "form_specifier", "help", "value", "cfg") or
                    maybe_process_as_strategy(cls, n, method_spec)):
                        if n is "help":
                                # decorating all the stuff into staticmethod in-text would be a real shame
                                cls.help = staticmethod(method_spec)
                        continue
                ## otherwise, must be an indet method
                method, identityp, keys = \
                    ((method_spec,    nil, ())  if isinstance(method_spec, (types.FunctionType,
                                                                            builtins.staticmethod)) else
                     (cls, t,   method_spec[1]) if (isinstance(method_spec, tuple) and
                                                               method_spec[0] is identity) else
                     error("Invalid method specifier: %s", method_spec))
                indet_method_pool = find_indet_method_pool(n)
                primitive_add_method_keys(indet_method_pool, method, keys)
                primitive_add_method_keys(indet_method_pool, method, class_key_supers)
        return globals().get(clsname) ## Preserve original globals, if any.

def find_method(cls, tags):
        "Find *the* single method matching all tags."
        assert(tags)
        this, *rest = tags
        sett = set(cls.methods[this]) ## The lack of the 'everything' set.  Oh.
        while rest:
                this, *rest = rest
                sett &= cls.methods[this]
        if not sett:
                error("Could not find primitive method %s for tags %s.", cls.__name__, tags)
        if len(sett) > 1:
                error("Ambiguous method specification: primitive %s for tags %s.", cls.__name__, tags)
        return sett.pop()
p.prim.find_method = classmethod(find_method)

def determine(cls, args, keys):
        for name, test, xform_or_keys in cls.help_strategies:
                desc = "applicability predicate for method %s.%s" % (cls.__name__.upper(), name)
                validate_function_args(desc, test, args)
                validate_function_keys(desc, test, keys)
                if test(*args, **keys):
                        ## Simplify.
                        xf = (xform_or_keys if not isinstance(xform_or_keys, list) else
                              cls.find_method(xform_or_keys))
                        # if cls is progn:
                        #         # dprintf("PROGN-DET args %s", args)
                        # dprintf("xf %s", xf)
                        return xf(*args, **keys)
        else:
                error("Unhandled primitive form: %s / %s", (cls.__name__.upper(),) + args, keys)

def redetermining_as(cls, **keys):
        return lambda x: determine(cls, x.args, keys)

_compiler_trace_primitives_ = cl._compiler_trace_primitives_

def help(x) -> ([stmt], expr):
        if not isinstance(x, prim):
                return [], x
        with _progv({ cl._pp_base_depth_: cl.pp_base_depth() + 3 }):
                meth_name = type(x).__name__.upper()
                def handler(cond):
                        error("While calling %s.%s, caught:\n%s", meth_name, x.help, cond)
                validate_function_keys("%s.HELP" % meth_name, x.help, x.keys)
                validate_function_args("%s.HELP" % meth_name, x.help, x.args)
                r = x.help(*x.args, **x.keys)
                # r = handler_bind(lambda: x.help(*x.args, **x.keys),
                #                  (Exception, handler))
        p, v = (([], r) if not isinstance(r, ast.stmt) and isinstance(r, ast.AST) else
                ## list(r) if isinstance(r, tuple) else
                ## Unregistered Issue SLOW-CHECK
                r if typep(r, (pytuple_t, (pylist_t, ast.stmt), ast.expr))        else
                error("Invalid output from lowerer for %s (%s/%s)\n-- %s.", x, x.help, defun.help, r))
        if not isinstance(x, name) and symbol_value(cl._compiler_trace_subastification_):
                ssp = sex_space()
                dprintf("%s---- helpery %s --->\n"
                           "%s%s\n"
                           "%s%s\n",
                           ssp, cl.pp_chain_of_frame(cl.caller_frame(-1), callers = 15),
                           ssp, x,
                           ssp, ("\n" + ssp).join(pp_ast_as_code(x) for x in p + [v]))
        return p or [], v

def help_expr(x) -> expr:
        p, v = help(x)
        p and error("Helped %s to non-expr %s, %s, where an expression result was expected.", x, p, v)
        return v

def help_exprs(xs) -> [expr]:
        return [ help_expr(x) for x in xs ]

def help_prog(xs) -> [stmt]:
        p_acc = []
        for x in the((pyseq_t, prim), xs):
                p, v = help(x)
                p_acc.extend(p)
                if not cl.ast_efless_p(v):
                        p_acc.append(ast.Expr(v))
        return p_acc

def help_progn(xs) -> ([stmt], expr):
        assert(xs)
        xs, vf = xs[:-1], xs[-1]
        p_acc = help_prog(xs)
        p, v = help(vf)
        p_acc.extend(p)
        return p_acc, v

def help_args(fixed, opts, optvals, args, keys, keyvals, restkey):
        assert(len(opts) == len(optvals) and
               len(keys) == len(keyvals))
        return ast.arguments(
                args             = [ ast.arg(x.value(), None) for x in fixed + opts ],
                vararg           = args.value()    if args    else None,
                varargannotation = None,
                kwonlyargs       = [ ast.arg(x.value(), None) for x in         keys ],
                kwarg            = restkey.value() if restkey else None,
                kwargannotation  = None,
                defaults         = help_exprs(optvals),
                kw_defaults      = help_exprs(keyvals))

def help_ctx(writep):
        return (ast.Store if writep else ast.Load)()

def help_nil():
        return help(p.prim_nil() if symbol_value(p._valueless_primitive_statement_must_yield_nil_) else
                    name("None"))[1]

###
### Spill theory
###
### Q1: what end goals are we trying to attain?
### seeming candidates are:
### - determine indeterminate primitives
###   - maximise the use of expressions
###   - punt to statements where we cannot
### - preserve the correct order and time of evaluation
###
### Problematic primitives.  Basically, anything with pieces having different time of evaluation.
###
### (IF spillable unspillable unspillable)
###   ..we still need to spill unspillables, but.. thunk allocation, in normal mode, just to delay..
###   Better to solve unspillables through stmt form.
###   ..still we'd like to be able to spill the spillable, independent of the expr-ness of the chosen
###   primitive.
###
### (LAMBDA (const &OPTIONAL (const spillable)...) unspillable)
###   same logic for unspillables leading to a stmt form.
###   The spillables present a problem for preservation of evaluation order.
###
### The transient logic of skies:
###
### HELP needs final expr-ness for dispatch decision making.
### Exprness, thus, must be an immediate property of the primitive.
### Exprness is a product of the primitive's kind and its spills.
### Expression spills is a recursive property.
### Spill computation is expensive and needs to be obtained at different nest levels, and thus must be cached.
### Primitive, thus, at the time it reaches this stage, must be an object, to store the cached property.
### Only applicatively positioned subforms can be conveniently spilled (proven to be true only partially).
###
def prim_check_and_spill(primitive) -> prim:
        def check_prim_type(arg, type) -> bool:
                if not typep(arg, type):
                        raise p.primitive_mismatch("type mismatch",
                                                   prim = primitive, pspec = primitive.form_specifier,
                                                   spec = type, form = arg)
        def tuple_spills(spec, args, force_spill) -> ([stmt], [expr]):
                specialp = spec and spec[0] in [maybe, satisfies]
                if specialp:
                        if spec[0] is maybe:
                                if args is None:
                                        return [], None
                                return process(spec[1], args, force_spill)
                        if spec[0] is satisfies:
                                return type_check((satisfies_t, spec[1]), args, None)
                segmentp = spec and isinstance(spec[-1], list)
                check_prim_type(args, (or_t, tuple, list))
                nspec, nargs = len(spec), len(args)
                if not (segmentp and nargs >= (nspec - 1)
                        or nspec is nargs):
                        error("Invalid primitive %s: subform %s has %d elements, but %s was/were expected.",
                              primitive, args, nargs,
                              ("at least %s" % (nspec - 1)) if segmentp else
                              ("exactly %d" % nspec))
                a_fixed, a_segment, s_spec = ((args[:nspec - 1], args[nspec - 1:], spec[-1][0]) if segmentp else
                                              (args,             [],               None))
                def expr_tuple_spill_partition(spec, args):
                        pre_spills = []
                        for s, a in zip(spec, a_fixed):
                                pre_spills.append(process(s,      a, force_spill))
                        for a in a_segment:
                                pre_spills.append(process(s_spec, a, force_spill))
                        ## only allowable spills will land here
                        last_spilled_posns = [ i
                                               for i, x in reversed(list(enumerate(pre_spills)))
                                               if x[0] ]
                        last_spilled_posn = last_spilled_posns[0] if last_spilled_posns else None
                        n_spilled = (last_spilled_posn + 1 if last_spilled_posn is not None else
                                     0)
                        for_spill, unspilled = args[:n_spilled], args[n_spilled:]
                        n_segspill = max(0, n_spilled - len(a_fixed))
                        return for_spill, ((spec[:-1] + (s_spec,) * n_segspill)
                                           if n_segspill else
                                           spec[:n_spilled]), unspilled
                for_spill_as, for_spill_ss, unspilled = expr_tuple_spill_partition(spec, args)
                ## Re-collecting spills, while forcing spill for unspilled spillables.
                spills, subforms = [], []
                for s, a in zip(for_spill_ss, for_spill_as):
                        spill, subform = process(s, a, for_spill_as)
                        spills.extend(spill)
                        subforms.append(subform)
                return (spills,
                        tuple(subforms) + tuple(unspilled))
        def maybe_spill(spec, arg, force_spill) -> ([stmt], prim):
                maybep = spec is maybe_expr_spill
                check_prim_type(arg, ((or_t, (eql_t, nil), expr, stmt) if maybep else
                                      (or_t, expr, stmt)))
                # Debug this: if (isinstance(arg, expr) and (not force_spill or (maybep and arg is nil))):
                ## Spill, iff any of the conditions hold:
                if (maybep and arg is nil
                    or not (force_spill
                            or isinstance(arg, stmt))):
                        return ([],
                                arg)
                tn = genname("EXP") ## Temporary Name.
                return ([ assign(tn, arg) ],
                        tn)
        def type_check(spec, arg, force_spill) -> ([stmt], t):
                check_prim_type(arg, spec)
                return ([],
                        arg)
        def process(spec, arg, force_spill) -> ([stmt], (or_t, prim, [expr])):
                isinstance(spec, list) and error("List type specifier (%s), outside of appropriate context.", spec)
                return (maybe_spill  if spec in (expr_spill, maybe_expr_spill) else
                        tuple_spills if isinstance(spec, tuple)                else
                        type_check)(spec, arg, force_spill)
        spills, primitive.args  = tuple_spills(primitive.form_specifier, primitive.args, nil)
        return (progn(*spills + [primitive]) if spills else
                primitive)

###
### Python-geared IR exprness-specific definitions
###
@defpy
class string(literal):
        def help(name): return ast.Str(name)

@defpy
class symbol(literal):
        def value(x, writep = nil):
                return x
        def help(x, writep = nil):
                return ast.Name(x, help_ctx(writep))

@defpy
class literal_list(literal):
        def help(*xs):
                return reduce(lambda cdr, car: ast.List([help_expr(car), cdr], help_ctx(nil)),
                              ## Namespace separation leak:
                              reversed(xs + (help_nil(),)))

@defpy
class name(expr):
        def help(x, writep = nil, globalp = nil):
                return ast.Name(x, help_ctx(writep))

@defpy
class integer(literal):
        def help(x): return ast.Num(x)

@defpy
class float_num(literal):
        def help(x): return ast.Num(x)

@defprim(intern("DEFUN")[0],
         (name, ((maybe, name), [name]),
          prim))
class defun(body): ...

@defpy
class defun(body):
        def help(name, args, body, pydecorators = []):
                maybe_rest, *fixed_args = args
                return [ ast.FunctionDef(
                                name = name.value(),
                                args = help_args(fixed_args, [], [], maybe_rest, [], [], None),
                                decorator_list = help_exprs(pydecorators),
                                returns = None,
                                body = help(return_(body))[0])
                         ], help_expr(name)
        @defmethod(function)
        def function(name, args, expr, id = nil, pydecorators = []):
                return defun(name or genname((id) if id else "DEFLAM"), args, expr, pydecorators = pydecorators)

@defprim(intern("LAMBDA-EXPR")[0],
         (NoneType, ((maybe, name), [name]),
          expr))
class lambda_expr(expr): ...

@defpy
class lambda_expr(expr):
        def help(name, args, expr, id = nil, pydecorators = []):
                assert not (name or pydecorators)
                maybe_rest, *fixed_args = args
                return ast.Lambda(help_args(fixed_args, [], [], maybe_rest, [], [], None),
                                  help_expr(expr))
        function = identity_method()

@defprim(intern("LET-EXPR")[0],
         (([(name, expr_spill)],), ## Unregistered Issue VALIDATE-CORRECT-LET-SPILL-ORDER
          expr))
class let_expr(expr): ...

@defpy
class let_expr(expr):
        def help(bindings, expr):
                ns, vs = list(zip(*bindings))
                return help(funcall(lambda_expr(None, (None,) + ns,
                                                expr),
                                    *vs))
        let = identity_method()

@defprim(intern("LET-THUNK")[0],
         (([(name, expr_spill)],), ## Unregistered Issue VALIDATE-CORRECT-LET-SPILL-ORDER
          prim))
class let_thunk(body): ...

@defpy
class let_thunk(body):
        "The most universal, yet bulky kind of LET."
        def help(bindings, body):
                ns, vs = list(zip(*bindings))
                tn = genname("LET_THUNK")
                return help(progn(defun(tn, (None,) + ns,
                                        body),
                                  funcall(tn, *vs)))
        let = identity_method()

@defprim(intern("LET*-SETQ")[0],
         (([(name, prim)],),
          prim)) ## This one handles binding spills by virtue of using ASSIGN.
class let__setq(body): ...

@defpy
class let__setq(body):
        "Can only be used as a tail, when it can be proved that no unwind will use mutated variables."
        def help(bindings, body, head = None):
                sum = (tuple(assign(n, v) for n, v in bindings)
                       + (body,))
                return help(progn(*sum))
        let_ = identity_method(setq)

@defprim(intern("LET*-EXPR")[0],
         (([(name, expr)],),
          expr))
class let__expr(expr): ...

@defpy
class let__expr(expr):
        def help(bindings, expr):
                return help_expr(let_expr((bindings[0],),
                                          let_(bindings[1:],
                                               expr))
                                 if bindings else
                                 expr)
        let_ = identity_method()

@defprim(intern("LET*-STMT")[0],
         (([(name, expr)],),
          prim))
class let__stmt(body): ...

@defpy
class let__stmt(body):
        def help(bindings, body):
                return help(let((bindings[0],),
                                let_(bindings[1:],
                                     body))
                            if bindings else
                            body)
        let_ = identity_method()

# @defprim(intern("PROGN-STMT")[0],
#          ([prim],))
# class progn_stmt(body):
@defpy
class progn(body):
        def help(*body):
                return help_progn(body)
        progn = identity_method()

@defpy
class if_(indet):
        a_expr = defstrategy(test = lambda _, conseq, ante: (exprp(conseq) and
                                                             exprp(ante)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

@defprim(intern("IF-EXPR")[0],
         (expr_spill, expr, expr))
class if_expr(expr): ...

@defpy
class if_expr(expr):
        def help(*tca):
                return ast.IfExp(*[help_expr(x) for x in tca]) # Test, Consequent, Antecedent.
        if_ = identity_method()

@defprim(intern("IF-STMT")[0],
         (expr_spill, prim, prim))
class if_stmt(body): ...

@defpy
class if_stmt(body):
        def help(*tca):
                (_, tv), (cp, cv), (ap, av) = [ help(x) for x in tca ]
                tn = genname("IFVAL")
                return [ ast.If(tv,
                                cp + [ ast.Assign([ help_expr(tn) ], cv)],
                                ap + [ ast.Assign([ help_expr(tn) ], av)]
                                ) ], help_expr(tn)
        if_ = identity_method()
## This is no high-level it hurts!  Obviously needed for intrinsics.
@defprim(intern("ATTR-REF")[0],
         (prim, prim))
class attr(indet): ...

@defpy
class attr(indet):
        a_const = defstrategy(test = lambda _, attr: isinstance(attr, string),
                              keys = "const attr")
        b_var   = defstrategy(keys = efless)

@defprim(intern("CONST-ATTR-REF")[0],
         (expr_spill, string))
class const_attr(efless): ...

@defpy
class const_attr(efless):
        ## We assume, that within the domain of emitted code, objects do not have accessors defined.  So, efless.
        def help(x, attr, writep = nil): return ast.Attribute(help_expr(x), attr.value(), help_ctx(writep))
        attr = identity_method("const attr")

@defprim(intern("VAR-ATTR-REF")[0],
         (expr_spill, expr_spill))
class var_attr(efless): ...

@defpy
class var_attr(efless):
        def help(x, attr):
                return help(funcall(name("getattr"), help_expr(x), help_expr(attr)))
        attr = identity_method()

def prim_attr_chain(xs, writep = nil):
        return reduce((lambda acc, attr: const_attr(acc, attr, writep = writep)),
                      xs[1:],
                      xs[0])

@defprim(intern("SLICE")[0],
         (expr_spill, expr_spill, maybe_expr_spill, maybe_expr_spill))
class slice(expr): ...

@defpy
class slice(expr):
        def help(x, start, end, step, writep = nil):
                return ast.Subscript(help_expr(x), ast.Slice(help_expr(start),
                                                             help_expr(end if end is not nil else
                                                                       name("None")),
                                                             help_expr(step if step is not nil else
                                                                       name("None"))),
                                     help_ctx(writep))

@defprim(intern("FILTERMAP")[0],
         (name, expr_spill, prim, (maybe, prim)))
class filtermap(indet): ...

@defpy
class filtermap(indet):
        a_expr = defstrategy(test = lambda name, iter, body, condition = None: (exprp(body)
                                                                                and (exprp(condition) or condition is None)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

@defprim(intern("FILTERMAP-EXPR")[0],
         (name, expr_spill, expr, (maybe, expr)))
class filtermap_expr(expr): ...

@defpy
class filtermap_expr(expr):
        def help(var, iter, body, condition):
                conditional = isinstance(condition, name) and condition.value == "None"
                return ast.ListComp(help_expr(body),
                                    [ ast.comprehension(help_expr(var), help_expr(iter),
                                                        [ help_expr(condition) ] if conditional else
                                                        []) ] )
        filtermap = identity_method()

@defprim(intern("FILTERMAP-STMT")[0],
         (name, expr_spill, prim, (maybe, prim)))
class filtermap_prim(body): ...

@defpy
class filtermap_prim(body):
        def help(name, iter, body, condition = None):
                tn = genname("FILTERMAP_THUNK")
                return help(progn(defun(tn, (None, name),
                                        body),
                                  filtermap(name, iter, funcall(tn, name), condition)))
        filtermap = identity_method()

@defprim(intern("GENERATOR")[0],
         (expr, [(expr, expr, [expr])]))
class generator(expr): ...

@defpy
class generator(expr):
        def help(result, *comps):
                return ast.GeneratorExp(help_expr(result),
                                        [ ast.comprehension(help_expr(target), help_expr(iter),
                                                            [ help_expr(if_) for if_ in ifs ])
                                          for target, iter, *ifs in comps ])

@defprim(intern("IMPL-REF")[0],
         (str,))
class impl_ref(expr): ...

@defpy
class impl_ref(expr):
        def help(x):
                return cl.ast_attribute_chain(["cl", x])

@defprim(intern("BUILTIN-REF")[0],
         (string_t,))
class blin_ref(expr): ...

@defpy
class blin_ref(expr):
        def help(x):
                return help_expr(name(x))

@defprim(intern("IMPORT")[0],
         ([string_t],))
class import_(stmt): ...

@defpy
class import_(stmt):
        def help(*xs):
                return [ ast.Import([ ast.alias(x, None) for x in xs ])
                         ], help_nil()

@defprim(intern("DEL")[0],
         ([expr],))
class delete(stmt): ...

@defpy
class delete(stmt):
        def help(*exprs):
                return [ ast.Delete([ help_expr(x) for x in exprs ])
                         ], help_nil()

@defprim(intern("GLOBAL")[0],
         ([name],))
class global_(stmt): ...

@defpy
class global_(stmt):
        def help(*xs):
                return [ ast.Global([ x.value() for x in xs ]
                                  ) ], help_nil()

@defprim(intern("NONLOCAL")[0],
         ([name],))
class nonlocal_(stmt): ...

@defpy
class nonlocal_(stmt):
        def help(*xs):
                return [ ast.Nonlocal([ x.value() for x in xs ]
                                      ) ], help_nil()


@defprim(intern("ASSERT")[0], (expr_spill, expr_spill))
class assert_(stmt): ...

@defpy
class assert_(stmt):
        def help(condition, description):
                return [ ast.Assert(help_expr(condition), help_expr(description)
                                    ) ], help_nil()


## We cannot spill these, so they need to be treated specially.
def wrap_raw_ast(x):
        def wrap(x):
                return ([ rec(x) for x in x ]
                        if isinstance(x, list) else
                        rec(x))
        def rec(x):
                return (raw(type(x).__name__, *(wrap(getattr(x, slot))
                                                for slot in type(x)._fields))
                        if isinstance(x, ast.AST) else
                        x)
        return rec(x)

@defprim(intern("RAW")[0],
         ((satisfies, lambda x: hasattr(ast, x) and issubclass(getattr(ast, x), ast.AST)),
          [t]))
class raw(indet): ...

@defpy
class raw(indet):
        a_stmt = defstrategy(test = lambda cls_name, *args, **keys: issubclass(getattr(ast, cls_name), ast.stmt),
                             keys = stmt)
        b_expr = defstrategy(keys = expr)

@defprim(intern("RAW-EXP")[0],
         ((satisfies, lambda x: hasattr(ast, x) and issubclass(getattr(ast, x), ast.AST)),
          [t]))
class raw_expr(expr): ...

@defpy
class raw_expr(expr):
        def help(cls_name, *args, **keys):
                cls = getattr(ast, cls_name)
                return cls( *(([ help_expr(x)
                                 for x in x ] if isinstance(x, (tuple, list)) else
                               help_expr(x))
                              for x in args),
                             **{k: help_expr(v)
                                   for k, v in keys.items()})
        raw = identity_method()

@defprim(intern("RAW-STMT")[0],
         ((satisfies, lambda x: hasattr(ast, x) and issubclass(getattr(ast, x), ast.AST)),
          [t]))
class raw_stmt(stmt): ...

@defpy
class raw_stmt(stmt):
        def help(cls_name, *args, **keys):
                cls = getattr(ast, cls_name)
                return cls( *((tuple(help_expr(x) for x in x) if isinstance(x, tuple) else
                               help_expr(x))
                              for x in args),
                             **{k: help_expr(v)
                                   for k, v in keys.items()})
        raw = identity_method()

@defprim(intern("LITERAL-HASH-TABLE-EXPR")[0],    ([expr_spill],))
## Unregistered Issue EXTREME-NICETY-OF-AUTOMATIC-RECLASSIFICATION-TO-A-NARROWER-TYPE
class literal_hash_table_expr(expr): ...

@defpy
class literal_hash_table_expr(expr):
        def help(*ksvs):
                if len(ksvs) % 2:
                        error("In LITERAL-HASH-TABLE-EXPR: odd number of arguments: %s", pp_consly(ksvs))
                keys, vals = ksvs[0::2], ksvs[1::2]
                return ast.Dict(help_exprs(keys), help_exprs(vals))

@defprim(intern("PYLIST")[0],
         ([expr_spill],))
class pylist(expr): ...

@defpy
class pylist(expr):
        def help(*xs):
                return ast.List([ help_expr(x) for x in xs ], help_ctx(nil))

@defprim(intern("PYTUPLE")[0],
         ([expr_spill],))
class pytuple(expr): ...

@defpy
class pytuple(expr):
        def help(*xs):
                return ast.Tuple([ help_expr(x) for x in xs ], help_ctx(nil))

@defprim(intern("PYSET")[0],
         ([expr_spill],))
class pyset(expr): ...

@defpy
class pyset(expr):
        def help(*xs):
                return ast.Set([ help_expr(x) for x in xs ])

@defpy
class index(expr):
        def help(x, index, writep = nil):
                return ast.Subscript(help_expr(x), ast.Index(help_expr(index)), help_ctx(writep))

@defpy
class cons(expr):
        def help(car, cdr): return ast.List([ help_expr(car),
                                              help_expr(cdr) ])

@defpy
class car(expr):
        def help(cons): return ast.Subscript(help_expr(cons), ast.Index(0), ast.Load())

@defpy
class cdr(expr):
        def help(cons): return ast.Subscript(help_expr(cons), ast.Index(1), ast.Load())

@defpy
class rplaca(expr):
        def help(cons, value):
                return help(assign(index(cons, integer(0), writep = t),
                                   value))

@defpy
class rplacd(expr):
        def help(cons, value):
                return help(assign(index(cons, integer(1), writep = t),
                                   value))

@defpy
class assign(stmt):
        def help(name, value):
                p, v = help(value)
                return (p
                        + [ ast.Assign([ help_expr(name) ], v) ]
                        ), help_expr(name)

@defpy
class return_(stmt):
        def help(x):
                return [ ast.Return(help_expr(x)) ], help_nil()

@defpy
class function(indet):
        "NOTE: default value form evaluation is not delayed."
        a_expr = defstrategy(test = (lambda name, args, body, id = nil, pydecorators = []:
                                             exprp(body) and not (name or pydecorators)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

@defpy
class let(indet):
        a_expr = defstrategy(test = lambda bindings, body: (all(exprp(x)
                                                                for x in list(zip(*bindings))[1])
                                                            and exprp(body)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

def bindings_free_eval_order_p(bindings):
        return (all(isinstance(form, const)
                    for _, form in bindings)
                or (all(isinstance(form, (name, const))
                        for _, form in bindings)
                    and not (set(zip(*bindings)[0]) &
                             set(zip(*bindings)[1]))))

@defpy
class let_(indet):
        ## Case for uncaught tail:
        ##
        ## Lisp:
        ## (let ((foo 0))
        ##   (handler-case
        ##       (let* ((foo 1))
        ##         (error "Foo."))
        ##     (error ()
        ##       foo)))
        ##
        ## Naive SETQ-BASED LET* implementation:
        ## foo = 0
        ## try:
        ##         foo = 1
        ##         return error("Foo.")
        ## except Exception as _:
        ##         return foo
        ##
        ## Note that the call to ERROR is in a HEAD position,
        ## unless we introduce the HEADness-breaking property,
        ## which, then, would be attributed to the likes of LOOP and HANDLER-BIND.
        a_head        = defstrategy(test  = lambda _, *__, head = nil, uncaught_tail = nil: head or uncaught_tail,
                                    keys  = setq)
        b_reorderable = defstrategy(test  = lambda bindings, *_: bindings_free_eval_order_p(bindings),
                                    xform = redetermining_as(let))
        # TODO: try to reduce frame creation, even in the non-reorderable case.
        c_expr        = defstrategy(test  = lambda bindings, body: (all(exprp(x)
                                                                        for x in list(zip(*bindings))[1])
                                                                    and exprp(body)),
                                    keys  = expr)
        d_stmt        = defstrategy(keys  = stmt)

@defpy
class progv(body):
        def help(vars, vals, body):
                tn = genname("VALUE")
                return [ ast.With(help_expr(funcall(impl_ref("progv"),
                                                    literal_hash_table_expr(*reduce(operator.add, zip(vars, vals))))),
                                  None,
                                  help(assign(tn, body))[0])
                         ], help_expr(tn)

@defpy
class progn(body):
        ## Make this special, to provide instantiation-time elision of redundant PROGNs.
        ...

@defpy
class funcall(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, *fixed_args):
                return ast.Call(help_expr(func),
                                help_exprs(fixed_args), [], None, None)

@defpy
class apply(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, arg, *args):
                fixed_args, restarg = (((arg,) + args[:-1], args[-1]) if args else
                                       ([],                 arg))
                return ast.Call(help_expr(func), help_exprs(fixed_args), [], help_expr(restarg), None)

@defpy
class unwind_protect(body):
        def help(protected_form, body):
                # need a combinator for PRIM forms
                if body:
                        tn = genname("UWP_VALUE")
                        return [ ast.TryFinally(help(assign(tn, protected_form))[0],
                                                help_prog([body])
                                                ) ], help_expr(tn)
                else:
                        return help(protected_form)

@defpy
class loop(body):
        def help(body):
                return [ ast.While(help_expr(name("True")),
                                   help_prog([body]),
                                   []) ], help_nil()

@defpy
class resignal(stmt):
        def help():
                return [ ast.Raise(exc = None,
                                   cause = None) ], help_nil() ## Python behavior mismatches doc: exc/cause documented as expr?.

@defpy
class catch(body):
        ## Lift this to a known?
        def help(tag, body):
                val_tn, ex_tn = genname("BODY_VALUE"), genname("EX")
                return [ ast.TryExcept(
                                help(assign(val_tn, body))[0],
                                [ ast.ExceptHandler(help_expr(impl_ref("__catcher_throw__")),
                                                    ex_tn.value(),
                                                    help(if_(eq(attr(ex_tn, string("ball")),
                                                                tag),
                                                             progn(funcall(impl_ref("__catch_maybe_reenable_pytracer"),
                                                                           ex_tn),
                                                                   assign(val_tn,
                                                                          attr(ex_tn, string("value")))),
                                                             resignal()))[0]) ],
                                [])
                         ], help_expr(val_tn)

@defpy
class throw(expr):
        def help(tag, value):
                return help_expr(funcall(impl_ref("throw"), tag, value))

@defpy
class special_ref(efless):
        def help(name):
                return help(funcall(impl_ref("symbol_value"), name))

@defpy
class special_setq(expr):
        def help(nom, value):
                return help(funcall(impl_ref("do_set"), nom, value, name("None")))

###
### Operations
###
def help_unop(op, x):        return ast.UnaryOp(op(), help_expr(x))
def help_binop(op, x, y):    return ast.BinOp(help_expr(x), op(), help_expr(y))
def help_compare(op, x, ys): return ast.Compare(help_expr(x), [op()] * len(ys), [ help_expr(y) for y in ys ])

def help_binop_seq(args, type, one):
        init, rest = ((args[0], args[1:]) if args else (one, args))
        return reduce(lambda x, y: ast.BinOp(x, type(), help_expr(y)),
                      rest, help_expr(init))

def help_boolop(op, xs):     return ast.BoolOp(op(), [ help_expr(x) for x in xs ])

### primitive
@defpy
class add(potconst):
        def help(*xs):  return help_binop_seq(xs, ast.Add, 0)

@defpy
class sub(potconst):
        def help(*xs):  return help_binop_seq(xs, ast.Sub, 0)

@defpy
class mul(potconst):
        def help(*xs):  return help_binop_seq(xs, ast.Mult, 1)

@defpy
class div(potconst):
        def help(*xs):  return help_binop_seq(xs, ast.Div, 1)

@defpy
class mod(potconst):
        def help(x, y): return help_binop(ast.Mod, x, y)

@defpy
class shl(potconst):
        def help(x, y): return help_binop(ast.LShift, x, y)

@defpy
class shr(potconst):
        def help(x, y): return help_binop(ast.RShift, x, y)

@defpy
class logior(potconst):
        def help(x, y): return help_binop(ast.BitOr, x, y)

@defpy
class logxor(potconst):
        def help(x, y): return help_binop(ast.BitXor, x, y)

@defpy
class logand(potconst):
        def help(x, y): return help_binop(ast.BitAnd, x, y)

@defpy
class lognot(potconst):
        def help(x):    return help_unop(ast.Invert, x)

@defpy
class eq(potconst):
        def help(x, y): return help_compare(ast.Is, x, [y])

@defprim(intern("NEQ")[0], (expr_spill, expr_spill))
class neq(potconst): ...

@defpy
class neq(potconst):
        def help(x, y):          return help_compare(ast.IsNot, x, [y])

@defpy
class lt(potconst):
        def help(x, y): return help_compare(ast.Lt, x, [y])

@defpy
class le(potconst):
        def help(x, y): return help_compare(ast.LtE, x, [y])

@defpy
class gt(potconst):
        def help(x, y): return help_compare(ast.Gt, x, [y])

@defpy
class ge(potconst):
        def help(x, y):          return help_compare(ast.GtE, x, [y])

@defpy
class floor(potconst):
        def help(x, y): return help_binop(ast.FloorDiv, x, y)

@defpy
class expt(potconst):
        def help(x, y): return help_binop(ast.Pow, x, y)

### logic
@defpy
class not_(potconst):
        ## Optimisation: fold (NOT (EQ X Y)) to ast.IsNot
        def help(x): return help_unop(ast.Not, x)

@defprim(intern("AND")[0], ([expr_spill],))
class and_(potconst): ...

@defpy
class and_(potconst):
        def help(*xs): return help_boolop(ast.And, xs)

@defprim(intern("OR")[0], ([expr_spill],))
class or_(potconst): ...

@defpy
class or_(potconst):
        def help(*xs): return help_boolop(ast.Or, xs)

### high-level
@defprim(intern("EQUAL")[0], (expr_spill, [expr_spill]))
class equal(potconst): ...

@defpy
class equal(potconst):
        def help(x, *ys): return help_compare(ast.Eq, x, ys)

@defprim(intern("NOT-EQUAL")[0], (expr_spill, [expr_spill]))
class nequal(potconst): ...

@defpy
class nequal(potconst):
        def help(x, *ys): return help_compare(ast.NotEq, x, ys)

@defprim(intern("IN")[0], (expr_spill, expr_spill))
class in_(potconst): ...

@defpy
class in_(potconst):
        def help(x, y): return help_compare(ast.In, x, [y])

@defprim(intern("NOT-IN")[0], (expr_spill, expr_spill))
class not_in(potconst): ...

@defpy
class not_in(potconst):
        def help(x, y): return help_compare(ast.NotIn, x, [y])

###
### The Python machine definition
###
fixupp = gensym("FIXUPP")

class name_context_fixer(ast.NodeTransformer):
        def visit_Name(w, o):
                return (ast.Name(o.id, ast.Store()) if symbol_value(fixupp) else
                        o)
        def visit_Assign(w, o):
                with _progv({ fixupp: t }):
                        targets = [ w.visit(x)
                                    for x in o.targets ]
                return ast.Assign(targets = targets,
                                   value = w.visit(o.value))
        def visit_AugAssign(w, o):
                with _progv({ fixupp: t }):
                        target = w.visit(o.target)
                return ast.AugAssign(target = target,
                                      op = o.op,
                                      value = w.visit(o.value))
        def visit_For(w, o):
                with _progv({ fixupp: t }):
                        target = w.visit(o.target)
                return ast.For(target = target,
                                iter = w.visit(o.iter),
                                body = [ w.visit(x) for x in o.body ],
                                orelse = [ w.visit(x) for x in o.orelse ])
        def visit_With(w, o):
                with _progv({ fixupp: t }):
                        optional_vars = w.visit(o.optional_vars) if o.optional_vars else None
                return ast.With(context_expr = w.visit(o.context_expr),
                                 optional_vars = optional_vars,
                                 body = [ w.visit(x) for x in o.body ])
        def visit_comprehension(w, o):
                with _progv({ fixupp: t }):
                        target = w.visit(o.target)
                return ast.comprehension(target = target,
                                          iter = w.visit(o.iter),
                                          ifs = [ w.visit(x) for x in o.ifs ])
        def visit_Subscript(w, o):
                writep = symbol_value(fixupp)
                with _progv({ fixupp: nil }):
                        return ast.Subscript(value = w.visit(o.value),
                                              slice = w.visit(o.slice),
                                              ctx = ast.Store() if writep else o.ctx)
        def visit_Attribute(w, o):
                writep = symbol_value(fixupp)
                with _progv({ fixupp: nil }):
                        return ast.Attribute(value = w.visit(o.value),
                                              attr = o.attr,
                                              ctx = ast.Store() if writep else o.ctx)

name_context_fixer = name_context_fixer()

@defmachine
class py(machine):
        def globals(self):
                return globals()
        ## Known level
        def vector_consifier(self, x):
                return ir_cl_call("consify_linear", x)
        def vararg_count(self, vararg_name):
                return ir_funcall("len", vararg_name)
        def vararg_subseq(self, vararg_name, start):
                return list_(_primitive, slice, vararg_name, start, None, None)
        def keyword_binding_checking(self, optless_rest, keys, defaults, must_check_keys = True):
                l, l_ = list_, list__
                ksyms        = [ make_keyword_tn(symbol_name(x)) for x in keys ]
                keyset_gsym  = gensym("KEYSET-") if must_check_keys else nil
                key_map_gsym = gensym_tn("KWHASH-")
                return append(
                        l_(l(key_map_gsym, ir_cl_call("parse_keyword_args", optless_rest)),
                           consify_linear(l(name, l(_if, l(_primitive, not_in, ksym.tn, key_map_gsym.tn),
                                                    def_expr,
                                                    l(_primitive, p.index, key_map_gsym.tn, ksym.tn)))
                                          for name, ksym, def_expr in zip(keys, ksyms, defaults))),
                        (l(l(keyset_gsym, ir_apply("set", ir_cl_call("vectorise_linear", ir_funcall(_list, *ksyms)))),
                           l(gensym("DUMMY-"), ir_cl_call("validate_keyword_args", keyset_gsym, key_map_gsym)))
                         if must_check_keys else nil))
        ## Primitive level
        __supported_primitives__ = {
                string, symbol, literal_list,
                name, integer, float_num,
                add, sub, mul, div, mod, shl, shr, logior, logxor, logand, lognot, floor, expt,
                eq, lt, le, gt, ge,
                not_,
                cons, car, cdr, rplaca, rplacd, vector, index,
                assign, return_, progn, if_, loop, unwind_protect, catch, throw, resignal,
                function, funcall, apply,
                let, let_, progv,
                special_ref, special_setq,

                ## Specific for this backend.
                defun, lambda_expr,
                let_expr, let_thunk, let__setq,
                let__expr, let__stmt,
                progn,
                if_expr, if_stmt,
                attr, const_attr, var_attr,
                slice,
                filtermap, filtermap_expr, filtermap_prim, generator,
                impl_ref, blin_ref,
                import_, delete, global_, nonlocal_, assert_,
                raw, raw_expr, raw_stmt,
                literal_hash_table_expr, pylist, pytuple, pyset,
                neq, and_, or_, equal, nequal, in_, not_in,
                }
        def make_indeterminate_primitive(_, cls, args, keys):
                return determine(cls, args, keys)
        def after_initialise_determinate_primitive(_, x):
                return prim_check_and_spill(x)
        def primitivise_implref(_, x):
                return prim_attr_chain([ p.name(x[1][0][0]) ]
                                       + xmap_to_vector(p.string, x[1][0][1]))
        def lower(self, prim):
                asts = help_prog([prim])
                if symbol_value(cl._compiler_validate_ast_):
                        [ ast_validate(a) for a in asts ]
                with _progv({ fixupp: nil }):
                        fixed_asts = [ name_context_fixer.visit(the(ast.AST, a))
                                       for a in asts ]
                if symbol_value(cl._compiler_trace_ast_):
                        report(fixed_asts, "ast", form_id = id(form), desc = "%LOWER")
                return fixed_asts
        def compilation_unit_prologue(m, funs, syms, gfuns, gvars):
                def wrap_str_or_None(x):
                        return (m.string(x)    if isinstance(x, str) else
                                m.name("None") if x is None          else
                                error("Bad wrap: %s.", x))
                def wrap_bool(x):
                        return m.name("True" if x else "False")
                with _progv(cl.compiler_debugless_traceless_frame):
                         names = sorted(funs | syms, key = lambda x: str(x if isinstance(x, symbol_t) else x[1]))
                         names = [ (cl.pythonised_function_name_symbol(x), x, isinstance(x, tuple)) for x in names]
                         prologue = m.progn(
                                 m.import_("cl"),
                                 m.funcall(
                                         m.const_attr(m.name("cl"), m.string("fop_make_symbols_available")),
                                         m.funcall(m.name("globals")),
                                         m.pylist(*((m.string(package_name(symbol_package(sym))) if symbol_package(sym) else
                                                    m.name("None"))
                                                    for sym, x, _ in names)),
                                         m.pylist(*(m.string(symbol_name(sym))            for sym, x, _     in names)),
                                         m.pylist(*(wrap_bool(setfp)                      for _, __, setfp  in names)),
                                         m.pylist(*(wrap_str_or_None(sym.setf_function_pyname if setfp else
                                                                     sym.function_pyname) for sym, x, setfp in names)),
                                         m.pylist(*(wrap_str_or_None(sym.symbol_pyname)   for sym, x, _     in names)),
                                         m.pylist(*(wrap_bool(x in gfuns)                 for sym, x, _     in names)),
                                         m.pylist(*(wrap_bool(sym in gvars)               for sym, x, _     in names))))
                         # dprintf("prologue:\n%s", pp_consly(prologue))
                         with _progv({ p._valueless_primitive_statement_must_yield_nil_: nil }):
                                 return m.lower(prologue)
