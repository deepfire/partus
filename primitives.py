import cl
from cl import *

import ast
import frost
import collections

### names
def fun_pyname(x): return frost.full_symbol_name_python_name(x)
def sym_pyname(x): return frost.full_symbol_name_python_name(x)
def var_pyname(x): return frost.full_symbol_name_python_name(x)

###
__primitives__           = dict()
__primitives_by_pyname__ = dict()

def find_indet_method_pool(indet_name):
        return __primitives_by_pyname__[indet_name].methods

def defmethod(prim, *tags):
        def do_defmethod(method):
                ## Unregistered Issue METHOD-NEED-DIFFERENTIATION
                primitive_method_pool = find_indet_method_pool(method.__name__)
                for tag in tags: ## HOW DO YOU FUCKING ACCESS THE CLASS OF THE METHOD IT IS BEING DEFINED ON?  NO WAI.
                        primitive_method_pool[tag].add(method)
        return do_defmethod

def identity_method(*keys):
        return (identity, keys) # just a marker, to be processed by defprim

def defstrategy(test = lambda _: True, keys = None):
        assert(keys)
        return (defstrategy, test, keys if isinstance(keys, list) else [keys])

def defprim(name, form_specifier):
        def maybe_process_as_strategy(cls, name, spec):
                def strategyp(x): return (isinstance(x, tuple) and x and x[0] is defstrategy and x[1:]
                                          or (None, None))
                test, keys = strategyp(spec)
                if test and keys:
                        # implementation strategy
                        cls.help_strategies.append((name, test, keys))
                        cls.help_strategies.sort()
                        return t
        def do_defprim(cls):
                __primitives__[name] = __primitives_by_pyname__[cls.__name__] = cls
                class_key_supers = cls.__mro__[0:cls.__mro__.index(prim)]
                cls.form_specifier = form_specifier
                def primitive_add_method_keys(primitive_method_pool, method, keys):
                        for key in keys:
                                primitive_method_pool[key].add(method)
                help_stdmethod = cls.__dict__["help"]
                for name, method_spec in cls.__dict__.items():
                        if maybe_process_as_strategy(cls, name, method_spec) or name == "help":
                                continue
                        ## otherwise, must be an indet method
                        method, identityp, keys = \
                            ((method_spec,    nil, ())             if isinstance(method_spec, types.FunctionType) else
                             (help_stdmethod, t,   method_spec[1]) if (isinstance(method_spec, tuple) and
                                                                       method_spec[0] is identity) else
                             error("Invalid method specifier: %s", method_spec))
                            indet_method_pool = find_indet_method_pool(name)
                            primitive_add_method_keys(indet_method_pool, method, keys)
                            primitive_add_method_keys(indet_method_pool, method, class_key_supers)
                return cls
        return do_defprim

###
### Categories
###
class primclass():
        def __init__(self):
                ## Methods are specific lowering functions, with unspecified, yet implicit applicability.
                self.methods         = collections.defaultdict(set) ## tag -> { method }
                ## An ordered list of explicitly guarded methods.
                self.help_strategies = list()

class prim(metaclass = primclass):
        def __init__(self, *args, **keys):
                self.args = args
                self.keys = keys
                self.spills = []
        def __str__(self):
                return "(%s%s)" % (type(self).__name__.upper(),
                                   "" if not args else (" " + " ".join(str(a) for a in self.args)))
        @classmethod
        def find_method(cls, tags):
                "Find *the* single method matching all tags."
                assert(tags)
                this, *rest = tags
                set = cls.methods[this]  ## The lack of the 'everything' set.  Oh.
                while rest:
                        this, *rest = rest
                        set &= cls.methods[this]
                if not set:
                        raise Exception("Could not find primitive method %s for tags %s.", cls.__name__, tags)
                if len(set) > 1:
                        raise Exception("Ambiguous method specification: primitive %s for tags %s.", cls.__name__, tags)
                return set.pop()

def determine(cls, args, keys):
        for name, test, _ in cls.help_strategies:
                if test(*args, **keys):
                        ## Simplify and compute spills.
                        return (method if not isinstance(method, prim) else
                                cls.find_method(keys))(*args, **keys)
        else:
                raise Exception("Unhandled primitive form: %s" % self)

class indet(prim):
        """Those have context-sensitivity, provided by a choice of strategies.
           The indeterminacy is short-lived, though."""
        def __new__(cls, *args, **keys):
                return determine(cls, args, keys)

class det(prim):
        "Those are spill-determinate, post-init."
        def __init__(self, *args, **keys):
                prim.__init__(self, *args, **keys)
                prim_check_and_spill(self)

class stmt(det):         pass ## lower to ([ast.stmt], ast.expr)
class body(stmt):        pass ## lower to ([ast.stmt], ast.expr), and has an embedded body
class name_setter(stmt): pass

class expr(det):         pass ## lower to an ast.expr
class expr_spiil(expr):  pass
class potefless(expr):   pass ## might have no side effect
class efless(potefless): pass ## on side effect
class potconst(efless):  pass ## might end up being a constant expression
class const(potconst):   pass ## constant
class literal(const):
        def value(self):
                return self.args[0]

## to consider: no-return

###
### Toolkit
###
def prim_kind_det_p(x):         return issubclass(x, (expr, stmt))
def prim_unspilled_expr_p(x):   return isinstance(x, expr) and not x.spills
def suite_unspilled_expr_p(xs): return len(xs) is 1 and prim_unspilled_expr_p(xs[0])

def ast_to_expr(x):
        return (x.value if isinstance(x, ast.Expr) else
                x       if isinstance(x, ast.expr) else
                error("%s cannot be coerced to an expression." % x))

def ast_to_Expr(x):
        return (ast.Expr(x) if isinstance(x, ast.expr) else
                x           if isinstance(x, ast.Expr) else
                error("%s cannot be coerced to an expression." % x))

def coerce_to_stmt(x):
        return (x if isinstance(x, ast.stmt) else
                ast.Expr(x))

TheEmptyList = list()

def help(x):
        r = x.help(*x.args, **x.keys)
        p, v = (r if isinstance(r, tuple) else
                ([], r))
        return x.spills + p or TheEmptyList, v

def help_expr(x):
        p, v = help(x)
        p and error("Helped %s to non-expr %s, %s, where an expression result was expected.", x, p, v)
        return v

def help_exprs(xs):
        return [ help_expr(x) for x in xs ]

def help_prog(xs):
        p_acc = []
        for x in xs:
                p, v = help(x)
                p_acc.extend(p)
                p_acc.append(ast.Expr(v))
        return p_acc

def help_prog_n(xs, vf):
        p_acc = help_prog(xs)
        p, v = help(vf)
        p_acc.extend(p)
        return p_acc, v

def help_progn(xs):
        assert(xs)
        return help_prog_n(xs[:-1], xs[-1])

def help_progn_star(*xs):
        return help_progn(xs)

def help_tail_prog(xs, kind):
        p, v = help_progn(xs)
        p.append(kind(value = value))
        return p

def help_args(fixed, opts, optvals, args, keys, keyvals, restkey):
        assert(len(opts) == len(optvals) and
               len(keys) == len(keyvals))
        return ast.arguments(
                args             = [ ast.arg(var_pyname(x), None) for x in fixed + optnames ],
                vararg           = var_pyname(args)    if args    else None,
                varargannotation = None,
                kwonlyargs       = [ ast.arg(var_pyname(x), None) for x in         keynames ],
                kwarg            = var_pyname(restkey) if restkey else None,
                kwargannotation  = None,
                defaults         = help_exprs(optvals),
                kw_defaults      = help_exprs(keyvals))

def help_ctx(writep):
        return (ast.Store if writep else ast.Load)()

def fixed_ll(names):            return (names, [], [], None, [], [], None)
def fixed_rest_ll(names, rest): return (names, [], [], rest, [], [], None)

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
### Only applicatively positioned subforms can be conveniently spilled.
###
class primitive_mismatch(Exception):
        def __init__(self, spec = None, form = None):
                assert(spec and form)
                self.spec, self.form = spec, form
        def __str__(self):
                return "Primitive mismatch: (%s %s) is not of type %s." % \
                    (type(self.form).__name__, " ".join(repr(x) for x in self.form.args), self.spec)

def prim_check_and_spill(primitive) -> primitive, list(dict()):
        def check_prim_type(arg, type):
                if not typep(arg, type):
                        raise primitive_mismatch(spec = type, form = arg)
        ###
        def tuple_spills(spec, args):
                segmentp = spec and isinstance(spec[-1], list)
                nspec, nargs = len(spec), len(args)
                assert((segmentp and nargs >= (nspec - 1))
                       or nspec is nargs)
                a_fixed, a_segment, s_spec = ((args[:nspec - 1], args[nspec - 1:], spec[-1][0]) if segmentp else
                                              (args,             [],               None))
                def expr_tuple_spill_partition(spec, args):
                        pre_spills = []
                        for s, a in zip(spec, a_fixed):
                                pre_spills.append(process(s, a))
                        for a in a_segment:
                                pre_spills.append(process(s_spec, a))
                        ## only allowable spills will land here
                        last_spilled_posn = position_if(identity, pre_spills, from_end = t, key = _indexing(1))
                        first_unspilled_posn = last_spilled_posn + 1
                        n_spilled = len(pre_spills) - first_unspilled_posn
                        n_segspill = max(0, first_unspilled_posn - len(a_fixed))
                        for_spill, unspilled = args[:n_spilled], args[n_spilled:]
                        return for_spill, ((spec[:-1] + (s_spec,) * n_segspill)
                                           if n_segspill else
                                           spec[:first_unspilled_posn]), unspilled
                for_spill_as, for_spill_ss, unspilled = tuple_spill_partition(spec, args)
                ## Re-collecting spills, while forcing spill for unspilled spillables.
                forms, spills = [], []
                for s, a in zip(for_spill_ss, for_spill_as):
                        form, spill = process(s, a, force_spill = t)
                        forms += (form,)
                        spills.append(spill)
                return (spills,
                        forms + unspilled)
        def options_spills(spec, arg, force_spill = nil):
                for option in spec:
                        try:
                                return process(option, arg, force_spill = force_spill)
                        except primitive_mismatch as _:
                                pass
                raise primitive_mismatch(spec = spec, form = arg)
        def map_spills(spec, arg, force_spill = nil):
                for result, option in spec:
                        if typep(arg, option):
                                return process(result, arg, force_spill = force_spill)
                raise primitive_mismatch(spec = spec, form = arg)
        def type_check(spec, arg, force_spill = nil):
                check_prim_type(arg, type)
                return []
        processors = { tuple: tuple_spills,
                       list:  lambda misspec, *_, **__: error("List type specifier (%s), outside of appropriate context.",
                                                              misspec),
                       set:   options_spills,
                       dict:  map_spills }
        def process(spec, arg, force_spill = nil):
                if spec is expr_spillable:
                        check_prim_type(arg, (or_t, expr, stmt))
                        ## Spill, iff any of the conditions hold:
                        if not (force_spill           or # - spilling is required
                                isinstance(arg, stmt) or # - the argument is not an expression
                                arg.spills):             # - the argument has spilled itself
                                return (arg, [])
                        tn = gensym("EXP-") ## Temporary Name.
                        return (arg.spills + [lexical_setq(tn, arg)],
                                lexical_ref(tn))
                return processors.get(type(spec), type_check)(spec, arg, force_spill = force_spill)
        primitive.args, primitive.spills = tuple_spills(primitive, args)
        return primitive

def process(p):
        ...

###
### (current) (not so very) grand scheme of things
###
## 1. Init-time:
##  - calculation of spills for determinates
##  - indeterminates dispatch to strategies
## 2. Help-time
##  - lowering
## ... ?

### Core TODO
##
# ? M-V-CALL
# ? M-V-PROG1
# ? NTH-VALUE
# ?? TAGBODY, GO
# Later: PROGV
##

## Registry:
## - STRING, INTEGER, FLOAT-NUM, SYMBOL, LITERAL-LIST, LITERAL-HASH-TABLE-EXPR
## - QUOTE
## - FUNCTION,
## - LAMBDA, DEFUN, LAMBDA-EXPR
## - LET, LET-EXPR, LET-TAIL, LET-THUNK
## - SPECIAL-LET
## - FLET, FLET-EXPR, FLET-STMT
## - LABELS
## - PROGN
## - IF, IF-EXPR, IF-STMT
## - FUNCALL, APPLY
## - UNWIND-PROTECT
## - RESIGNAL
## - LEXICAL-{REF,SETQ}, SPECIAL-{REF,SETQ}
## - IMPL-REF
## - ATTR-REF, CONST-ATTR-REF, VAR-ATTR-REF
## - EQ
## - ADD

###
### Constants
###
@defprim(string,
         (str,))
class string(literal):
        def help(name): return ast.Str(x)

@defprim(integer,
         (int,))
class integer(literal):
        def help(x): return ast.Num(x)

@defprim(intern("FLOAT-NUM")[0],
         (float,))
class float_num(literal):
        def help(x): return ast.Num(x)

@defprim(symbol,
         (symbol_t,))
class symbol(literal):
        def help(name): return ast.Name(sym_pyname(name), ast.Load())

@defprim(intern("LITERAL-LIST"),
         ([literal],))
class listeral_list(literal):
        def help(*xs):
                return reduce(lambda car, cdr: ast.List(help_expr(car), cdr),
                              reversed(xs),
                              ast.Name(sym_pyname(nil), ast.Load()))

@defprim(intern("LITERAL-HASH-TABLE-EXPR"),
         ([(expr_spill, expr_spill)],))
## Unregistered Issue EXTREME-NICETY-OF-AUTOMATIC-RECLASSIFICATION-TO-A-NARROWER-TYPE
class literal_hash_table_expr(expr):
        def help(*kvs):
                keys, vals = zip(*kvs)
                return ast.Dict(help_exprs(keys), help_exprs(vals))

@defprim(intern("QUOTE")[0],
         (const,))
class quote(const):
        def help(x):
                return help(x)

###
### Functions
###
@defprim(function,
         (or_t, symbol_t, ((eql_t, cl.setq), symbol_t)))
class function(efless):
        def help(name): return ast.Name(fun_pyname(name), ast.Load())

@defprim(lambda_,
         ((([symbol_t],),
          ([symbol_t],), ([prim],), symbol_t,
          ([symbol_t],), ([prim],), symbol_t),
          [prim]))
class lambda_(indet):
        "NOTE: default value form evaluation is not delayed."
        1_expr = defstrategy(test = lambda pyargs, *body: suite_unspilled_expr_p(body),
                             keys = expr)
        2_stmt = defstrategy(keys = body)

@defprim(defun,
         (symbol_t, (([symbol_t],),
                     ([symbol_t],), ([expr],), symbol_t,
                     ([symbol_t],), ([expr],), symbol_t),
          [prim]))
class defun(body):
        def help(name, pyargs, decorators, *body):
                return [ ast.FunctionDef(
                                fun_pyname(name),
                                help_args(*pyargs),
                                decorator_list = help_exprs(decorators),
                                returns = None,
                                body = help_tail_prog(body, kind = ast.Return))
                         ], help_expr(function(name))
        @defmethod(lambda_)
        def lambda_(pyargs, expr):
                return defun(gensym("DEFLAM-"), pyargs, [], expr)

@defprim(intern("LAMBDA-EXPR")[0],
         ((([symbol_t],),
           ([symbol_t],), ([expr_spill],), symbol_t,
           ([symbol_t],), ([expr_spill],), symbol_t),
          expr))
class lambda_expr(expr):
        def help(pyargs, expr): return ast.Lambda(help_args(*pyargs), help_expr(expr))
        lambda_ = identity_method()

###
### Binding
###
@defprim(let,
         (([(symbol_t, prim)],),
          [prim]))
class let(indet):
        1_expr = defstrategy(test = lambda bindings, *body: suite_unspilled_expr_p(body),
                             keys = expr)
        2_stmt = defstrategy(keys = body)

@defprim(intern("LET-EXPR"),
         (([(symbol_t, expr_spill)],),
          expr))
class let_expr(expr):
        def help(bindings, expr):
                ns, vs = list(zip(*bindings))
                return help(funcall(lambda_expr(fixed_ll(ns),
                                                expr),
                                    *vs))
        let = identity_method()

@defprim(intern("LET-TAIL"),
         (([(symbol_t, expr_spill)],),)
         [prim])
class let_tail(body):
        "Can only be used, when it can be proved, that no used variables can be mutated."
        ## NOT LINKED UP -- deferred for usage by higher levels,
        ## as there is not enough information to perform indet-init-time selection.
        def help(bindings, *body):
                return help(progn(*([ lexical_setq(n, v) for n, v in bindings ]
                                    + body)))

@defprim(intern("LET-THUNK"),
         (([(symbol_t, expr_spill)],),
          [prim]))
class let_thunk(body):
        "The most universal, yet bulky kind of LET."
        def help(bindings, *body):
                ns, vs = list(zip(*bindings))
                tn = gensym("LET-THUNK-")
                return help(progn(defun(tn, fixed_ll(ns), [],
                                        *body),
                                  funcall(function(tn), *vs)))
        let = identity_method()

@defprim(intern("SPECIAL-LET"),
         (([(symbol_t, expr_spill)],),
          [prim]))
class special_let(body):
        def help(bindings, *body):
                tn = gensym("VALUE-")
                return [ ast.With(funcall(impl_ref("_env_cluster"),
                                          literal_hash_table_expr(*((symbol(name), val)
                                                                    for name, val in bindings.items()))),
                                  None,
                                  help_prog_star(lexical_setq(tn, progn(*body))))
                         ], help_exp(lexical_ref(tn))
                return help(progn(defun(tn, fixed_ll(ns), [],
                                        *body),
                                  funcall(function(tn), *vs)))

@defprim(flet,
         (([(symbol_t, (([symbol_t],),
                        ([symbol_t],), ([expr_spill],), symbol_t,
                        ([symbol_t],), ([expr_spill],), symbol_t),
             [prim])],),
          [prim]))
class flet(indet):
        1_expr = defstrategy(test = lambda bindings, *body: (suite_unspilled_expr_p(body) and
                                                             all(suite_unspilled_expr_p(body)
                                                                 for name, lam, body in bindings)),
                             keys = expr)
        2_stmt = defstrategy(keys = body)

@defprim(intern("FLET-EXPR"),
         (([(symbol_t, (([symbol_t],),
                        ([symbol_t],), ([expr_spill],), symbol_t,  ## EXPR-SPILL?
                        ([symbol_t],), ([expr_spill],), symbol_t),
             expr)],),
          expr))
class flet_expr(body):
        def help(bindings, expr):
                ns, lls, bs = zip(*bindings)
                return help(funcall(lambda_expr(fixed_ll(ns),
                                                expr),
                                    *[ lambda_expr(lam, expr)
                                       for _, lam, expr in bindings ]))
        flet = identity_method()

@defprim(intern("FLET-STMT"),
         (([(symbol_t, (([symbol_t],),
                        ([symbol_t],), ([expr_spill],), symbol_t,  ## EXPR-SPILL?
                        ([symbol_t],), ([expr_spill],), symbol_t),
             [prim])],),
          [prim]))
class flet_stmt(body):
        def help(bindings, *body):
                gennames = [ gensym(symbol_name(name)) for name, _, __ in bindings ]
                names, lams, bodies = zip(*bindings)
                tn = gensym("FLET-THUNK-")
                return (help_prog([defun(tn, fixed_ll([]),
                                         *([ defun(name, args, [],
                                                   *body)
                                             for name, args, *body in bindings ]
                                           + body))]),
                        help(funcall(function(tn))))
        flet = identity_method()

@defprim(intern("LABELS")[0],
         (([(symbol_t, (([symbol_t],),
                        ([symbol_t],), ([expr_spill],), symbol_t,  ## EXPR-SPILL?
                        ([symbol_t],), ([expr_spill],), symbol_t),
             [prim])],),
          [prim]))
class labels(body):
        def help(bindings, *body):
                tn = gensym("LABELS-THUNK-")
                return (help_prog([defun(tn, fixed_ll([]),
                                         *([ defun(name, args, [],
                                                   *body)
                                             for name, args, *body in bindings ]
                                           + body))]),
                        help(funcall(function(tn))))

###
### Control
###
@defprim(intern("PROGN")[0],
         ([prim],))
class progn(indet):
        1_expr = defstrategy(test = lambda *body: suite_unspilled_expr_p(body),
                             keys = help_exp)
        2_stmt = defstrategy(keys = help_progn_star)

@defprim(if_,
         (prim, prim, prim))
class if_(indet):
        1_expr = defstrategy(test = lambda _, conseq, ante: (prim_unspilled_expr_p(conseq) and
                                                             prim_unspilled_expr_p(ante)),
                             keys = expr)
        2_stmt = defstrategy(keys = body)

@defprim(intern("IF-EXPR")[0],
         (expr_spill, expr, expr))
class if_expr(expr):
        def help(*tca):
                return ast.IfExpr(*help(x for x in tca)) # Test, Consequent, Antecedent.
        if_ = identity_method()

@defprim(intern("IF-STMT")[0],
         (expr_spill, prim, prim))
class if_stmt(body):
        def help(*tca):
                tv, (cp, cv), (ap, av) = [ help(x) for x in tca ]
                tn = gensym("IFVAL-")
                return [ ast.If(tv,
                                cp + help_prog([lexical_setq(tn, cv)]),
                                ap + help_prog([lexical_setq(tn, av)])
                                ) ], help(lexical_ref(tn))
        if_ = identity_method()

@defprim(funcall,
         (expr_spill, [expr_spill]))
class funcall(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, *fixed_args):
                return ast.Call(help_expr(func),
                                help_exprs(fixed_args), [])

@defprim(apply,
         (expr_spill, expr_spill, [expr_spill]))
class apply(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, arg, *args):
                fixed_args, arglist = (([arg] + args[:-1], args[-1]) if args else
                                       ([],                [arg]))
                return ast.Call(help_expr(func), help_exprs(fixed_args), [], help_expr(arglist))

@defprim(unwind_protect,
         (prim,
          [prim]))
class unwind_protect(body):
        def help(protected_form, *body):
                # need a combinator for PRIM forms
                tn = gensym("UWP-VALUE-")
                return ast.TryFinally(help_prog([lexical_setq(tn, protected_form)]),
                                      help_prog(body)
                                      ), help_expr(lexical_ref(tn))

@defprim(resignal, ())
class resignal(stmt):
        def help():
                return ast.Raise()

@defprim(catch,
         (expr_spill,
          [prim]))
class catch(tag, *body):
        def help(tag, *body):
                val_tn, ex_tn = gensym("BODY-VALUE-"), gensym("EX")
                return [ ast.TryExcept(
                                help_prog([lexical_setq(val_tn, progn(*body))]),
                                [ ast.ExceptHandler(impl_ref("__catcher_throw__"),
                                                    var_pyname(ex_tn),
                                                    help_prog_star(
                                                        if_(is_(attr(lexical_ref(ex_tn), string("ball")),
                                                                help_expr(tag)),
                                                            progn(funcall(ref_impl("__catch_maybe_reenable_pytracer"),
                                                                          lexical_ref(ex_tn)),
                                                                  lexical_setq(val_tn,
                                                                               attr(lexical_ref(ex_tn), string("value")))),
                                                            resignal()))) ],
                                [])
                         ], help_expr(lexical_ref(val_tn))

@defprim(throw,
         (expr_spill, expr_spill))
class throw(expr):
        def help(tag, value):
                return help_expr(funcall(impl_ref("__throw"),
                                         help_expr(tag), help_expr(value)))

###
### References
###
@defprim(intern("LEXICAL-REF")[0],
         (symbol_t,))
class lexical_ref(efless): ## potconst, if we choose to do constant propagation this way
        def help(name, writep = nil):
                return ast.Name(var_pyname(name), (ast.Store if writep else ast.Load)())

@defprim(intern("LEXICAL-SETQ")[0],
         (symbol_t, expr_spill))
class lexical_setq(stmt):
        def help(name, value):
                return help_progn_star(assign(lexical_ref(name, writep = t), value, tn = nil))

@defprim(intern("SPECIAL-REF")[0],
         (symbol_t,))
class special_ref(efless):
        def help(name):
                return help(funcall(impl_ref("_symbol_value"), symbol(name)))

@defprim(intern("SPECIAL-SETQ")[0],
         (symbol_t, expr_spill))
class special_setq(expr):
        def help(name, value):
                return help(funcall(impl_ref("_do_set"), symbol(name), value))

@defprim(intern("IMPL-REF")[0],
         (string,))
class impl_ref(expr):
        def help(name):
                return _ast_attribute_chain("cl", name.value())

###
### Specials
###
@defprim(intern("ATTR-REF")[0],
         (prim, prim))
class attr(indet):
        1_const = defstrategy(test = lambda _, attr: isinstance(attr, string),
                              keys = "const attr")
        2_var   = defstrategy(keys = efless)

@defprim(intern("CONST-ATTR-REF")[0],
         (expr_spill, string))
class const_attr(efless):
        def help(x, attr, writep = nil): return ast.Attribute(help_expr(x), help_expr(attr), help_ctx(writep))
        attr = identity_method("const attr")

@defprim(intern("VAR-ATTR-REF")[0],
         (expr_spill, expr_spill))
class var_attr(efless):
        def help(x, attr):
                return help(funcall(ast.Name("getattr", ast.Load()),
                                    help_expr(x), help_expr(attr)))
        attr = identity_method()

@defprim(intern("INDEX")[0],
         (expr_spill, expr_spill))
class index(expr):
        def help(x, index, writep = nil): return ast.Subscript(help_expr(x), ast.Index(help_expr(index)), help_ctx(writep))

@defprim(intern("ASSIGN")[0],
         (expr, expr_spill))
class assign(stmt):
        def help(place, value):
                tn = gensym("TARGET-")
                return [ help(lexical_setq(tn, value))[0],
                         ast.Assign([ help_expr(place) ], help_expr(lexical_ref(tn)))
                         ], lexical_ref(tn)

###
### Lists
###
@defprim(intern("CONS")[0], (expr_spill, expr_spill))
class cons(expr):
        def help(car, cdr): return ast.List([ help_expr(car),
                                              help_expr(cdr) ])

@defprim(intern("CAR")[0], (expr_spill,))
class car(expr):
        def help(cons): return _ast.Subscript(help_expr(cons), ast.Index(0), ast.Load())

@defprim(intern("CDR")[0], (expr_spill,))
class cdr(expr):
        def help(cons): return _ast.Subscript(help_expr(cons), ast.Index(1), ast.Load())

@defprim(intern("RPLACA")[0], (expr_spill,))
class rplaca(expr):
        def help(cons, value):
                return help_progn_star(assign(index(lexical_ref(tn, writep = t), integer(0), writep = t),
                                              value, tn = gensym("CAR-")))

@defprim(intern("RPLACD")[0], (expr_spill,))
class rplacd(expr):
        def help(cons, value):
                return help_progn_star(assign(index(lexical_ref(tn, writep = t), integer(1), writep = t),
                                              value, tn = gensym("CDR-")))

###
### Operations
###
def help_binop(op, x, y):
        return ast.BinOp(help_expr(x), op(), help_expr(y))

@defprim(intern("EQ")[0], (expr_spill, expr_spill))
class eq(potconst):
        ## Optimisation: fold (NOT (EQ X Y)) to ast.IsNot
        def help(x, y): return help_binop(ast.Is, x, y)

def help_binop_seq(args, type):
        init, rest = ((args[0], args[1:]) if args else (0, args))
        return reduce(lambda x, y: ast.BinOp(x, type(), help_expr(y)),
                      rest, help_expr(init))

@defprim(intern("+")[0], ([expr_spill],))
class add(potconst):
        def help(*xs): return help_binop_seq(xs, ast.Add)
