import cl
from cl import *
from cl import _gensymname as gensymname
from cl import _ensure_symbol_pyname as ensure_symbol_pyname
from cl import _indexing as indexing
from cl import _sex_space as sex_space

import ast
import frost
import types
import collections

from more_ast import pp_ast_as_code

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

def defprim(name, form_specifier):
        def maybe_process_as_strategy(cls, name, spec):
                def strategyp(x): return (isinstance(x, tuple) and x and x[0] is defstrategy and x[1:]
                                          #         See defstrategy above, for the definition of x[1:]
                                          or (None, None))
                # cl._debug_printf("trying to process %s as strategy in %s", spec, cls)
                test, xform_or_keys = strategyp(spec)
                # Due to the definition of defstrategy, test cannot be a false-equivalent.
                if test and xform_or_keys:
                        # implementation strategy
                        cls.help_strategies.append((name, test, xform_or_keys))
                        cls.help_strategies.sort()
                        return t
        def do_defprim(cls):
                __primitives__[name] = __primitives_by_pyname__[cls.__name__] = cls
                class_key_supers = cls.__mro__[0:cls.__mro__.index(prim)]
                cls.form_specifier = form_specifier
                def primitive_add_method_keys(primitive_method_pool, method, keys):
                        for key in keys:
                                primitive_method_pool[key].add(method)
                help_stdmethod = cls.__dict__.get("help", None)
                for n, method_spec in cls.__dict__.items():
                        if (n in ("__module__", "__locals__", "__doc__",
                                  "methods", "help_strategies", "form_specifier", "help", "value") or
                            maybe_process_as_strategy(cls, n, method_spec)):
                                if n is "help":
                                        # decorating all the stuff into staticmethod in-text would be a real shame
                                        cls.help = staticmethod(method_spec)
                                continue
                        ## otherwise, must be an indet method
                        method, identityp, keys = \
                            ((method_spec,    nil, ())             if isinstance(method_spec, types.FunctionType) else
                             (cls, t,   method_spec[1]) if (isinstance(method_spec, tuple) and
                                                                       method_spec[0] is identity) else
                             error("Invalid method specifier: %s", method_spec))
                        indet_method_pool = find_indet_method_pool(n)
                        primitive_add_method_keys(indet_method_pool, method, keys)
                        primitive_add_method_keys(indet_method_pool, method, class_key_supers)
                return cls
        return do_defprim

###
### Categories
###
class primclass(type):
        def __init__(self, name, cpl, dict):
                ## Methods are specific lowering functions, with unspecified, yet implicit applicability.
                self.methods         = collections.defaultdict(set) ## tag -> { method }
                ## An ordered list of explicitly guarded methods.
                self.help_strategies = list()

def print_primitive(x):
        return ('"%s"' % x                                             if     isinstance(x, str)      else
                "'%s"  % x                                             if     isinstance(x, symbol_t) else
                "(%s)" % " ".join(print_primitive(ix) for ix in x)     if     isinstance(x, tuple)    else
                " [ %s ] " % " ".join(print_primitive(ix) for ix in x) if     isinstance(x, list)     else
                str(x)                                                 if not isinstance(x, prim)     else
                ("(%s%s%s)" % (type(x).__name__.upper(),
                               " " if x.args else "",
                               " ".join(print_primitive(ix) for ix in x.args))))

class prim(metaclass = primclass):
        def __init__(self, *args, **keys):
                self.args = args
                # cl._here("  making a %s", self, offset = 2, callers = 9)
                self.keys = keys
                self.spills = []
        def __str__(self):
                return print_primitive(self)
        @classmethod
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

def determine(cls, args, keys):
        for name, test, xform_or_keys in cls.help_strategies:
                if test(*args, **keys):
                        ## Simplify and compute spills.
                        xf = (xform_or_keys if not isinstance(xform_or_keys, list) else
                                cls.find_method(xform_or_keys))
                        # if cls is progn:
                        #         # cl._debug_printf("PROGN-DET args %s", args)
                        # cl._debug_printf("xf %s", xf)
                        return xf(*args, **keys)
        else:
                error("Unhandled primitive form: %s", self)

def redetermining_as(cls, **keys):
        return lambda x: determine(cls, x.args, keys)

class indet(prim):
        """Those have context-sensitivity, provided by a choice of strategies.
           The indeterminacy is short-lived, though."""
        def __new__(cls, *args, **keys):
                return determine(cls, args, keys)

class det(prim):
        "Those are spill-determinate, post-init."
        def __init__(self, *args, **keys):
                prim.__init__(self, *args, **keys)
                # cl._debug_printf("making  %s", self)
                prim_check_and_spill(self)
                assert isinstance(self.spills, list)

class stmt(det):               pass ## lower to ([ast.stmt], ast.expr)
class body(stmt):              pass ## lower to ([ast.stmt], ast.expr), and has an embedded body
class name_setter(stmt):       pass

class expr(det):               pass ## lower to an ast.expr
class expr_spill(expr):        pass
class maybe_expr_spill(expr):  pass
class potefless(expr):         pass ## might have no side effect
class efless(potefless):       pass ## on side effect
class potconst(efless):        pass ## might end up being a constant expression
class const(potconst):         pass ## constant
class literal(const):
        def value(self):
                return self.args[0]

class maybe(): pass

## to consider: no-return

###
### Toolkit
###
def unspilled_expr_p(x):        return isinstance(x, expr) and not x.spills
def suite_unspilled_expr_p(xs): return len(xs) is 1 and unspilled_expr_p(xs[0])

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
_compiler_trace_primitives_ = cl._compiler_trace_primitives_

def help(x) -> ([stmt], expr):
        if not isinstance(x, prim):
                error("A non-primitive leaked to the HELP phase: %s", x)
        with cl.progv({ cl._pp_base_depth_: cl._pp_base_depth() + 3 }):
                r = x.help(*x.args, **x.keys)
        p, v = (([], r) if isinstance(r, ast.expr)                         else
                ## list(r) if isinstance(r, tuple) else
                ## Unregistered Issue SLOW-CHECK
                r if typep(r, (pytuple_t, (pylist_t, ast.stmt), ast.expr)) else
                error("Invalid output from lowerer for %s -- %s.", x, r))
        spills = help_prog(x.spills)
        if not isinstance(x, name) and symbol_value(_compiler_trace_primitives_):
                ssp = sex_space()
                cl._debug_printf("%s---- helpery %s --->\n%s%s%s\n%s%s\n%s",
                                 ssp, cl._pp_chain_of_frame(cl._caller_frame(), callers = 15),
                                 ("%s%s\n%s- spielleren ^v form -\n" %
                                  (ssp, ("\n" + ssp).join(str(x) for x in x.spills), ssp)) if x.spills else "",
                                 ssp, x,
                                 ssp, ssp, ("\n" + ssp).join(pp_ast_as_code(x) for x in spills + p + [v]))
        return spills + p or TheEmptyList, v

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
                p_acc.append(ast.Expr(v))
        return p_acc

def help_prog_n(xs, vf) -> ([stmt], expr):
        p_acc = help_prog(xs)
        p, v = help(vf)
        p_acc.extend(p)
        return p_acc, v

def help_progn(xs) -> ([stmt], expr):
        assert(xs)
        return help_prog_n(xs[:-1], xs[-1])

def help_progn_star(*xs) -> ([stmt], expr):
        return help_progn(xs)

def help_tail_prog(xs, kind):
        p, v = help_progn(the((pyseq_t, prim), xs))
        p.append(kind(value = v))
        return p

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
        return help_expr(name(cl._unit_symbol_pyname(nil)))

def          fixed_ll(fixed):                    return (fixed, [],  [],     None, [], [], None)
def      fixed_opt_ll(fixed, opt, optval):       return (fixed, opt, optval, None, [], [], None)
def     fixed_rest_ll(fixed, rest):              return (fixed, [],  [],     rest, [], [], None)
def fixed_opt_rest_ll(fixed, opt, optval, rest): return (fixed, opt, optval, rest, [], [], None)

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
class primitive_mismatch(error_t):
        def __init__(self, mesg, prim = None, pspec = None, spec = None, form = None):
                ni = "#<not initialised>"
                self.prim, self.pspec, self.spec, self.form = ni, ni, ni, ni
                assert(prim and pspec and spec)
                self.mesg, self.prim, self.pspec, self.spec, self.form = mesg, prim, pspec, spec, form
        def __str__(self):
                return "While matching primitive %s against its argspec %s, mismatch of %s with spec %s: %s." % \
                    (self.prim, self.pspec,
                     print_primitive(self.form),
                     self.spec,
                     self.mesg)

def prim_check_and_spill(primitive) -> (prim, list(dict())):
        def check_prim_type(arg, type):
                if not typep(arg, type):
                        raise primitive_mismatch("type mismatch",
                                                 prim = primitive, pspec = primitive.form_specifier,
                                                 spec = type, form = arg)
        ###
        def tuple_spills(spec, args, force_spill = nil):
                # if isinstance(primitive, defun):
                #         cl._debug_printf("DEFUN args: %s", primitive.args)
                specialp = spec and spec[0] in [maybe]
                if specialp:
                        if spec[0] is maybe:
                                if args is None:
                                        return None, []
                                return process(spec[1], args, force_spill = force_spill)
                check_prim_type(args, (or_t, tuple, list))
                segmentp = spec and isinstance(spec[-1], list)
                nspec, nargs = len(spec), len(args)
                if not (segmentp and nargs >= (nspec - 1)
                        or nspec is nargs):
                        error("Invalid primitive %s: subform %s has %d elements, but %s was/were expected.",
                              primitive, args, nargs,
                              ("at least %s" % (nspec - 1)) if segmentp else
                              ("exactly %d" % nspec))
                a_fixed, a_segment, s_spec = ((args[:nspec - 1], args[nspec - 1:], spec[-1][0]) if segmentp else
                                              (args,             [],               None))
                # isinstance(primitive, defun) and cl._debug_printf("tuple %s, spec %s,  af %s, as %s",
                #                                                   args, spec, a_fixed, a_segment)
                def expr_tuple_spill_partition(spec, args):
                        pre_spills = []
                        for s, a in zip(spec, a_fixed):
                                pre_spills.append(process(s, a, force_spill = force_spill))
                        for a in a_segment:
                                pre_spills.append(process(s_spec, a, force_spill = force_spill))
                        # isinstance(primitive, defun) and cl._debug_printf("presp %s",
                        #                                                   pre_spills)
                        ## only allowable spills will land here
                        # cl._debug_printf("pre-spills of %s: %s", primitive, pre_spills)
                        last_spilled_posn = position_if(identity, pre_spills, from_end = t, key = indexing(1))
                        n_spilled = (last_spilled_posn + 1 if last_spilled_posn is not None else
                                     0)
                        for_spill, unspilled = args[:n_spilled], args[n_spilled:]
                        n_segspill = max(0, n_spilled - len(a_fixed))
                        return for_spill, ((spec[:-1] + (s_spec,) * n_segspill)
                                           if n_segspill else
                                           spec[:n_spilled]), unspilled
                for_spill_as, for_spill_ss, unspilled = expr_tuple_spill_partition(spec, args)
                ## Re-collecting spills, while forcing spill for unspilled spillables.
                forms, spills = [], []
                for s, a in zip(for_spill_ss, for_spill_as):
                        form, spills = process(s, a, force_spill = t)
                        forms += (form,)
                        spills.extend(spills)
                return (tuple(forms) + tuple(unspilled),
                        spills)
        def options_spills(spec, arg, force_spill = nil):
                for option in spec:
                        try:
                                return process(option, arg, force_spill = force_spill)
                        except primitive_mismatch as _:
                                pass
                raise primitive_mismatch("no option matched",
                                         prim = primitive, pspec = primitive.form_specifier, spec = spec, form = arg)
        def map_spills(spec, arg, force_spill = nil):
                for result, option in spec:
                        if typep(arg, option):
                                return process(result, arg, force_spill = force_spill)
                raise primitive_mismatch("no option matched",
                                         prim = primitive, pspec = primitive.form_specifier, spec = spec, form = arg)
        def type_check(spec, arg, force_spill = nil):
                check_prim_type(arg, spec)
                return (arg,
                        [])
        processors = { tuple: tuple_spills,
                       list:  lambda misspec, *_, **__: error("List type specifier (%s), outside of appropriate context.",
                                                              misspec),
                       set:   options_spills,
                       dict:  map_spills }
        def process(spec, arg, force_spill = nil):
                if spec in (expr_spill, maybe_expr_spill):
                        maybe = spec is maybe_expr_spill
                        check_prim_type(arg, ((or_t, (eql_t, nil), expr, stmt) if maybe else
                                              (or_t, expr, stmt)))
                        ## Spill, iff any of the conditions hold:
                        if (maybe and arg is nil or       # - no spilling for an optional, non-provided primitive argument
                            not (force_spill           or # - spilling is required
                                 isinstance(arg, stmt) or # - the argument is not an expression
                                 arg.spills)):            # - the argument has spilled itself
                                return (arg,
                                        [])
                        tn = genname("EXP-") ## Temporary Name.
                        return (tn,
                                arg.spills + [ assign(tn, arg) ])
                return processors.get(type(spec), type_check)(spec, arg, force_spill = force_spill)
        primitive.args, primitive.spills = tuple_spills(primitive.form_specifier, primitive.args)
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
## - NAME
## - ASSIGN
## - ATTR, CONST-ATTR, VAR-ATTR
## - INDEX, SLICE, PYLIST
## - STRING, INTEGER, FLOAT-NUM, LITERAL-LIST, LITERAL-HASH-TABLE-EXPR
## - LAMBDA, DEFUN, LAMBDA-EXPR
## - LET, LET-EXPR, LET-THUNK
## - LET*, LET*-SETQ, LET*-EXPR, LET*-STMT
## - PROGV
## - FLET, FLET-EXPR, FLET-STMT
## - LABELS
## - PROGN
## - IF, IF-EXPR, IF-STMT
## - FUNCALL, APPLY
## - UNWIND-PROTECT
## - LOOP
## - RESIGNAL
## - SPECIAL-{REF,SETQ}
## - IMPL-REF, BUILTIN-REF
## - CONS, CAR, CDR, RPLACA, RPLACD
## - AND, OR
## - +, -, *, /, MOD, POW, <<, >>, LOGIOR, LOGXOR, LOGAND, FLOOR
## - NOT, LOTNOT
## - EQ, NEQ, EQUAL, NOT-EQUAL, <, <=, >, >=, IN, NOT-IN

###
### Spycials
###
@defprim(intern("STRING")[0],
         (string_t,))
class string(literal):
        def help(name): return ast.Str(name)

@defprim(intern("NAME")[0],
         (string_t,))
class name(expr):
        def value(self, writep = nil):
                return self.args[0]
        def help(x, writep = nil):
                return ast.Name(x, help_ctx(writep))

def genname(x = "#:G"):
        return name(gensymname(x))

@defprim(intern("ASSIGN")[0],
         (expr, prim))
class assign(stmt):
        def help(place, value, tn = nil, spills = []):
                the_tn = tn or genname("TARGET-")
                p, v = help(value)
                simple_val_p = isinstance(value, (name, const))
                statem_val_p = not not p
                simple_tgt_p = isinstance(place, name)
                ret =  (p + ([ ast.Assign([ help_expr(the_tn) ], v) ] if p else [])
                        ) + [ ast.Assign([ help_expr(place) ], help_expr(value if not statem_val_p else the_tn))
                              ], help_expr(value if simple_val_p else
                                           place if simple_tgt_p else
                                           the_tn)
                # cl._debug_printf("ASSIGN:\nsimp-val-p  %s\nstmt-val-p  %s\nsimp-tgt-p  %s",
                #                  simple_val_p,
                #                  statem_val_p,
                #                  simple_tgt_p)
                # cl._debug_printf("ASSIGN gets:\n%s  =  %s\nASSIGN yields P:\n%s\nV:\n%s",
                #                  place, value,
                #                  "\n".join(pp_ast_as_code(x) for x in ret[0]),
                #                  pp_ast_as_code(ret[1]))
                return ret

@defprim(intern("ATTR-REF")[0],
         (prim, prim))
class attr(indet):
        a_const = defstrategy(test = lambda _, attr: isinstance(attr, string),
                              keys = "const attr")
        b_var   = defstrategy(keys = efless)

@defprim(intern("CONST-ATTR-REF")[0],
         (expr_spill, string))
class const_attr(efless):
        ## We assume, that within the domain of emitted code, objects do not have accessors defined.  So, efless.
        def help(x, attr, writep = nil): return ast.Attribute(help_expr(x), attr.value(), help_ctx(writep))
        attr = identity_method("const attr")

@defprim(intern("VAR-ATTR-REF")[0],
         (expr_spill, expr_spill))
class var_attr(efless):
        def help(x, attr):
                return help(funcall(name("getattr"), help_expr(x), help_expr(attr)))
        attr = identity_method()

@defprim(intern("INDEX")[0],
         (expr_spill, expr_spill))
class index(expr):
        def help(x, index, writep = nil):
                return ast.Subscript(help_expr(x), ast.Index(help_expr(index)), help_ctx(writep))

@defprim(intern("SLICE")[0],
         (expr_spill, expr_spill, maybe_expr_spill, maybe_expr_spill))
class slice(expr):
        def help(x, start, end, step, writep = nil):
                return ast.Subscript(help_expr(x), ast.Slice(help_expr(start),
                                                             help_expr(end if end is not nil else
                                                                       name("None")),
                                                             help_expr(step if step is not nil else
                                                                       name("None"))),
                                     help_ctx(writep))

@defprim(intern("PYLIST")[0],
         ([expr_spill],))
class pylist(expr):
        def help(*xs):
                return ast.List([ help_expr(x) for x in xs ], help_ctx(nil))

def prim_attr_chain(xs, writep = nil):
        return reduce((lambda acc, attr: const_attr(acc, attr, writep = writep)),
                      xs[1:],
                      xs[0])

@defprim(intern("GLOBAL")[0],
         ([name],))
class global_(stmt):
        def help(*xs):
                return [ ast.Global([ x.value() for x in xs ]
                                  ) ], help_expr(name("None"))

@defprim(intern("NONLOCAL")[0],
         ([name],))
class nonlocal_(stmt):
        def help(*xs):
                return [ ast.Nonlocal([ x.value() for x in xs ]
                                      ) ], help_expr(name("None"))

@defprim(intern("IMPORT")[0],
         ([name],))
class import_(stmt):
        def help(*xs):
                return [ ast.Import([ ast.alias(x.value(), None) for x in xs ]
                                    ) ], help_expr(name("None"))

###
### Constants
###
@defprim(intern("INTEGER")[0],
         (int,))
class integer(literal):
        def help(x): return ast.Num(x)

@defprim(intern("FLOAT-NUM")[0],
         (float,))
class float_num(literal):
        def help(x): return ast.Num(x)

@defprim(intern("SYMBOL")[0],
         (string_t,))
class symbol(literal):
        def value(x, writep = nil):
                return x
        def help(x, writep = nil):
                return ast.Name(x, help_ctx(writep))

@defprim(intern("LITERAL-LIST")[0],
         ([literal],))
class literal_list(literal):
        def help(*xs):
                return reduce(lambda cdr, car: ast.List([help_expr(car), cdr], help_ctx(nil)),
                              ## Namespace separation leak:
                              reversed(xs + (help_nil(),)))

@defprim(intern("LITERAL-HASH-TABLE-EXPR")[0],
         ([(expr_spill, expr_spill)],))
## Unregistered Issue EXTREME-NICETY-OF-AUTOMATIC-RECLASSIFICATION-TO-A-NARROWER-TYPE
class literal_hash_table_expr(expr):
        def help(*kvs):
                keys, vals = zip(*kvs)
                return ast.Dict(help_exprs(keys), help_exprs(vals))

###
### Functions
###
@defprim(intern("LAMBDA")[0],
         ((([name],),
          ([name],), ([prim],), name,
          ([name],), ([prim],), name),
          [prim]))
class lambda_(indet):
        "NOTE: default value form evaluation is not delayed."
        a_expr = defstrategy(test = (lambda pyargs, *body, name = nil, decorators = []:
                                             suite_unspilled_expr_p(body) and not (name or decorators)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

@defprim(intern("DEFUN")[0],
         (name, (([name],),
                 ([name],), ([expr],), (maybe, name),
                 ([name],), ([expr],), (maybe, name)),
          ([expr],),
          [prim]))
class defun(body):
        def help(nam, pyargs, decorators, *body):
                return [ ast.FunctionDef(
                                name = nam.value(),
                                args = help_args(*pyargs),
                                decorator_list = help_exprs(decorators),
                                returns = None,
                                body = help_tail_prog(body, kind = ast.Return))
                         ], help_expr(nam)
        @defmethod(lambda_)
        def lambda_(pyargs, expr, name = nil, decorators = []):
                return defun(name or genname("DEFLAM-"), pyargs, decorators, expr)

@defprim(intern("LAMBDA-EXPR")[0],
         ((([name],),
           ([name],), ([expr_spill],), name,
           ([name],), ([expr_spill],), name),
          expr))
class lambda_expr(expr):
        def help(pyargs, expr): return ast.Lambda(help_args(*pyargs), help_expr(expr))
        lambda_ = identity_method()

###
### Binding
###
def bindings_free_eval_order_p(bindings):
        return all(isinstance(x, (name, const))
                   for x, form in bindings)

@defprim(let,
         (([(name, prim)],),
          [prim]))
class let(indet):
        a_expr = defstrategy(test = lambda bindings, *body: (all(unspilled_expr_p(x)
                                                                 for x in list(zip(*bindings))[1])
                                                             and suite_unspilled_expr_p(body)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

@defprim(intern("LET-EXPR")[0],
         (([(name, expr_spill)],),
          expr))
class let_expr(expr):
        def help(bindings, expr):
                ns, vs = list(zip(*bindings))
                return help(funcall(lambda_expr(fixed_ll(ns),
                                                expr),
                                    *vs))
        let = identity_method()

@defprim(intern("LET-THUNK")[0],
         (([(name, expr_spill)],),
          [prim]))
class let_thunk(body):
        "The most universal, yet bulky kind of LET."
        def help(bindings, *body):
                ns, vs = list(zip(*bindings))
                tn = genname("LET-THUNK-")
                return help(progn(defun(tn, fixed_ll(ns), [],
                                        *body),
                                  funcall(tn, *vs)))
        let = identity_method()

setq = intern("SETQ")[0]

@defprim(intern("LET*")[0],
         (([(name, prim)],),
         [prim]))
class let_(indet):
        a_head        = defstrategy(test  = lambda _, *__, headp = nil, uncaught_tail = nil: headp or uncaught_tail,
                                    keys  = setq)
        b_reorderable = defstrategy(test  = lambda bindings, *_: bindings_free_eval_order_p(bindings),
                                    xform = redetermining_as(let))
        # TODO: try to reduce frame creation, even in the non-reorderable case.
        c_expr        = defstrategy(test   = lambda bindings, *body: (all(unspilled_expr_p(x)
                                                                          for x in list(zip(*bindings))[1])
                                                                      and suite_unspilled_expr_p(body)),
                                    keys   = expr)
        d_stmt        = defstrategy(keys   = stmt)

@defprim(intern("LET*-SETQ")[0],
         (([(name, prim)],),
          [prim])) ## This one handles binding spills by itself.
class let__setq(body):
        "Can only be used as a tail, when it can be proved, that no unwind will use mutated variables."
        def help(bindings, *body, headp = None):
                sum = (tuple(assign(n, v) for n, v in bindings)
                       + body)
                pn = progn(*sum)
                return help(progn(*sum))
        let_ = identity_method(setq)

@defprim(intern("LET*-EXPR")[0],
         (([(name, expr_spill)],),
          expr))
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
          [prim]))
class let__stmt(body):
        def help(bindings, *body):
                return help(let((bindings[0],),
                                let_(bindings[1:],
                                     *body))
                            if bindings else
                            progn(*body))
        let_ = identity_method()

@defprim(intern("PROGV")[0],
         (([(expr_spill, expr_spill)],),
          [prim]))
class progv(body):
        def help(vars, vals, *body):
                tn = genname("VALUE-")
                return [ ast.With(funcall(impl_ref("_env_cluster"),
                                          literal_hash_table_expr(*((help_expr(var), help_expr(val))
                                                                    # No type checking!
                                                                    for var, val in zip(vars, vals)))),
                                  None,
                                  help_prog_star(assign(tn, progn(*body))))
                         ], help_expr(tn)

flet = intern("FLET")[0]

@defprim(flet,
         (([(name, (([name],),
                    ([name],), ([expr_spill],), name,
                    ([name],), ([expr_spill],), name),
             [prim])],),
          [prim]))
class flet(indet):
        a_expr = defstrategy(test = lambda bindings, *body: (suite_unspilled_expr_p(body) and
                                                             all(suite_unspilled_expr_p(body)
                                                                 for name, lam, body in bindings)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

@defprim(intern("FLET-EXPR")[0],
         (([(name, (([name],),
                    ([name],), ([expr_spill],), name,  ## EXPR-SPILL?
                    ([name],), ([expr_spill],), name),
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

@defprim(intern("FLET-STMT")[0],
         (([(name, (([name],),
                    ([name],), ([expr_spill],), name,  ## EXPR-SPILL?
                    ([name],), ([expr_spill],), name),
             [prim])],),
          [prim]))
class flet_stmt(body):
        def help(bindings, *body):
                names, lams, bodies = zip(*bindings)
                tn = genname("FLET-THUNK-")
                return (help_prog([defun(tn, fixed_ll([]),
                                         *([ defun(name, lam, [],
                                                   # this suffers from NAME being available for BODY
                                                   *body)
                                             for name, lam, *body in bindings ]
                                           + body))]),
                        help_expr(funcall(tn)))
        flet = identity_method()

@defprim(intern("LABELS")[0],
         (([(name, (([name],),
                    ([name],), ([expr_spill],), name,  ## EXPR-SPILL?
                    ([name],), ([expr_spill],), name),
             [prim])],),
          [prim]))
class labels(body):
        def help(bindings, *body):
                tn = genname("LABELS-THUNK-")
                return (help_prog([defun(tn, fixed_ll([]),
                                         *([ defun(name, lam, [],
                                                   *body)
                                             for name, lam, *body in bindings ]
                                           + body))]),
                        help_expr(funcall(tn)))

###
### Control
###
@defprim(intern("PROGN")[0],
         ([prim],))
class progn(indet):
        a_expr = defstrategy(test  = lambda *body: suite_unspilled_expr_p(body),
                             xform = lambda x, *_: x)
        b_stmt = defstrategy(keys  = body)

@defprim(intern("PROGN-STMT")[0],
         ([prim],))
class progn_stmt(body):
        def help(*body):
                return help_progn(body)
        progn = identity_method()

if_ = intern("IF")[0]

@defprim(if_,
         (prim, prim, prim))
class if_(indet):
        a_expr = defstrategy(test = lambda _, conseq, ante: (unspilled_expr_p(conseq) and
                                                             unspilled_expr_p(ante)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

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
                tn = genname("IFVAL-")
                return [ ast.If(tv,
                                cp + help_prog([assign(tn, cv)]),
                                ap + help_prog([assign(tn, av)])
                                ) ], help_expr(tn)
        if_ = identity_method()

funcall = intern("FUNCALL")[0]

@defprim(funcall,
         (expr_spill, [expr_spill]))
class funcall(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, *fixed_args):
                return ast.Call(help_expr(func),
                                help_exprs(fixed_args), [], None, None)

@defprim(intern("APPLY")[0],
         (expr_spill, expr_spill, [expr_spill]))
class apply(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, arg, *args):
                fixed_args, restarg = (([arg] + args[:-1], args[-1]) if args else
                                       ([],                arg))
                return ast.Call(help_expr(func), help_exprs(fixed_args), [], help_expr(restarg), None)

@defprim(intern("UNWIND-PROTECT")[0],
         (prim,
          [prim]))
class unwind_protect(body):
        def help(protected_form, *body):
                # need a combinator for PRIM forms
                if body:
                        tn = genname("UWP-VALUE-")
                        return [ ast.TryFinally(help_prog([assign(tn, protected_form)]),
                                                help_prog(body)
                                                ) ], help_expr(tn)
                else:
                        return help_progn_star(protected_form)

@defprim(intern("LOOP")[0],
         ([prim],))
class loop(body):
        def help(*body):
                return [ ast.While(help_expr(name("True")),
                                   help_prog(*body),
                                   []) ], help_nil()

@defprim(intern("ASSERT")[0], (expr_spill, expr_spill))
class assert_(stmt):
        def help(condition, description):
                return [ ast.Assert(help_expr(condition), help_expr(description)
                                    ) ], help_nil()

@defprim(intern("RESIGNAL")[0], ())
class resignal(stmt):
        def help():
                return [ ast.Raise() ], help_nil()

@defprim(intern("CATCH")[0],
         (expr_spill,
          [prim]))
class catch(body):
        def help(tag, *body):
                val_tn, ex_tn = genname("BODY-VALUE-"), genname("EX")
                return [ ast.TryExcept(
                                help_prog([assign(val_tn, progn(*body))]),
                                [ ast.ExceptHandler(impl_ref("__catcher_throw__"),
                                                    ex_tn.value(),
                                                    help_prog_star(
                                                        if_(is_(attr(ex_tn, string("ball")),
                                                                help_expr(tag)),
                                                            progn(funcall(ref_impl("__catch_maybe_reenable_pytracer"),
                                                                          ex_tn),
                                                                  assign(val_tn,
                                                                         attr(ex_tn, string("value")))),
                                                            resignal()))) ],
                                [])
                         ], help_expr(val_tn)

@defprim(intern("THROW")[0],
         (expr_spill, expr_spill))
class throw(expr):
        def help(tag, value):
                return help_expr(funcall(impl_ref("__throw"),
                                         help_expr(tag), help_expr(value)))

###
### References
###
@defprim(intern("SPECIAL-REF")[0],
         (name,))
class special_ref(efless):
        def help(name):
                return help(funcall(impl_ref("_symbol_value"), name))

@defprim(intern("SPECIAL-SETQ")[0],
         (name, expr_spill))
class special_setq(expr):
        def help(name, value):
                return help(funcall(impl_ref("_do_set"), name, value, name("None")))

@defprim(intern("IMPL-REF")[0],
         (str,))
class impl_ref(expr):
        def help(x):
                return cl._ast_attribute_chain(["cl", x])

@defprim(intern("BUILTIN-REF")[0],
         (string_t,))
class blin_ref(expr):
        def help(x):
                return help_expr(name(x))

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

@defprim(intern("RPLACA")[0], (expr_spill, expr_spill))
class rplaca(expr):
        def help(cons, value):
                return help_progn_star(assign(index(cons, integer(0), writep = t),
                                              value, tn = genname("CAR-")))

@defprim(intern("RPLACD")[0], (expr_spill, expr_spill))
class rplacd(expr):
        def help(cons, value):
                return help_progn_star(assign(index(cons, integer(1), writep = t),
                                              value, tn = genname("CDR-")))

###
### Operations
###
def help_boolop(op, xs):     return ast.BoolOp(op(), [ help_expr(x) for x in xs ])
def help_unop(op, x):        return ast.UnaryOp(op(), help_expr(x))
def help_binop(op, x, y):    return ast.BinOp(help_expr(x), op(), help_expr(y))
def help_compare(op, x, ys): return ast.Compare(help_expr(x), op(), [ help_expr(y) for y in ys ])

def help_binop_seq(args, type, one):
        init, rest = ((args[0], args[1:]) if args else (one, args))
        return reduce(lambda x, y: ast.BinOp(x, type(), help_expr(y)),
                      rest, help_expr(init))

## AND OR
@defprim(intern("AND")[0], ([expr_spill],))
class and_(potconst):
        def help(xs): return help_boolop(ast.And, xs)

@defprim(intern("OR")[0], ([expr_spill],))
class or_(potconst):
        def help(xs): return help_boolop(ast.Or, xs)

## + - * / MOD POW << >> LOGIOR LOGXOR LOGAND FLOOR
@defprim(intern("+")[0], ([expr_spill],))
class add(potconst):
        def help(*xs): return help_binop_seq(xs, ast.Add)

@defprim(intern("-")[0], ([expr_spill],))
class subtract(potconst):
        def help(*xs): return help_binop_seq(xs, ast.Sub)

@defprim(intern("*")[0], ([expr_spill],))
class multiply(potconst):
        def help(*xs): return help_binop_seq(xs, ast.Mult)

@defprim(intern("/")[0], ([expr_spill],))
class divide(potconst):
        def help(*xs): return help_binop_seq(xs, ast.Div)

@defprim(intern("MOD")[0], (expr_spill, expr_spill))
class mod(potconst):
        def help(x, y): return help_binop(ast.Mod, x, y)

@defprim(intern("POW")[0], (expr_spill, expr_spill))
class expt(potconst):
        def help(x, y): return help_binop(ast.Pow, x, y)

@defprim(intern("<<")[0], (expr_spill, expr_spill))
class lshift(potconst):
        def help(x, y): return help_binop(ast.LShift, x, y)

@defprim(intern(">>")[0], (expr_spill, expr_spill))
class rshift(potconst):
        def help(x, y): return help_binop(ast.RShift, x, y)

@defprim(intern("LOGIOR")[0], (expr_spill, expr_spill))
class logior(potconst):
        def help(x, y): return help_binop(ast.BitOr, x, y)

@defprim(intern("LOGXOR")[0], (expr_spill, expr_spill))
class logxor(potconst):
        def help(x, y): return help_binop(ast.BitXor, x, y)

@defprim(intern("LOGAND")[0], (expr_spill, expr_spill))
class logand(potconst):
        def help(x, y): return help_binop(ast.BitAnd, x, y)

@defprim(intern("FLOOR")[0], (expr_spill, expr_spill))
class floor(potconst):
        def help(x, y): return help_binop(ast.FloorDiv, x, y)

## NOT LOGNOT
@defprim(intern("NOT")[0], (expr_spill,))
class not_(potconst):
        ## Optimisation: fold (NOT (EQ X Y)) to ast.IsNot
        def help(x): return help_unop(ast.Not, x)

@defprim(intern("LOGNOT")[0], (expr_spill,))
class lognot(potconst):
        def help(x): return help_unop(ast.Invert, x)

## EQ NEQ EQUAL NOT-EQUAL < <= > >= IN NOT-IN
@defprim(intern("EQ")[0], (expr_spill, expr_spill))
class eq(potconst):
        def help(x, y): return help_compare(ast.Is, x, [y])

@defprim(intern("NEQ")[0], (expr_spill, expr_spill))
class neq(potconst):
        def help(x, y): return help_compare(ast.IsNot, x, [y])

@defprim(intern("EQUAL")[0], (expr_spill, [expr_spill]))
class equal(potconst):
        def help(x, *ys): return help_compare(ast.Eq, x, ys)

@defprim(intern("NOT-EQUAL")[0], (expr_spill, [expr_spill]))
class nequal(potconst):
        def help(x, *ys): return help_compare(ast.NotEq, x, ys)

@defprim(intern("<")[0], (expr_spill, [expr_spill]))
class lthan(potconst):
        def help(x, *ys): return help_compare(ast.Lt, x, ys)

@defprim(intern("<=")[0], (expr_spill, [expr_spill]))
class lorequal(potconst):
        def help(x, *ys): return help_compare(ast.LtE, x, ys)

@defprim(intern(">")[0], (expr_spill, [expr_spill]))
class gthan(potconst):
        def help(x, *ys): return help_compare(ast.Gt, x, ys)

@defprim(intern(">=")[0], (expr_spill, [expr_spill]))
class gorequal(potconst):
        def help(x, *ys): return help_compare(ast.GtE, x, ys)

@defprim(intern("IN")[0], (expr_spill, expr_spill))
class in_(potconst):
        def help(x, y): return help_compare(ast.In, x, [y])

@defprim(intern("NOT-IN")[0], (expr_spill, expr_spill))
class not_in(potconst):
        def help(x, y): return help_compare(ast.NotIn, x, [y])
