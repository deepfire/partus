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

def identity_method():
        return (identity,) # just a marker, to be processed by defprim

def defstrategy(test = lambda _: True, method = None):
        assert(method)
        return (defstrategy, test, method)

def strategyp(x):
        return isinstance(x, tuple) and x and x[0] is defstrategy and x[1:] or (None, None)

def defprim(name, form_specifier):
        def do_defprim(cls):
                __primitives__[name] = __primitives_by_pyname__[cls.__name__] = cls
                class_key_supers = cls.__mro__[0:cls.__mro__.index(prim)]
                cls.form_specifier = form_specifier
                def primitive_add_method_keys_by_supers(primitive_method_pool, method, supers):
                        for pcl in supers:
                                primitive_method_pool[pcl].add(method)
                help_stdmethod = cls.__dict__["help"]
                for name, method_spec in cls.__dict__.items():
                        test, method = strategyp(method_spec)
                        if test and method:
                                ## implementation strategy
                                cls.help_strategies.append((name, test, method))
                                cls.help_strategies.sort()
                        elif name != "help": # must be an indet method
                                method, identityp = ((method_spec, nil)  if isinstance(method_spec, types.FunctionType) else
                                                     (help_stdmethod, t) if (isinstance(method_spec, tuple) and
                                                                             method_spec[0] is identity) else
                                                     error("Invalid method specifier: %s", method_spec))
                                primitive_add_method_keys_by_supers(find_indet_method_pool(name), method, class_key_supers)
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
        def __init__(self, *args):
                self.args = args
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

def determine(cls, args):
        for name, test, target_primitive in cls.help_strategies:
                if test(*args):
                        ## Simplify and compute spills.
                        return (method if not isinstance(method, prim) else
                                cls.find_method([target_primitive]))(*args)
        else:
                raise Exception("Unhandled primitive form: %s" % self)

class indet(prim):
        """Those have context-sensitivity, provided by a choice of strategies.
           The indeterminacy is short-lived, though."""
        def __new__(cls, *args):
                return determine(cls, args)

class det(prim):
        "Those are spill-determinate, post-init."
        def __init__(self, *args):
                prim.__init__(self, *args)
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

## to consider: no-return

###
### Toolkit
###
def stmtp(x):     return isinstance(x, stmt)
def bodyp(x):     return isinstance(x, body)
def exprp(x):     return isinstance(x, expr)
def the_stmt(x):
        if not isinstance(x, stmt):   error("Statement expected, was: %s", x)
        return x
def the_expr(x):
        if not isinstance(x, expr):   error("Expression expected, was: %s", x)
        return x
def the_efless(x):
        if not isinstance(x, efless): error("Effect-less expression expected, was: %s", x)
        return x
def the_const(x):
        if not isinstance(x, const):  error("Constant expression expected, was: %s", x)
        return x
def the_str(x):
        if not isinstance(x, str):    error("String expected, was: %s", x)
        return x

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
        r = x.help(*x.args)
        p, v = (r if isinstance(r, tuple) else
                ([], r))
        return x.spills + p or TheEmptyList, v

def help_exp(x):
        p, v = help(x)
        p and error("Helped %s to non-expr %s, %s, where an expression result was expected.", x, p, v)
        return v

def help_exps(xs):
        return [ help_exp(x) for x in xs ]

def help_progn(xs):
        assert(xs)
        p_acc = []
        for x in xs[:-1]:
                p, v = help(x)
                p_acc.extend(p)
                p_acc.append(ast.Expr(v))
        p, v = help(xs[-1])
        p_acc.extend(p)
        return p_acc, v

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
                defaults         = help_exps(optvals),
                kw_defaults      = help_exps(keyvals))

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
                        return (arg.spills + [setq_lexical(tn, arg)],
                                ref_lexical(tn))
                return processors.get(type(spec), type_check)(spec, arg, force_spill = force_spill)
        primitive.args, primitive.spills = tuple_spills(primitive, args)
        return primitive

def process(p):
        ...

###
### Granch scheme of things
###
## 1. Init-time calculation of spills for determinates
## 2. Help-time
##  - indeterminates dispatch to strategies
## ... ?

###
### High-level (un-lived, indeterminate)
###
@defprim(lambda_,
         ((([symbol_t],),
          ([symbol_t],), ([prim],), symbol_t,
          ([symbol_t],), ([prim],), symbol_t),
          [prim]))
class lambda_(indet):
        "NOTE: default value form evaluation is not delayed."
        1_expr = defstrategy(test   = lambda pyargs, *body: suite_unspilled_expr_p(body),
                             method = expr)
        2_stmt = defstrategy(method = body)

@defprim(if_,
         (prim, prim, prim))
class if_(indet):
        1_expr = defstrategy(test   = lambda _, conseq, ante: (prim_unspilled_expr_p(conseq) and
                                                               prim_unspilled_expr_p(ante)),
                             method = expr)
        2_stmt = defstrategy(method = body)

@defprim(intern("PROGN")[0],
         ([prim],))
class progn(indet):
        1_expr = defstrategy(test   = lambda *body: suite_unspilled_expr_p(body),
                             method = help_exp)
        2_stmt = defstrategy(method = help_progn_star)

@defprim(let,
         (([(symbol_t, prim)],),
          [prim]))
class let(indet):
        1_expr = defstrategy(test   = lambda bindings, *body: suite_unspilled_expr_p(body),
                             method = expr)
        2_stmt = defstrategy(method = body)

###
### Constants
###
@defprim(string,
         (str,))
class string(const):
        def help(name): return ast.Str(x)

@defprim(integer,
         (int,))
class integer(const):
        def help(x): return ast.Num(x)

@defprim(symbol,
         (symbol_t,))
class symbol(const):
        def help(name): return ast.Name(sym_pyname(name), ast.Load())

@defprim(intern("QUOTED-LIST")[0],
         ([const],))
class quoted_list(const):
        def help(*xs):
                return reduce(lambda car, cdr: ast.List(help_exp(car), cdr),
                              reversed(xs),
                              ast.Name(sym_pyname(nil), ast.Load()))

@defprim(function,
         (or_t, symbol_t, ((eql_t, cl.setq), symbol_t)))
class function(efless):
        def help(name): return ast.Name(fun_pyname(name), ast.Load())

###
### Functions
###
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
                                decorator_list = help_exps(decorators),
                                returns = None,
                                body = help_tail_prog(body, kind = ast.Return))
                         ], ast.Name(fun_pyname(name), ast.Load())
        @defmethod(lambda_)
        def lambda_(pyargs, expr):
                return defun(gensym("DEFLAM-"), pyargs, [], expr)

@defprim(intern("LAMBDA-EXPR")[0],
         ((([symbol_t],),
           ([symbol_t],), ([expr_spill],), symbol_t,
           ([symbol_t],), ([expr_spill],), symbol_t),
          expr))
class lambda_expr(expr):
        def help(pyargs, expr): return ast.Lambda(help_args(*pyargs), help_exp(expr))
        lambda_ = identity_method()

@defprim(funcall,
         (expr_spill, [expr_spill]))
class funcall(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, *fixed_args):
                return ast.Call(help_exp(func), [ help_exp(x) for x in fixed_args ], [])

@defprim(apply,
         (expr_spill, expr_spill, [expr_spill]))
class apply(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, arg, *args):
                fixed_args, arglist = (([arg] + args[:-1], args[-1]) if args else
                                       ([],                [arg]))
                return ast.Call(help_exp(func), [ help_exp(x) for x in fixed_args ], [], help_exp(arglist))

###
### Binding
###
@defprim(intern("LET-EXPR"),
         (([(symbol_t, expr_spill)],),
          expr))
class let_expr(expr):
        def help(bindings, expr):
                ns, vs = list(zip(*bindings))
                return ast.Call(ast.Lambda(help_args([ var_pyname(x) for x in ns ], [], [], None, [], [], None),
                                           help_exp(expr)),
                                help_exps(vs))
        let = identity_method()

@defprim(intern("LET-TAIL"),
         (([(symbol_t, expr_spill)],),
          [prim]))
class let_tail(body):
        "Can only be used, when it can be proved, that no used variables can be mutated."
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(bindings, *body):
                ns, vs = list(zip(*bindings))
                p, v = help_progn(body)
                return [ ast.Assign([ ast.Name(var_pyname(x) for x in ns) ],
                                    ast.List([ help_exp(x) for x in vs], ast.Load()))
                         ] + p, v

@defprim(intern("LET-THUNK"),
         (([(symbol_t, expr_spill)],),
          [prim]))
class let_thunk(body):
        "The most universal, yet bulky kind of LET."
        def help(bindings, *body):
                ns, vs = list(zip(*bindings))
                tn = gensym("LET-THUNK-")
                return [ ast.FunctionDef(
                                fun_pyname(tn),
                                help_args([ var_pyname(x) for x in ns ], [], [], None, [], [], None),
                                decorator_list = [],
                                returns = None,
                                body = help_tail_prog(body, kind = ast.Return))
                         ], ast.Call(ast.Name(fun_pyname(tn), ast.Load()),
                                     help_exps(vs))
        let = identity_method()

###
### Control
###
@defprim(intern("IMPL-CALL")[0],
         (str, [expr_spill]))
class impl_call(expr):
        def help(name, *args):
                return ast.Call(_ast_attribute_chain("cl", the_str(name)), help_exps(args))

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
                                cp + help(setq_lexical(tn, cv))[0],
                                ap + help(setq_lexical(tn, av))[0]
                                ) ], help(ref_lexical(tn))
        if_ = identity_method()

###
### References
###
@defprim(intern("REF-LEXICAL")[0],
         (symbol_t,))
class ref_lexical(efless): ## potconst, if we choose to do constant propagation this way
        def help(name):
                return ast.Name(var_pyname(name), ast.Load())

@defprim(intern("SETQ-LEXICAL")[0],
         (symbol_t, expr_spill))
class setq_lexical(stmt):
        def help(name, value):
                pyname = var_pyname(name)
                return [ ast.Assign([ast.Name(pyname, ast.Store())], help(value))
                        ], ast.Name(pyname, ast.Load())

@defprim(intern("REF-SPECIAL")[0],
         (symbol_t,))
class ref_special(efless):
        def help(name):
                return help(impl_call("_symbol_value", symbol(name)))

@defprim(intern("SETQ-SPECIAL")[0],
         (symbol_t, expr_spill))
class setq_special(expr):
        def help(name, value):
                return help(impl_call("_do_set", symbol(name), value))

###
### Operations
###
def help_boolop(args, type): 
        init, rest = ((args[0], args[1:]) if args else (0, args))
        return reduce(lambda x, y: ast.BoolOp(x, type(), the_expr(help(y))),
                      rest, the_expr(help(init)))

@defprim(intern("+")[0],
         ([expr_spill],))
class add(potconst):
        def help(*xs): return help_boolop(xs, ast.Add)
