import cl
from cl import *
from cl import gensymname
from cl import symbol_value, sex_deeper
from cl import ensure_symbol_rtname
from cl import sex_space, defaulted
from cl import dprintf

from cl import _machine_

import ast
import sys
import types
import builtins
import collections

from more_ast import pp_ast_as_code

NoneType = type(None)

def find_primitive(name, machine = None):
        machine = defaulted_to_var(machine, _machine_)
        prim_gl = globals().get(name, None)
        x = (prim_gl if isinstance(prim_gl, primclass) else
             machine.globals().get(name, None))
        return (x if isinstance(x, primclass) else
                error("In machine %s: unhandled name %s - it is neither a method name, nor a primitive name (value is %s, of type %s).",
                      machine, repr(name), x, type(x)))

###
### Machine
###
machine_methods = { "capable_of", "globals" }
class machine():
        "The opposite edge of The Gap."
        def capable_of(self, prim) -> bool:
                return prim in self.__supported_primitives__
        def make_indeterminate_primitive(self, cls, args, keys):
                o = prim.__new__(cls, *args, **keys)
                o.__init__(*args, machine = self, **keys)
                return o
        def after_initialise_determinate_primitive(self, instance):
                return instance
        def __getattr__(self, name):
                mach_name = type(self).__name__.upper()
                if name in machine_methods:
                        return self.__dict__.get(name)
                x = find_primitive(name, self)
                if not self.capable_of(x):
                        error("In machine %s: not capable of handling primitive %s.", mach_name, name.upper())
                def instantiator(*args, **keys):
                        instance = x(*args, machine = self, **keys)
                        return instance
                return instantiator

def defmachine(cls):
        if not issubclass(cls, machine):
                error("In DEFMACHINE %s: not subclass of machine.", cls.__name__.upper())
        for req_method in ["globals", "make_indeterminate_primitive", "after_initialise_determinate_primitive"]:
                if not hasattr(cls, req_method):
                        error("In DEFMACHINE %s: missing required method %s.", cls.__name__.upper(), req_method)
        if not hasattr(cls, "__supported_primitives__"):
                error("In DEFMACHINE %s: missing required slot %s.", cls.__name__.upper(), "__supported_primitives__")
        return cls

###
### Primitives
###
__primitives__           = dict()
__primitives_by_pyname__ = dict()

def defprim(name, form_specifier):
        def do_defprim(cls):
                __primitives__[name] = __primitives_by_pyname__[cls.__name__] = cls
                cls.form_specifier = form_specifier
                return cls
        return do_defprim

class primclass(type):
        def __init__(self, name, cpl, dict):
                ####
                #### Python AST backend:
                ## Methods are specific lowering functions, with unspecified, yet implicit applicability.
                self.methods         = collections.defaultdict(set) ## tag -> { method }
                ## An ordered list of explicitly guarded methods.
                self.help_strategies = list()
        def __instancecheck__(cls, x):
                return issubclass(type(x), cls) or (cls == maybe_expr_spill and x is nil)

def prim_type_p(x):
        return isinstance(x, primclass)

class prim(metaclass = primclass):
        def __init__(self, *args, machine = None, **keys):
                ## Note the filtrage of the machine initarg -- we don't need it stored.
                self.args, self.keys = args, keys
        def __str__(self):
                return print_primitive(self)
        def __repr__(self):
                return print_primitive(self)

def print_primitive(x):
        return ('"%s"' % x                                             if     isinstance(x, str)      else
                "'%s"  % x                                             if     isinstance(x, symbol_t) else
                "(%s)" % " ".join(print_primitive(ix) for ix in x)     if     isinstance(x, tuple)    else
                " [ %s ] " % " ".join(print_primitive(ix) for ix in x) if     isinstance(x, list)     else
                str(x)                                                 if not isinstance(x, prim)     else
                ("(%s%s%s)" % (lambda name: (name.upper(),
                                             " " if x.args else "",
                                             ("".join(sex_deeper(len(name) + 2, lambda: print_primitive(x))
                                                      for x in x.args)
                                              if len(x.args) < 2 else
                                              ("\n" + cl.sex_space(len(name) + 2)
                                               ).join(sex_deeper(len(name) + 2, lambda: print_primitive(x))
                                                      for x in x.args)))
                               )(type(x).__name__)))

###
### Primitive categories
###
class indet(prim):
        def __new__(cls, *args, machine = None, **keys):
                machine = defaulted_to_var(machine, _machine_)
                if not machine:
                        error("While creating primitive %s: machine not specified.", cls.__name__.upper())
                return machine.make_indeterminate_primitive(cls, args, keys)

def primitive_really_trivial_p(x):
        return isinstance(x, name)

class det(prim):
        def __new__(cls, *args, machine = None, **keys):
                self = prim.__new__(cls, *args, **keys)
                prim.__init__(self, *args, **keys)
                if primitive_really_trivial_p(self):
                        return self
                machine = defaulted_to_var(machine, _machine_)
                if not machine:
                        error("While creating non-trivial primitive %s: machine not specified.", cls.__name__.upper())
                return machine.after_initialise_determinate_primitive(self) ## Still allow for some extreme self-modifiabilty.

class stmt(det):               pass ## lower to ([ast.stmt], ast.expr)
class body(stmt):              pass ## lower to ([ast.stmt], ast.expr), and has an embedded body

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

class maybe():     pass
class satisfies(): pass

###
### Generic methods on primitives
###
def exprp(x):         return isinstance(x, expr)

cl.string_set("*VALUELESS-PRIMITIVE-STATEMENT-MUST-YIELD-NIL*", t, globals = globals())

_compiler_trace_primitives_ = cl._compiler_trace_primitives_

class primitive_mismatch(error_t):
        def __init__(self, mesg, prim = None, pspec = None, spec = None, form = None):
                ni = "#<not initialised>"
                self.prim, self.pspec, self.spec, self.form = ni, ni, ni, ni
                assert(prim and pspec and spec)
                self.mesg, self.prim, self.pspec, self.spec, self.form = mesg, prim, pspec, spec, form
        def __str__(self):
                return "While matching primitive %s against its argspec %s, mismatch of %s (of type %s) with spec %s: %s." % \
                    (self.prim, self.pspec,
                     print_primitive(self.form), type(self.form).__name__,
                     self.spec,
                     self.mesg)

def map_primitives(fn, p):
        # The identity is:  map_primitives(lambda f, x: type(x)(*f(x)), prim)
        def rec_tuple(xs, spec):
                if spec and spec[0] in [maybe, satisfies]:
                        if spec[0] is maybe:
                                return xs if xs is None else rec(xs, spec[1])
                        if spec[0] is satisfies:
                                if not spec[1](xs):
                                        error("In primitive %s: element %s does not match (SATISFIES %s).",
                                              p, xs, spec[0])
                                return check_type(xs, (satisfies_t, spec[1]))
                segmentp = spec and isinstance(spec[-1], list)
                check_type(xs, (or_t, tuple, list))
                nspec, nxs = len(spec), len(xs)
                if not (segmentp and nxs >= (nspec - 1)
                        or nspec is nxs):
                        error("Invalid primitive %s: subform %s has %d elements, but %s was/were expected.",
                              primitive, xs, nxs,
                              ("at least %s" % (nspec - 1)) if segmentp else
                              ("exactly %d" % nspec))
                a_fixed, a_segment, s_spec = ((xs[:nspec - 1], xs[nspec - 1:], spec[-1][0]) if segmentp else
                                              (xs,             [],             None))
                f_spec = spec[:-1] if segmentp else spec
                return (tuple(rec(x, s)
                                 for x, s in zip(a_fixed, f_spec)) +
                        tuple(rec(x, s_spec)
                              for x in a_segment))
        def rec(x, spec = None):
                return (fn(lambda x: rec_tuple(x.args, x.form_specifier), x) if isinstance(x, prim)     else
                        rec_tuple(x, spec)                                   if isinstance(spec, tuple) else
                        the(spec, x))
        ret = rec(p)
        return ret

###
### CFG-able primitive definitions
###
## Registry:
#
# STRING SYMBOL LITERAL_LIST
# NAME INTEGER FLOAT_NUM
# ADD SUB MUL DIV MOD SHL SHR LOGIOR LOGXOR LOGAND LOGNOT FLOOR EXPT
# EQ LT LE GT GE
# NOT_
# CONS CAR CDR RPLACA RPLACD VECTOR INDEX
# ASSIGN RETURN_ PROGN IF_ LOOP UNWIND_PROTECT CATCH THROW RESIGNAL
# FUNCTION FUNCALL APPLY
# LET LET_ PROGV
# SPECIAL_REF SPECIAL_SETQ
#
###
### Handled by non-immediate constant separation pass.
###
## STRING SYMBOL LITERAL-LIST
@defprim(intern("STRING")[0],         (string_t,))
class string(literal): ...

@defprim(intern("SYMBOL")[0],         (string_t,))
class symbol(literal): ...

@defprim(intern("LITERAL-LIST")[0],   ([literal],))
class literal_list(literal): ...

###
### Immediate values
###
## NAME INTEGER FLOAT-NUM
@defprim(intern("NAME")[0],           (string_t,))
class name(expr):
        def value(self, writep = nil, globalp = nil):
                return self.args[0]

def genname(x = "#:G"):
        return name(gensymname(x + "_"))

@defprim(intern("INTEGER")[0],        (int,))
class integer(literal): ...

@defprim(intern("FLOAT-NUM")[0],      (float,))
class float_num(literal): ...

###
### Operations
###
## + - * / MOD << >> LOGIOR LOGXOR LOGAND LOGNOT FLOOR EXPT
@defprim(intern("+")[0],              ([expr_spill],))
class add(potconst): ...

@defprim(intern("-")[0],              ([expr_spill],))
class sub(potconst): ...

@defprim(intern("*")[0],              ([expr_spill],))
class mul(potconst): ...

@defprim(intern("/")[0],              ([expr_spill],))
class div(potconst): ...

@defprim(intern("MOD")[0],            (expr_spill, expr_spill))
class mod(potconst): ...

@defprim(intern("<<")[0],             (expr_spill, expr_spill))
class shl(potconst): ...

@defprim(intern(">>")[0],             (expr_spill, expr_spill))
class shr(potconst): ...

@defprim(intern("LOGIOR")[0],         (expr_spill, expr_spill))
class logior(potconst): ...

@defprim(intern("LOGXOR")[0],         (expr_spill, expr_spill))
class logxor(potconst): ...

@defprim(intern("LOGAND")[0],         (expr_spill, expr_spill))
class logand(potconst): ...

@defprim(intern("LOGNOT")[0],         (expr_spill,))
class lognot(potconst): ...

@defprim(intern("FLOOR")[0],          (expr_spill, expr_spill))
class floor(potconst): ...

@defprim(intern("EXPT")[0],           (expr_spill, expr_spill))
class expt(potconst): ...

## EQ < <= > >=

@defprim(intern("EQ")[0],             (expr_spill, expr_spill))
class eq(potconst): ...

@defprim(intern("<")[0],              (expr_spill, [expr_spill]))
class lt(potconst): ...

@defprim(intern("<=")[0],             (expr_spill, [expr_spill]))
class le(potconst): ...

@defprim(intern(">")[0],              (expr_spill, [expr_spill]))
class gt(potconst): ...

@defprim(intern(">=")[0],             (expr_spill, [expr_spill]))
class ge(potconst): ...

## NOT -- the only logical op worth a primitive

@defprim(intern("NOT")[0], (expr_spill,))
class not_(potconst): ...

###
### Data structures
###
## CONS CAR CDR RPLACA RPLACD VECTOR INDEX
@defprim(intern("CONS")[0],           (expr_spill, expr_spill))
class cons(expr): ...

@defprim(intern("CAR")[0],            (expr_spill,))
class car(expr): ...

@defprim(intern("CDR")[0],            (expr_spill,))
class cdr(expr): ...

@defprim(intern("RPLACA")[0],         (expr_spill, expr_spill))
class rplaca(expr): ...

@defprim(intern("RPLACD")[0],         (expr_spill, expr_spill))
class rplacd(expr): ...

@defprim(intern("VECTOR")[0],         ([expr_spill],))
class vector(expr): ...

@defprim(intern("INDEX")[0],          (expr_spill, expr_spill))
class index(expr): ...

###
### Control
###
## ASSIGN RETURN PROGN IF LOOP UNWIND-PROTECT CATCH THROW RESIGNAL
@defprim(intern("ASSIGN")[0],         (name, prim))
class assign(stmt): ...

@defprim(intern("RETURN")[0],         (expr_spill,))
class return_(stmt): ...

class progn_like(): pass

def prim_nil():
        return name(cl.unit_symbol_rtname(nil))

def simplify_progns(children: [prim]) -> (prim, bool):
        children = children or [prim_nil()]
        flattened = []
        for c in children:
                if isinstance(c, progn_like):
                        flattened.extend(c.args)
                else:
                        flattened.append(c)
        efful = [ x for x in flattened[:-1]
                  if not isinstance(x, efless) ]
        efful.append(flattened[-1])
        return efful 

@defprim(intern("PROGN")[0],          ([prim],))
class progn(body, progn_like):
        def __new__(cls, *children, **keys):
                prims = simplify_progns(children)
                # dprintf("\nPRE-SIMP:\n%s\n\nPOST-SIMP %s:\n%s",
                #                  "\n          -------\n".join(str(x) for x in children),
                #                  "expr" if exprp else "non-expr",
                #                  "\n          -------\n".join(str(x) for x in prims))
                return prims[0] if len(prims) is 1 else object.__new__(cls, *prims, **keys) # progn_stmt(*prims)

@defprim(intern("IF")[0],             (prim, prim, prim))
class if_(indet): ...

@defprim(intern("LOOP")[0],           (prim,))
class loop(body): ...

@defprim(intern("UNWIND-PROTECT")[0], (prim, prim))
class unwind_protect(body): ...

@defprim(intern("CATCH")[0],          (expr_spill, prim))
class catch(body): ...

@defprim(intern("THROW")[0],          (expr_spill, expr_spill))
class throw(expr): ...

@defprim(intern("RESIGNAL")[0],       ())
class resignal(stmt): ...

###
### Functions
###
## FUNCTION FUNCALL APPLY
@defprim(intern("FUNCTION")[0],       (name, ((maybe, name), [name]), prim))
class function(indet): ...

@defprim(intern("FUNCALL")[0],        (expr_spill, [expr_spill]))
class funcall(expr): ...

@defprim(intern("APPLY")[0],          (expr_spill, expr_spill, [expr_spill]))
class apply(expr): ...

###
### Binding
###
## LET LET* PROGV
@defprim(intern("LET")[0],            (([(name, prim)],), prim))
class let(indet): ...

@defprim(intern("LET*")[0],           (([(name, prim)],), prim))
class let_(indet): ...

@defprim(intern("PROGV")[0],          (([expr_spill],), ([expr_spill],), prim))
class progv(body): ...

###
### Dynamic scope
###
## SPECIAL-REF SPECIAL-SETQ
@defprim(intern("SPECIAL-REF")[0],    (name,))
class special_ref(efless): ...

@defprim(intern("SPECIAL-SETQ")[0],   (name, expr_spill))
class special_setq(expr): ...
