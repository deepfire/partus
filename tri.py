import cl
import primitives as p

from cl import attrify_args, error, gensymname
from cl import t, nil, symbol_value, intern, intern_and_bind, progv as progv_
# from cl import _

from primitives import machine, prim, body, stmt, expr, efless, const, literal, indet
from primitives import defmachine

## Entire registry, essentially.  Automatable?  Worth it?  Maybe as soon as we hit the third backend..
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

def cfg_error(kind, control, *args):
        error("While CFG-lowering %s: " + control, kind, *args)

# The map we assemble on the journey across The Gap

class unit_global_data_t():
        __slots__ = ("constant_values", "constant_names", "functions", "functions_by_name", "closures", "load_effects")
        def __init__(self):
                self.constant_values   = dict()
                self.constant_names    = dict()
                self.functions         = list()
                self.functions_by_name = dict()
                self.closures          = list()
                self.load_effects      = list()

intern_and_bind("*UNIT-GLOBAL-DATA*", gvarp = t, globals = globals())

unit_global_data = cl.defwith("unit_global_data",
                              lambda self: cl.dynamic_scope_push({ _unit_global_data_: unit_global_data_t() }),
                              lambda *_:   cl.dynamic_scope_pop())
def get_unit_global_data():
        return cl.symbol_value(_unit_global_data_)

# Constant shovelling

def separate_nonimmediate_constants(mach, global_unit_data, prim):
        ## This expects being called with *GLOBAL-UNIT-DATA* bound.
        def maybe_separate_maybe_complex_constant(further, x):
                if (not mach.constantp(x)
                    or mach.literal_immediate_p(x)):
                        return further(x)
                ## Unregistered Issue TRIFIER-CONSTANT-NON-COALESCENCE-IS-WEAK
                tn = genname("CONST")
                global_unit_data.constants[tn] = x
                return tn
        p.map_primitives(maybe_separate_maybe_complex_constant, prim)

# Computing closures

## What kind of primitives factor into free variable computation?
## - name
## - function
## - let, let_
##
## What is borderline, but doesn't:
## - assign
## - progv, special_ref

intern_and_bind("*PRIMITIVE-FN*", gvarp = t, globals = globals())

def find_mark_and_register_closures(gdata, prim_lexenv, x):
        check_type(x, prim)
        def walk(further, x):
                fn = symbol_value(_primitive_fn_)
        ## Considerations:
        ##  - we need to compute the function at the root, regardless
        ##  - in case of a TLF, it's easy enough
        ##    - in case of a function, it's trivial
        ##    - in case of any other form, it's:
        ##      - no names are bound
        ##      - thunkify, register in gdata.load_effects
        ##  - in case of a non-NIL lexenv
        ##    - we have an incoming lexenv
        ##      - how do we even _know_ about it?
        ##        - the call chain:
        ##          - COMPILE-IN-LEXENV              :: form       -> lexenv -> function
        ##            - CODIFY-KNOWNS                :: known form -> lexenv -> linkable code
        ##              - MACH.CODIFY-PRIMITIVE-TREE :: primitive  -> lexenv -> linkable-code
        with progv({ _primitive_fn_: nil }):
                p.map_primitives(walk, x)

# CFG conversion

def cfg(mach, fn, bb, x):
        "Lower X into a CFG, in the context of the current basic block BB, for machine MACH."
        ## The generic recursor.
        if not isinstance(x, p.prim):
                error("In CFG: was asked to CFG-ify a non-primitive %s (of type %s).", x, type(x))
        if not fn:
                error("FN was not specified, while trifying %s.", x)
        if not bb:
                error("BB was not specified, while trifying %s.", x)
        if not mach.trifiablep(x):
                error("Not trifiable: %s.", x)
        return x.cfg(mach, fn, bb, *x.args, **x.keys)

# Machine definition

@defmachine
class cfg(machine):
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
                special_ref, special_setq
                }
        def globals(self): return globals()
        def literal_immediate_p(_, x):
                return (t   if isinstance(x, (p.integer, p.float_num))             else
                        nil if isinstance(x, (p.string, p.symbol, p.literal_list)) else
                        error("Objects of type %s cannot be represented as a constant value.",
                              type(x).__name__.upper()))
        def constantp(self, x):
                return (  (isinstance(x, (p.pylist, p.pytuple, p.pyset)) and all(self.constantp(x) for x in x))
                        or isinstance(x, p.literal))
        def immediatep(self, x):
                return isinstance(x, (p.integer, p.float_num, p.name))
        def trifiablep(self, x):
                return isinstance(x, p.prim) and hasattr(x, "cfg") and getattr(x, "cfg")
        def argumentp(self, x):
                ## This assumes that non-immediate literals have been filtered out.
                return isinstance(x, (p.literal, p.name))
        def codify_primitive_tree(self, prim, known_lexenv):
                ## A function context must be introduced here.
                ##
                ## We need to clarify, what contexts we're called in:
                ##  - PROCESS-TOP-LEVEL -- this is easy, either:
                ##    - a plain global, or
                ##    - a gensymmed global, shoveled into the _init section
                ##  - COMPILE-IN-LEXENV, and this is harder, as, in addition to above cases:
                ##    - might be a closure, if LEXENV is non-NIL, and free vars are employed
                ##      - which, as a prerequisite requires free var analysis
                ##        - ought to be done during primitivisation?
                ##      - currently can only happen for processing of MACROLET expander functions
                ##
                ## In any case, it becomes painfully obvious, that we need a straight story
                ## about closures at this point.  There does not seem to be any wiggle room about it.
                ##
                ## As a first step, we need a plan:
                ##  1. Detect and mark closures, separating them from non-closure anonymous functions
                ##     (which are convertible to simple gensymmed globals).
                ##     Q: Where should do the detection?  On what data structures?
                ##     A: Primitives, as we must operate on the final lambda structure.
                ##  2. Pass them to the machine layer.  Win.
                ##
                gdata  = unit_global_data_t()
                ## 1.
                separate_nonimmediate_constants(self, gdata, prim)
                ## 2.
                find_mark_and_register_closures(gdata, primitivise_lexenv(known_lexenv), prim)
                ## 3.
                root = bblock(unit = gdata)
                bb, val = cfg(mach, fn, root, p)
                return root

# Basic blockery
class bblock():
        __slots__ = ("unit", "insns", "label", "enters", "exits")
        def __init__(self, unit, insns = [], label = None, enters = [], exits = []):
                attrify_args(self, locals(),
                             "unit", "insns", "label", "enters", "exits")
        def append(self, x):  self.insns.append(x)
        def extend(self, xs): self.insns.extend(xs)

class function():
        __slots__ = ("unit", "parent", "children", "children_by_name", "name",
                     "arglist", "arglvars", "rest", "restlvar",
                     "return_type", "root")
        def __init__(self, unit, name, arglist, return_type = None, parent = None):
                rest, *fixed_args = arglist
                self.unit        = unit
                self.name        = name
                self.arglist     = fixed_args
                self.rest        = rest
                self.return_type = return_type
                self.root        = bblock(unit = unit)
                self.parent      = parent
                ##
                if parent:
                        parent.children.append(self)
                        parent.children_by_name[name] = self
                else: ## Global
                        unit.functions.append(self)
                        unit.functions_by_name[name] = self
                self.arglvars = [ lvar(lvar)
                                  for name in arglist ]
                self.restlvar = lvar(rest) if ret is not None else nil

# Tri-address code
class tri():       pass

class term():      pass

class var():
        def __init__(self, name, type):
                self.name, self.type = name, type

class lvar(var, tri):   pass
class gref(var):        pass
class const(gref, tri): pass
class gvar(gref, tri):  pass

class select(tri):      pass

class icmp(tri):        pass

class ret(tri, term):   pass
class br(tri, term):    pass

class add(tri):         pass
class sub(tri):         pass
class mul(tri):         pass
class sdiv(tri):        pass
class udiv(tri):        pass
class srem(tri):        pass
class urem(tri):        pass

class fadd(tri):        pass
class fsub(tri):        pass
class fmul(tri):        pass
class fdiv(tri):        pass
class frem(tri):        pass

class shl(tri):         pass
class ashr(tri):        pass
class lshr(tri):        pass
class and_(tri):        pass
class or_(tri):         pass
class xor(tri):         pass
class not_(tri):        pass

class lt(tri):          pass
class le(tri):          pass
class gt(tri):          pass
class ge(tri):          pass


def genlvar(x = "G"):
        return lvar(gensymname(x + "_"))

## CFG rules:
##
## 1. cfg() -> [tri], (or_t, name, (satisfies_t, mach.literal_immediate_p))
##
def cfg_reduce_binop(mach, fn, bb, lvar_prefix, op_ctor, zero, xs, min_args = 0):
        if len(xs) < min_args:
                error("Binary operation %s requires at least %d argument(s), but only %d were provided.", min_args, len(xs))
        if not xs:
                return bb, zero
        if len(xs) == 1:
                return cfg(mach, fn, bb, xs[0])
        vals = []
        for x in xs:
                bb, v = cfg(mach, fn, bb, x)
                vals.append(v)
        nacc = genlvar(lvar_prefix)
        bb.append(op_ctor(nacc, vals[0], vals[1]))
        for v in vals[2:]:
                bb.append(op_ctor(nacc, nacc, v))
        return bb, nacc

def cfg_binop(mach, fn, bb, lvar_prefix, ctor, left, right):
        nres = genlvar(lvar_prefix)
        bb, x = cfg(mach, fn, bb, left)
        bb, y = cfg(mach, fn, bb, right)
        bb.append(ctor(nres, x, y))
        return bb, nres

def cfg_unop(mach, fn, bb, lvar_prefix, ctor, arg):
        nres = genlvar(lvar_prefix)
        bb, x = cfg(mach, fn, bb, arg)
        bb.append(ctor(nres, x))
        return bb, nres

def let_cfg(mach, fn, bb, bindings, form):
        ## WARNING: We must rename the bindings.  It's a non-scoped LET*, otherwise.
        for name, value in bindings:
                bb, v = cfg(mach, fn, bb, value)
                bb.append(select(name, True, v, v))
        return cfg(mach, fn, bb, form)

def if_cfg(mach, fn, bb, test, cons, ante):
        bb, vtest = cfg(mach, fn, bb, test)
        nnilp, lcons, lante, nnil = genlvar("IF_TEST"), genlvar("IF_YES"), genlvar("IF_NO"), bb.unit.constant_names[nil]
        bb.extend([icmp(nnilp, vtest, nnil),
                   br(nnilp, lante, lcons)])
        ## BB is now done
        bbcons = bblock(unit = bb.unit, label = lcons, enters = [bb])
        bbante = bblock(unit = bb.unit, label = lante, enters = [bb])
        bb.exits = [bbcons, bbante]
        (bbcons, vcons), (bbante, vante) = cfg(mach, fn, bbcons, cons), cfg(mach, fn, bbante, ante)
        if bbcons and bbante:
                lvalue = genlvar("IF_RESULT")
                bbval = bblock(unit = bb.unit, label = lvalue, enters = [bbcons, bbante])
                bbcons.exits = bbante.exits = [bbval]
                nvalue = genlvar("IF_VAL")
                ## Strategy #1
                # bbcons.append(select(nvalue, True, vcons, vcons))
                # bbante.append(select(nvalue, True, vante, vante))
                # bbcons.append(br(lvalue))
                # bbante.append(br(lvalue))
                ## Strategy #2
                bbcons.append(br(lvalue))
                bbante.append(br(lvalue))
                ## BBCONS ans BBANTE are now done
                bbcons.append(select(nvalue, nnilp, vante, vcons))
                return bbval, nvalue
        if bbcons:
                return bbcons, vcons
        if bbante:
                return bbante, vante
        return None, None

# PRIMITIVE -> CFG
def defcfg(cls):
        prim_name = cls.__name__
        prim = getattr(p, prim_name, None)
        if not prim:
                error("Asked to define a CFG method for an unknown primitive: %s.", prim_name.upper())
        cfg = cls.__dict__.get("cfg", None)
        # if not cfg:
        #         error("A DEFCFG without a CFG method for primitive %s.", prim_name.upper())
        prim.cfg = cfg
        return globals().get(prim_name, None)

###
### Immediate values
###
## NAME INTEGER FLOAT-NUM
@defcfg
class name():
        def cfg(_, __, bb, x, writep = nil, globalp = nil):
                return bb, (gref if globalp else lvar)(x, None)

@defcfg
class integer:
        def cfg(_, __, bb, x):
                return bb, x

@defcfg
class float_num():
        def cfg(_, __, bb, x):
                return bb, x

###
### Operations
###
## + - * / MOD << >> LOGIOR LOGXOR LOGAND FLOOR EXPT
@defcfg
class add():
        def cfg(mach, fn, bb, *xs):  return cfg_reduce_binop(mach, fn, bb, "SUM", add, 0, xs)

@defcfg
class sub():
        def cfg(mach, fn, bb, *xs):  return cfg_reduce_binop(mach, fn, bb, "SUB", sub, 0, xs)

@defcfg
class mul():
        def cfg(mach, fn, bb, *xs):  return cfg_reduce_binop(mach, fn, bb, "MUL", mul, 1, xs)

@defcfg
class div():
        def cfg(mach, fn, bb, *xs):  return cfg_reduce_binop(mach, fn, bb, "DIV", sdiv, 0, xs, min_args = 1)

@defcfg
class mod():
        def cfg(mach, fn, bb, *xs):  return cfg_reduce_binop(mach, fn, bb, "REM", srem, 0, xs, min_args = 1)

@defcfg
class shl():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "SHL", shl, x, y)

@defcfg
class shr():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "ASHR", ashr, x, y)

@defcfg
class logior():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "OR", or_, x, y)

@defcfg
class logxor():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "XOR", xor, x, y)

@defcfg
class logand():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "AND", and_, x, y)

@defcfg
class lognot():
        def cfg(mach, fn, bb, x):    return cfg_unop(mach, fn, bb, "AND", not_, x)

@defcfg
class floor():
        ... # def cfg: call 'llvm.floor'

@defcfg
class expt():
        ... # def cfg: call 'llvm.floor'

## EQ < <= > >=

@defcfg
class eq():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "EQ", icmp, x, y)

@defcfg
class lt():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "LT", lt, x, y)

@defcfg
class le():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "LE", le, x, y)

@defcfg
class gt():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "GT", gt, x, y)

@defcfg
class ge():
        def cfg(mach, fn, bb, x, y): return cfg_binop(mach, fn, bb, "GE", ge, x, y)

## NOT -- the only logical op worth a primitive

@defcfg
class not_():
        def cfg(mach, fn, bb, x):
                bb, v = cfg(mach, fn, bb, x)
                nnil = bb.unit.constant_names[nil]
                nres = genlvar("NOT")
                bb.append(icmp(nres, nnil, v))
                return bb, nres
###
### Data structures
###
## CONS CAR CDR RPLACA RPLACD VECTOR INDEX
@defcfg
class cons():
        ...

@defcfg
class car():
        ...

@defcfg
class cdr():
        ...

@defcfg
class rplacd():
        ...

@defcfg
class rplaca():
        ...

@defcfg
class index():
        ...

###
### Control
###
## ASSIGN RETURN PROGN IF LOOP UNWIND-PROTECT CATCH THROW RESIGNAL
@defcfg
class assign():
        def cfg(mach, fn, bb, name, value):
                bb, v = cfg(mach, fn, bb, value)
                bb.append(select(name, 1, v, v))
                return bb, v

@defcfg
class return_():
        def cfg(mach, fn, bb, x):
                bb, v = cfg(mach, fn, bb, x)
                bb.append(ret(v, None))
                ## Unregistered Issue RETURN-VALUE-CFG-TRACKING
                return None, None

@defcfg
class progn():
        def cfg(mach, fn, bb, *body):
                for f in body[:-1]:
                        bb, _ = cfg(mach, fn, bb, f)
                return cfg(mach, fn, bb, body[-1])

@defcfg
class if_():
        def cfg(mach, fn, bb, test, cons, ante):
                return if_cfg(mach, fn, bb, test, cons, ante)

@defcfg
class loop():
        def cfg(mach, fn, bb, *body):
                lbegin = genlvar("LOOP_BEGIN")
                bb.append(br(lbegin))
                ## BB is now done
                loop_bb = bblock(unit = bb.unit, label = lbegin, enters = [bb])
                bb.exits = [loop_bb]
                bb = loop_bb
                for f in body:
                        bb, _ = cfg(mach, fn, bb, f)
                bb.append(br(lbegin))
                bb.exits = [loop_bb]
                loop_bb.enters.append(bb)

@defcfg
class unwind_protect():
        # def cfg: requires an unwind model
        ...

@defcfg
class catch():
        # def cfg: requires an unwind+exception model
        ...

@defcfg
class throw():
        # def cfg: requires an unwind+exception model
        ...

@defcfg
class resignal():
        # def cfg: requires an unwind+exception model
        ...

###
### Functions
###
## FUNCTION FUNCALL APPLY
@defcfg
class function():
        def cfg(mach, current_fn_or_noone, bb, name, arglist, body):
                maybe_rest, *fixed_args = args
                new_fn = function(bb.unit, name, args, maybe_rest, parent = current_fn_or_noone)
                cfg(mach, new_fn, new_fn.root, body)

@defcfg
class funcall():
        def cfg(mach, fn, bb, func, *args):
                avals = []
                bb, fval = cfg(mach, fn, bb, func)
                for a in args:
                        bb, v = cfg(mach, fn, bb, a)
                        avals.append(v)
                nret = genlvar("RETVAL")
                bb.append(call(nret, None, fval, *avals))
                return bb, nret

@defcfg
class apply():
        # def cfg: requires a calling convention.. can we arbitrarily tag call frame elements?
        ...

###
### Binding
###
## LET LET* PROGV
@defcfg
class let():
        def cfg(mach, fn, bb, bindings, expr):
                return let_cfg(mach, fn, bb, bindings, expr)

@defcfg
class let_():
        def cfg(mach, fn, bb, bindings, expr):
                return let_cfg(mach, fn, bb, bindings, expr)

@defcfg
class progv():
        ## def cfg: dynamic scope model
        ...

###
### Dynamic scope
###
## SPECIAL-REF SPECIAL-SETQ
@defcfg
class special_ref():
        ## def cfg: dynamic scope model
        ...

@defcfg
class special_setq():
        ## def cfg: dynamic scope model
        ...
