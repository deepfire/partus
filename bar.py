name = "name"

## attic?
def merg(d1:dict, d2:dict):
        cross = set(d1.keys()) & set(d2.keys())
        if cross: raise Exception("Binding conflict on names %s." % cross)
        r = dict(d1)
        r.update(d2)
        return r

## debug
depth = 0
def deeper(f):
        global depth
        try:
                depth += 1
                return f()
        finally:
                depth -= 1
def dprint(ctl, *args):
        print(" " * (depth * 2) + (ctl % args))

## utility part
def identity(x):    return x
def namep(x):       return isinstance(x, int)
def atom(x):        return not isinstance(x, tuple)
def atomvarp(x):    return x == "name"
def cut(n, xs):     return xs[0:n], xs[len(xs) if n is None else n:]
def position(x, xs):
        for i, ix in enumerate(xs):
                if x == ix: return i
def undict_val(xs): return tuple(xs.items())[0][1]

## app-specific part
def prod(x):
        return str(x) if x else ""
def comb(x, y, leader):
        bs, be = ("(", ")") if leader else ("", "")
        py = prod(y)
        return bs + prod(x) + ((" " + py) if py else "") + be
def preprocess(pat):
        "Expand syntactic sugar."
        def prep_binding(b):
                k, v = tuple(b.items())[0]
                return {k: preprocess(v)}
        return ((("some",) + preprocess(tuple(pat))) if isinstance(pat, list)                else
                prep_binding(pat)                    if isinstance(pat, dict)                else
                pat                                  if not (pat and isinstance(pat, tuple)) else
                (preprocess(pat[0]),) + preprocess(pat[1:]))
def match_atom(exp, pat):
        return namep(exp)
## A large part of work is development of a calling convention.
## Multiple values, as a concept, is an important, but basic step
## in the general direction.
                
## generic part
_error   = "error"
_replace = "replace"
def bind(value, bound, name, if_exists:{_error, _replace} = _error) -> dict:
        def error_bound(x):
                raise Exception("Rebinding %s from %s to %s." % (repr(name), repr(x), repr(value)))
        def error_keyword(name, bad, allowed):
                raise Exception("Keyword %s must be one of: %s.  Was: %s." % (name, allowed, bad))
        if   name is None:            return bound
        elif (name not in bound or
              if_exists is _replace):
                bound = dict(bound)
                bound.update({name:value}); return bound
        elif if_exists is _error:     error_bound(bound[name])
        else:                         error_keyword("if_exists", if_exists, { _error, _replace })
def succ(bound, res):              return bound, res, None
def fail(bound, exp, pat): return bound, exp, pat
def fcomb(fcomb, x, y):    return fcomb, x, y
def test(test, binds, resf:"() -> result", exp, pat, if_exists:{_error, _replace} = _error):
        return (succ(bind(exp, *binds, if_exists = if_exists), resf()) if test else
                fail(binds[0], exp, pat))
def equo(name, exp, x):
        "Apply result binding, if any."
        b, r, f = x
        return ((bind(exp, b, name), r, f) if f is None else
                x) # propagate failure as-is
def coor(l0ret, lR):
        l0b, l0r, l0f = l0ret
        if l0f is None: return succ(l0b, l0r)
        lRb, lRr, lRf = lR()
        if lRf is None: return succ(lRb, lRr)
        return fail(l0b, lRr, fcomb("<OR>", l0f, "<other segment variants>"))
def crec(l0res, lR, leader = False):
        l0b, l0r, l0f = l0res
        if l0f is not None: return fail(*l0res)
        lRres = lRb, lRr, lRf = lR(l0b)
        if lRf is not None: return fail(*lRres)
        return succ(lRb, comb(l0r, lRr, leader))

def complex_pat_p(x):
        return x and isinstance(x[0], str) and x[0] in { "some" }
def match_complex(binds, exp, pat, aux = None):
        complex, *rest = pat
        if complex[0] == "some":
                raise Exception("Not implemented.")

def segment_match(binds, exp, pat, end = None, aux = None):
        def constant_pat_p(pat):
                def nonconstant_pat_p(x): return atomvarp(x) or isinstance(x, (list, tuple))
                return not nonconstant_pat_p(undict_val(pat) if isinstance(pat, dict) else
                                             pat)
        bound, name = binds
        [*seg_pat], *rest_pat = pat # ensure that it destructures well
        seg_pat, rest_pat = tuple(seg_pat), tuple(rest_pat)
        end = (end                        if end is not None                          else
               position(rest_pat[0], exp) if rest_pat and constant_pat_p(rest_pat[0]) else
               0)
        if ((end and end > len(exp)) or ## no boundary variant fitted
            end is None):               ## a constant pattern was missing
                return fail(bound, exp, pat)
        seg_exp, rest_exp = (cut(end, exp) if rest_pat else
                             (exp, ()))
        if not seg_exp:
                b, r, f =  crec(succ(bind((), *binds), ## this binding is actualised by outer invocations, if any
                                     prod(seg_exp)),   ## ..same goes for the result.
                                lambda seg_bound:
                                        _match(rest_exp, rest_pat, seg_bound, None, False))
                if f is None:
                        return b, r, f
        aux = (tuple(seg_pat) + (list(seg_pat),)) if aux is None else aux # We'll MATCH against this
        return coor(crec((lambda seg_b, seg_r, seg_f:
                                  test(seg_f is None, (seg_b, name), lambda: seg_r, seg_exp, seg_f,
                                       if_exists = _replace))
                         (*_match(       seg_exp, aux, bound, aux, False)),
                         lambda seg_bound:
                                 _match(rest_exp, rest_pat,  seg_bound, None, False)),
                    lambda: segment_match(binds, exp, pat, end = (end or 0) + 1,
                                          aux = aux)) # Reuse cache!

## About the vzy33c0's idea:
## type-driven variable naming is not good enough, because:
## 1. type narrows down the case analysis chain (of which there is a lot)
## 2. expressions also need typing..
def _match(exp, pat, bound, aux, leader):
        def error_bad_pattern(pat): raise Exception("Bad pattern: %s." % (pat,))
        def maybe_getname(pat):     return ((None, pat)           if not isinstance(pat, dict) else
                                            tuple(pat.items())[0] if len(pat) == 1             else
                                            error_bad_pattern(pat))
        def maybe_get0name(pat):
                name, value = maybe_getname(pat[0])
                return name, value, (((value,) + pat[1:]) if name is not None else
                                     pat) ## Attempt to avoid consing..
        name, pat = maybe_getname(pat)
        binds = (bound, name)
        ## I just caught myself feeling so comfortable thinking about life matters,
        ## while staring at a screenful of code.  In "real" life I'd be pressed by
        ## the acute sense of time being wasted..
        return \
            (error_bad_pattern(pat)                                         if isinstance(pat, list) else
             test(match_atom(exp, pat), binds, lambda: prod(exp), exp, pat) if atom(pat) else # pat tuple,    exp t
             fail(bound, exp, pat)                                          if atom(exp) else # pat tuple,    exp tuple
             test(exp == (),            binds, lambda: prod(exp), exp, pat) if pat == () else # pat tupleful, exp tuple
             (lambda pat0name, pat0, clean_pat:
                      (equo(name, exp,                                                   # pat   leadseg tupleful, exp tuple
                            segment_match((bound, pat0name), exp, clean_pat, aux))
                                                          if isinstance(pat0, list) else # pat noleadseg tupleful, exp tuple
                       # equo(name, exp,
                       #      match_complex((bound, pat0name), exp, pat, aux))
                       #                                    if complex_pat_p(pat)     else
                       fail(bound, exp, pat)              if exp == ()              else # pat and exp are tuplefuls
                       equo(name, exp,
                            crec(               _match(exp[0],  pat[0],  bound, None, True),
                                 (lambda b0und: _match(exp[1:], pat[1:], b0und, None, False)), 
                                 leader = leader))))
             (*maybe_get0name(pat)))

def match(exp, pat):
        return _match(exp, identity(pat), dict(), None, True)

print("\n; compiled and loaded.")
###
### app
###

###
### testing
###
def run_mid_complex():
        pat = ({"headname":name},
                  {"headtupname":(name,)},
                           {"varitupseq":[(name, [name])]},
                                                    {"fix1tupseq":[(name,)]},
                                                                           {"nameseq":[name]},
                                                                                {"tailname":name})
        exp =             (1,    (1,),   (1,), (1, 1), (1, 1, 1), (1,), (1,), (1,),   1, 1, 1)
        return match(exp, pat)
def test_mid_complex():
        b, r, f = run_mid_complex()
        return (f is None,
                b == { 'headname': 1,
                       'headtupname': (1,),
                       'varitupseq': ((1,), (1, 1), (1, 1, 1)),
                       'fix1tupseq': ((1,), (1,), (1,)),
                       'nameseq': (1, 1),
                       'tailname': 1 })

nofail, bound_good = test_mid_complex()
assert(nofail)
assert(bound_good)
print("; MID-COMPLEX: passed")
