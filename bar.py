name = "name"

def namep(x):    return isinstance(x, int)
def atom(x):     return not isinstance(x, tuple)
def atomvarp(x): return x == "name"
def cut(n, xs):  return xs[0:n], xs[len(xs) if n is None else n:]
def position(x, xs):
        for i, ix in enumerate(xs):
                if x == ix: return i

def dict01(xs): return tuple(xs.items())[0][1]

## app-specific part
def prod(x : "expr") -> "result":                 return str(x)
def comb(x : "result", y : "result") -> "result": return prod(x) + prod(y)

## generic part
_error   = "error"
_replace = "replace"
def bind(value:"t", bound:dict, name:str, if_exists:{_error, _replace} = _error) -> dict:
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
def merg(d1:dict, d2:dict):
        cross = set(d1.keys()) & set(d2.keys())
        if cross: raise Exception("Binding conflict on names %s." % cross)
        r = dict(d1)
        r.update(d2)
        return r

## A large part of work is development of a calling convention.
## Multiple values, as a concept, is an important, but basic step
## in the general direction.

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
                
def succ(bound:dict, res:"result"):              return bound, res, None
def fail(bound:dict, exp:"expr", pat:"pattern"): return bound, exp, pat
def fcomb(fcomb:"marker", x:"expr", y:"expr"):   return fcomb, x, y
def test(test:bool, binds:dict, resf:"() -> result", exp:"expr", pat:"pattern", if_exists:{_error, _replace} = _error):
        return (succ(bind(exp, *binds, if_exists = if_exists), resf()) if test else
                fail(binds[0], exp, pat))
def equo(name:str, exp:"expr", x:("bound", "result/failexp", "fail")) -> ("bound", "result/failexp", "fail"):
        "Apply result binding, if any."
        b, r, f = x
        return ((bind(exp, b, name), r, f) if f is None else
                x) # propagate failure as-is
def coor(l0ret:("bound", "result/failexp", "fail"), lR:"() -> (b, r, f)"):
        l0b, l0r, l0f = l0ret
        if l0f is None: return succ(l0b, l0r)
        lRb, lRr, lRf = lR()
        if lRf is None: return succ(lRb, lRr)
        return fail(l0b, lRr, fcomb("<OR>", l0f, "<other segment variants>"))
def crec(l0res:("bound", "result/failexp", "fail"), lR:"() -> (b, r, f)"):
        l0b, l0r, l0f = l0res
        if l0f is not None: return fail(*l0res)
        lRres = lRb, lRr, lRf = lR(l0b)
        if lRf is not None: return fail(*lRres)
        return succ(lRb, comb(l0r, lRr))

def segment_match(binds, exp, pat, end = None, seg_patex = None):
        def constant_pat_p(pat):
                def nonconstant_pat_p(x): return atomvarp(x) or isinstance(x, (list, tuple))
                return not nonconstant_pat_p(dict01(pat) if isinstance(pat, dict) else
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
                                        match(rest_exp, rest_pat, seg_bound))
                if f is None:
                        return b, r, f
        seg_patex = (tuple(seg_pat) + (list(seg_pat),)) if seg_patex is None else seg_patex # We'll MATCH against this
        return coor(crec((lambda seg_b, seg_r, seg_f:
                                  test(seg_f is None, (seg_b, name), lambda: seg_r, seg_exp, seg_f,
                                       if_exists = _replace))
                         (*match(       seg_exp, seg_patex, bound,
                                        seg_patex = seg_patex)), # Reuse cache!
                         lambda seg_bound:
                                 match(rest_exp, rest_pat,  seg_bound)),
                    lambda: segment_match(binds, exp, pat, end = (end or 0) + 1,
                                          seg_patex = seg_patex)) # Reuse cache!

## About the vzy33c0's idea:
## type-driven variable naming is not good enough, because:
## 1. type narrows down the case analysis chain (of which there is a lot)
## 2. expressions also need typing..
def match(exp, pat, bound = None, seg_patex = None):
        bound = dict() if bound is None else bound
        def error_bad_pattern(pat): raise Exception("Bad pattern: %s." % (pat,))
        def getname(pat):           return ((None, pat) if not isinstance(pat, dict) else
                                            ((len(pat) == 1 and tuple(pat.items())[0]) or
                                             error_bad_pattern(pat)))
        name, pat = getname(pat)
        binds = (bound, name)
        return \
            (error_bad_pattern(pat)                               if isinstance(pat, list)  else
             test(namep(exp), binds, lambda: prod(exp), exp, pat) if atom(pat)              else # pat tuple, exp t
             test(exp == (),  binds, lambda: prod(exp), exp, pat) if pat == ()              else # pat tupleful, exp t
             fail(bound, exp, pat)                                if atom(exp)              else # exp tuple, pat tupleful
             (lambda pat0name, pat0:
                      (equo(name, exp,                                                   # pat   leadsed tupleful, exp tuple
                            segment_match((bound, pat0name), exp, (pat0,) + pat[1:],
                                          seg_patex)) # pass cache through
                                                          if isinstance(pat0, list) else # pat noleadseg tupleful, exp tuple
                       fail(bound, exp, pat)              if exp == ()              else # pat and exp are tuplefuls
                       equo(name, exp,
                            crec(              match(exp[0],  pat[0],  bound),
                                 lambda b0und: match(exp[1:], pat[1:], b0und)))))
             (*getname(pat[0])))

###
### testing
###
def test_mid_complex():
        pat = ({"headname":name},
                  {"headtupname":(name,)},
                           {"varitupseq":[(name, [name])]},
                                                    {"fix1tupseq":[(name,)]},
                                                                           {"nameseq":[name]},
                                                                                {"tailname":name})
        exp =             (1,    (1,),   (1,), (1, 1), (1, 1, 1), (1,), (1,), (1,),   1, 1, 1)
        b, r, f = match(exp, pat)
        return (f is None and
                b == {'headname': 1,
                      'headtupname': (1,),
                      'varitupseq': ((1,), (1, 1), (1, 1, 1)),
                      'fix1tupseq': ((1,), (1,), (1,)),
                      'nameseq': (1, 1),
                      'tailname': 1 })

print("\ncompiled")
assert(test_mid_complex())


