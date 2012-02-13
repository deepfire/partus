name = "name"

def namep(x):   return isinstance(x, int)
def atom(x):    return not isinstance(x, tuple)
def cut(n, xs): return xs[0:n], xs[n:]
def position(x, xs):
        for i, ix in enumerate(xs):
                if x == ix: return i

## app-specific part
def prod(x : "expr") -> result:                    return str(x)
def comb(x : "result", y -> "result") -> "result": return prod(x) + prod(y)

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
              if_exists is _replace): bound.update({name:value}); return bound
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
                
def succ(bound:dict, res:"result"):              return bound, res, None
def fail(bound:dict, exp:"expr", pat:"pattern"): return bound, exp, pat
def fcomb(fcomb:"marker", x:"expr", y:"expr"):   return fcomb, x, y
def test(test:bool, binds:dict, exp:"expr", pat:"pattern", if_exists:{_error, _replace} = _error):
        return (succ(bind(exp, *binds, if_exists = if_exists), exp) if test else
                fail(binds[0], exp, pat))
def equo(name:str, exp:"expr", x:tuple):
        "Apply result binding."
        b, r, f = x
        return ((bind(exp, b, name), r, f) if f is None else
                x) # propagate failure
def coor(l0ret, lR):
        l0b, l0r, l0f = l0ret
        if l0f is None: return succ(l0b, l0r)
        lRb, lRr, lRf = lR()
        if lRf is None: return succ(lRb, lRr)
        return fail(l0b, lRr, fcomb("<OR>", l0f, "<other segment variants>"))
def crec(l0res, lR):
        l0b, l0r, l0f = l0res
        if l0f is not None: return fail(*l0res)
        lRres = lRb, lRr, lRf = lR(l0b)
        if lRf is not None: return fail(*lRres)
        return succ(lRb, comb(l0r, lRr))

################ The question is -- where do we want the binding request to be interpreted?
def segment_match(binds, exp, pat, start = None):
        # print("segment_match(%s, %s, start = %s)" % (pat, exp, start))
        def constant_pat_p(pat):        return not isinstance(pat, (list, tuple))
        def boundary_minimum(exp, pat): return position(pat, exp) if constant_pat_p(pat) else 0
        bound, name = binds
        if not exp:
                return succ(bound, exp)
        else:
                [*seg_pat], *rest_pat = pat # ensure that it destructures well
                seg_pat, rest_pat = tuple(seg_pat), tuple(rest_pat)
                boundary = boundary_minimum(exp, rest_pat[0]) if start is None and rest_pat else start
                if boundary and boundary >= len(exp):
                        return fail(bound, exp, pat)
                seg_exp, rest_exp = (cut(boundary, exp) if rest_pat else
                                     (exp, ()))
                return coor(crec((lambda seg_b, seg_r, seg_f:
                                          test(seg_f is None, (seg_b, name), seg_r, seg_f,
                                               if_exists = _replace))
                                 (*match(seg_exp,  tuple(seg_pat) + (list(seg_pat),), bound)),
                                 lambda seg_bound:
                                         match(rest_exp, rest_pat, seg_bound)),
                            lambda: segment_match(binds, exp, pat, start = (boundary or 0) + 1))

def match(exp, pat, bound = dict()):
        def error_bad_pattern(pat): raise Exception("Bad pattern: %s." % (pat,))
        def dict0(x):               return tuple(x.items())[0]
        def headpat(x):             return dict0(x[0])[1] if isinstance(x[0], dict) else x[0]
        name = None
        if isinstance(pat, dict):
                if len(pat) != 1: error_bad_pattern(pat)
                name, pat = dict0(pat)
                print("match: extracted name %s, pat %s" % (repr(name), repr(pat)))
        print("match(%s, %s)" % (pat, exp))
        return \
            (error_bad_pattern(pat)                       if isinstance(pat, list)    else
             test(namep(exp), (bound, name), exp, pat)    if atom(pat)                else # pat tuple
             test(exp == (),  (bound, name), exp, pat)    if pat == ()                else # pat tupleful
             fail(bound, exp, pat)                        if atom(exp)                else # exp tuple
             equo(name, exp,
                  segment_match((bound, name), exp, pat)) if (isinstance(pat[0], list) or
                                                              (isinstance(pat[0], dict) and
                                                               isinstance(dict0(pat[0]), list))) else # pat tupleful, no leadseg
             fail(bound, exp, pat)                        if exp == ()                else # pat and exp are tuplefuls
             equo(name, exp,
                  crec(              match(exp[0],  pat[0],  bound),
                       lambda b0und: match(exp[1:], pat[1:], b0und))))

print("\ncompiled")
pat = (name, (name,), {"yay": [(name, [name])]},        [(name,)],  [name], name)
seq = (1,    (1,),    (1,), (1, 1), (1, 1, 1), (1,), (1,), (1,),   1)
def m(m, v): return v #print("%s: %s" % (m, v)); return v
