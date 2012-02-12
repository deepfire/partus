name = "name"
pat = (name, (name,), [(name, [name])],        [(name,)],  [name], name)
seq = (1,    (1,),    (1,), (1, 1), (1, 1, 1), (1,), (1,), (1,),   1)

def namep(x):   return isinstance(x, int)
def atom(x):    return not isinstance(x, tuple)
def cut(n, xs): return xs[0:n], xs[n:]
def position(x, xs):
        for i, ix in enumerate(xs):
                if x == ix: return i

def bind(value, bound, name):
        if name is not None:
                if name in bound:
                        raise Exception("Rebinding %s from %s to %s." % (repr(name), repr(bound[name]), repr(value)))
                bound.update({name:value})
        return bound
def merg(d1, d2):
        if set(d1.keys()) & set(d2.keys()):
                raise Exception("Binding conflict on names %s." % (set(d1.keys()) & set(d2.keys()),))
        r = dict(d1)
        r.update(d2)
        return r
                
def succ(bound, ret):            return bound, ret, None
def fail(bound, exp, pat):       return bound, exp, pat
def test(test, binds, exp, pat): return (succ(bind(exp, *binds), exp) if test else
                                         fail(binds[0], exp, pat))
def equo(x):        b, r, f = x; return b, r, f
def comb(x, y):           return str(x) + str(y)
def fcomb(fcomb, x, y):   return (fcomb, x, y)
def coor(l0, lR):
        l0b, l0r, l0f = l0()
        if l0f is None: return succ(l0b, l0r)
        lRb, lRr, lRf = lR()
        if lRf is None: return succ(lRb, lRr)
        return fail(l0b, lRr, fcomb("<OR>", l0f, "<other segment variants>"))

def segment_match(binds, exp, pat, start = None):
        # print("segment_match(%s, %s, start = %s)" % (pat, exp, start))
        def constant_pat_p(pat):        return not isinstance(pat, (list, tuple))
        def boundary_minimum(exp, pat): return position(pat, exp) if constant_pat_p(pat) else 0
        bound, name = binds
        if not exp:
                return succ(bind(???, *bound), exp)
        else:
                [*seg_pat], *rest_pat = pat # ensure that it destructures well
                seg_pat, rest_pat = tuple(seg_pat), tuple(rest_pat)
                boundary = boundary_minimum(exp, rest_pat[0]) if start is None and rest_pat else start
                if boundary and boundary >= len(exp):
                        return fail(exp, pat)
                seg_exp, rest_exp = (cut(boundary, exp) if rest_pat else 
                                     (exp, ()))
                return coor(lambda: c1n2(binds,
                                         lambda _:     match(seg_exp,  tuple(seg_pat) + (list(seg_pat),), bound),
                                         lambda bound: match(rest_exp, rest_pat,                          bound)),
                            lambda: equo(segment_match(binds, exp, pat, start = (boundary or 0) + 1)))
def cand(binds, l0, lR):
        bound, name = binds
        l0b, l0r, l0f = l0(bound)
        if l0f is not None: return fail(l0b, l0r, l0f)
        lRb, lRr, lRf = lR(l0b)
        if lRf is not None: return fail(lRb, lRr, lRf)
        return succ(lRb, comb(l0r, lRr))
def c1n2(binds, l0, lR):
        l0b, l0r, l0f = l0(binds[0])

def match(exp, pat, bound = dict()):
        def error_bad_pattern(pat): raise Exception("Bad pattern: %s.", pat)
        name = None
        if isinstance(pat, dict):
                if len(pat) != 1: error_bad_pattern(pat)
                name, pat = pat.items()[0]
        # print("match(%s, %s)" % (pat, exp))
        return \
            (error_bad_pattern(pat)                       if isinstance(pat, list)    else
             test(namep(exp), (bound, name), exp, pat)    if atom(pat)                else # pat tuple
             test(exp == (),  (bound, name), exp, pat)    if pat == ()                else # pat tupleful
             fail(exp, pat)                               if atom(exp)                else # exp tuple
             equo(segment_match((bound, name), exp, pat)) if isinstance(pat[0], list) else # pat tupleful, no leadseg
             fail(exp, pat)                               if exp == ()                else # pat and exp are tuplefuls
             cand((bound, name),
                  lambda _:     match(exp[0],  pat[0],  bound),
                  lambda bound: match(exp[1:], pat[1:], bound)))

print("\ncompiled")
def m(m, v): return v #print("%s: %s" % (m, v)); return v
