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
def integerp(x):    return isinstance(x, int)
def listp(x):       return isinstance(x, tuple)
def atom(x):        return not isinstance(x, tuple)
def cut(n, xs):     return xs[0:n], xs[len(xs) if n is None else n:]
def position(x, xs):
        for i, ix in enumerate(xs):
                if x == ix: return i
def undict_val(xs): return tuple(xs.items())[0][1]

## A large part of work is development of a calling convention.
## Multiple values, as a concept, is an important, but basic step
## in the general direction.
                
## generic part
some     = "some"
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
def error_bad_pattern(pat):
        raise Exception("Bad pattern: %s." % (pat,))
def maybe_destructure_binding(pat):
        return ((None, pat)           if not isinstance(pat, dict) else
                tuple(pat.items())[0] if len(pat) == 1             else
                error_bad_pattern(pat))
def succ(bound, res):      return bound, res, None
def post(x, mutator):      return (x[0], mutator(x[1]), None) if x[2] is None else x
def fail(bound, exp, pat): return bound, exp, pat
def test(test, bound, name, resf:"() -> result", exp, fail_pat, if_exists:{_error, _replace} = _error):
        return (succ(bind(exp, bound, name, if_exists = if_exists), resf()) if test else
                fail(bound, exp, fail_pat))
def equo(name, exp, x):
        "Apply result binding, if any."
        b, r, f = x
        return ((bind(exp, b, name), r, f) if f is None else
                x) # propagate failure as-is
def coor(l0ret, lR, leader = False):
        l0b, l0r, l0f = l0ret
        if l0f is None: return succ(l0b, l0r)
        lRb, lRr, lRf = lR()
        if lRf is None: return succ(lRb, lRr)
        return fail(l0b, lRr, forc(l0f, lRf, leader))
def crec(l0, lR, leader = False):
        ## Unregistered Issue PYTHON-LACK-OF-RETURN-FROM
        failpat, failex, bound0, boundR = None, None, None, None
        def try_produce_0():
                nonlocal bound0, failex, failpat
                bound0, failex, failpat = l0()
                if failpat is None: return failex
        def try_produce_R():
                nonlocal boundR, failex, failpat
                boundR, failex, failpat = lR(bound0)
                if failpat is None: return failex
        result = comb(try_produce_0, try_produce_R, leader)
        return (succ(boundR, result) if failpat is None else
                fail(boundR or bound0, failex, failpat))

__complex_patterns__ = dict()
def register_complex_matcher(name, matcher):
        __complex_patterns__[name] = matcher
def complex_pat_p(x):
        return x and isinstance(x[0], str) and x[0] in __complex_patterns__
def match_complex(bound, name, exp, pat, leader, aux):
        return __complex_patterns__[pat[0][0]](bound, name, exp, pat, leader, aux)
def matcher_not_implemented(bound, name, exp, pat, leader, aux):
        raise Exception("Not yet capable of matching complex patterns of type %s.", pat[0][0])

def segment_match(bound, name, exp, pat, leader, aux, end = None):
        def constant_pat_p(pat):
                def nonconstant_pat_p(x): return listp(x) or nonliteral_atom_p(x)
                return not nonconstant_pat_p(undict_val(pat) if isinstance(pat, dict) else
                                             pat)
        ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
        seg_pat, rest_pat = pat[0][1:], pat[1:]
        end = (end                        if end is not None                          else
               position(rest_pat[0], exp) if rest_pat and constant_pat_p(rest_pat[0]) else
               0)
        if ((end and end > len(exp)) or ## no boundary variant fitted
            end is None):               ## a constant pattern was missing
                return fail(bound, exp, pat)
        seg_exp, rest_exp = (cut(end, exp) if rest_pat else
                             (exp, ()))
        aux = (seg_pat + ((some,) + seg_pat,)) if aux is None else aux # We'll MATCH against this
        return coor(crec(lambda:
                                 ((lambda seg_bound, seg_r, seg_fail_pat:
                                           test(seg_fail_pat is None, seg_bound, name, (lambda: seg_r), seg_exp, seg_fail_pat,
                                                if_exists = _replace))
                                  (*(succ(bind((), bound, name), prod((), False)) if seg_exp == () else
                                     _match(bound, name,  seg_exp,       aux,  False,  aux)))),
                         lambda seg_bound:
                                 _match(seg_bound, None, rest_exp,  rest_pat,  False, None),
                         leader = leader),
                    lambda: segment_match(  bound, name,      exp,       pat, leader,  aux, end = (end or 0) + 1),
                    leader = leader)

register_complex_matcher(some, segment_match)

## About the vzy33c0's idea:
## type-driven variable naming is not good enough, because:
## 1. type narrows down the case analysis chain (of which there is a lot)
## 2. expressions also need typing..
def _match(bound, name, exp, pat, leader, aux):
        def maybe_get0Rname(pat):
                ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                (name, pat0), patR = maybe_destructure_binding(pat[0]), pat[1:]
                return name, pat0, patR, (((pat0,) + patR) if name is not None else
                                                 pat) ## Attempt to avoid consing..
        ## I just caught myself feeling so comfortable thinking about life matters,
        ## while staring at a screenful of code.  In "real" life I'd be pressed by
        ## the acute sense of time being wasted..
        atomp, null = atom(pat), pat == ()
        return \
            (test((match_atom(exp, pat) if atomp else
                   exp == ()),
                  bound, name, lambda: prod(exp, leader), exp, pat) if atomp or null else
             (lambda pat0name, pat0, patR, clean_pat:
                      (equo(name, exp,
                            match_complex(bound, pat0name, exp, clean_pat, leader, aux))
                                                 if complex_pat_p(pat0)    else
                       fail(bound, exp, pat)     if atom(exp) or exp == () else      # pat tupleful, exp tupleful
                       equo(name, exp,
                            crec(lambda:        _match(bound, pat0name, exp[0],  pat0, True,  None),
                                 (lambda b0und: _match(b0und, None,     exp[1:], patR, False, None)),
                                 leader = leader))))
             (*maybe_get0Rname(pat)))

def match(exp, pat):
        name, prepped = maybe_destructure_binding(preprocess(pat))
        return _match(dict(), name, exp, prepped, True, None)

print("\n; compiled and loaded.")
###
### app
###
newline = "newline"
indent  = "indent"
form    = "form"
def forc(x, y, leader):
        return (("OR",) if leader else ()) + (x,) + y
def preprocess(pat):
        "Expand syntactic sugar."
        def prep_binding(b):
                k, v = tuple(b.items())[0]
                return {k: preprocess(v)}
        return (((some,) + preprocess(tuple(pat))) if isinstance(pat, list)                else
                prep_binding(pat)                  if isinstance(pat, dict)                else
                (form,)                            if pat == form                          else
                (newline, 0)                       if pat == "\n"                          else
                (newline, pat)                     if integerp(pat)                        else
                (indent, 1)                        if pat == " "                           else
                pat                                if not (pat and isinstance(pat, tuple)) else
                (preprocess(pat[0]),) + preprocess(pat[1:]))
def nonliteral_atom_p(x):
        return x == "name"
def match_atom(exp, pat):
        return isinstance(exp, int)
pp_base_depth = 0
pp_depth = 0
def prod(x, leader):
        return str(x) if x or leader else ""
def comb(f0, fR, leader):
        global pp_base_depth, pp_depth
        acc = "(" if leader else ""
        base_depth_save = pp_base_depth
        try:
                if leader:
                        pp_base_depth = pp_base_depth + pp_depth + 1
                res0 = f0()
                if res0 is None: return
                acc += res0
                try:
                        pp_depth += len(res0)
                        resR = fR()
                        if resR is None: return
                        acc += resR
                        acc += ")" if leader else ""
                finally:
                        pp_depth -= len(res0)
        finally:
                pp_base_depth = base_depth_save
        return acc
def process_newline(bound, name, exp, pat, leader, aux):
        global pp_base_depth, pp_depth
        n, tail = pat[0], pat[1:]
        try:
                pp_base_depth += n
                pp_depth = 0
                return post(_match(bind(pp_base_depth, bound, name), None, exp, tail, leader, aux),
                            lambda r: "\n" + (" " * pp_base_depth) + r)
        finally:
                pp_base_depth -= n
def process_indent(bound, name, exp, pat, leader, aux):
        global pp_base_depth, pp_depth
        n, tail = pat[0], pat[1:]
        try:
                pp_depth += n
                return post(_match(bind(pp_depth, bound, name), None, exp, tail, leader, aux),
                            lambda r: (" " * n) + r)
        finally:
                pp_depth -= n

register_complex_matcher(newline, process_newline)
register_complex_matcher(indent,  process_indent)
register_complex_matcher(form,    matcher_not_implemented)

###
### testing
###
_results = []
def runtest(fun, bindings, result):
        b, r, f = fun()
        _results.append((fun, b, r, f))
        return (b == bindings,
                r == result,
                f is None)
def results():
        for fun, b, r, f in _results:
                dprint("%15s bound: %s", fun.__name__, b)
                dprint("%15s res: %s", fun.__name__, r)
def empty():
        return match((), {"whole":()})
bound_good, result_good, nofail = runtest(empty,
                                          { 'whole': () },
                                          "()")
assert(nofail)
assert(bound_good)
assert(result_good)
print("; RUN-EMPTY: passed")

def empty_cross():
        return match((), ({"a":[name]}, {"b":[(name,)]},))
bound_good, result_good, nofail = runtest(empty_cross,
                                          { 'a': (), 'b': () },
                                          "()")
assert(nofail)
assert(bound_good)
assert(result_good)
print("; RUN-EMPTY-CROSS: passed")

def mid_complex():
        pat = ({"headname":name},
                  {"headtupname":(name,)},
                           {"varitupseq":[(name, [name])]},
                                                    {"fix1tupseq":[(name,)]},
                                                                           {"nameseq":[name]},
                                                                                {"tailname":name})
        exp =             (1,    (1,),   (1,), (1, 1), (1, 1, 1), (1,), (1,), (1,),   1, 1, 1)
        return match(exp, pat)
bound_good, result_good, nofail = runtest(mid_complex,
                                          { 'headname': 1,
                                            'headtupname': (1,),
                                            'varitupseq': ((1,), (1, 1), (1, 1, 1)),
                                            'fix1tupseq': ((1,), (1,), (1,)),
                                            'nameseq': (1, 1),
                                            'tailname': 1 },
                                          "(1 (1) (1) (1 1) (1 1 1) (1) (1) (1) 1 1 1)")
assert(nofail)
assert(bound_good)
assert(result_good)
print("; MID-COMPLEX: passed")
