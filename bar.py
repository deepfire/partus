import sys as _sys

def _debug_printf(format_control, *format_args):
        print((format_control % format_args + "\n"), file = _sys.stderr)

## utility part
def identity(x):    return x
def integerp(x):    return isinstance(x, int)
def listp(x):       return isinstance(x, tuple)
def atom(x):        return not isinstance(x, tuple)
def position(x, xs):
        for i, ix in enumerate(xs):
                if x == ix: return i
def typep(x, type): return isinstance(x, type)
def length(x):      return len(x)

def _tuplep(x):     return isinstance(x, tuple)

def _python_builtins_dictionary():
        import builtins    as _builtins
        return _builtins.getattr(__builtins__, "__dict__", __builtins__)

import collections as _collections

class _dictator(_collections.UserDict):
        def __hasattr__(self, name): return name in self.data
        def __getattr__(self, name): return self.data[name]
        def __setitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __delitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __setattr__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __init__(self, dict):
                self.__dict__.update(data = dict)

_py = _dictator(_python_builtins_dictionary())

## A large part of work is development of a calling convention.
## Multiple values, as a concept, is an important, but basic step
## in the general direction.

## generic part
some      = "some"
error     = "error"
replace   = "replace"
_name     = "name"
_maybe    = "maybe"
def _error_bad_pattern(pat):
        raise Exception("Bad pattern: %s." % (pat,))
def _maybe_destructure_binding(pat):
        return ((None, pat)           if not typep(pat, _py.dict) else
                tuple(pat.items())[0] if length(pat) == 1         else
                error_bad_pattern(pat))
def bind(value, bound, name, if_exists:{error, replace} = error) -> dict:
        def error_bound(x):
                raise Exception("Rebinding %s from %s to %s." % (_py.repr(name), _py.repr(x), _py.repr(value)))
        def error_keyword(name, bad, allowed):
                raise Exception("Keyword %s must be one of: %s.  Was: %s." % (_py.repr(name), allowed, _py.repr(bad)))
        if   name is None:            return bound
        elif (name not in bound or
              if_exists is replace):
                bound = _py.dict(bound)
                bound.update({name:value}); return bound
        elif if_exists is error:     error_bound(bound[name])
        else:                        error_keyword("if_exists", if_exists, { error, replace })
def succ(bound, res):      return bound, res, None
def fail(bound, exp, pat): return bound, exp, pat
def post(x, mutator):      return (x[0], mutator(x[1]), None) if x[2] is None else x
def test(test, bound, name, resf:"() -> result", exp, fail_pat, if_exists:{error, replace} = error):
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

def _matcher_init():
        global __complex_patterns__
        __complex_patterns__ = dict()
        register_complex_matcher(some, segment_match)
        register_complex_matcher(_maybe, match_maybe)

def register_complex_matcher(name, matcher):
        __complex_patterns__[name] = matcher
def complex_pat_p(x):
        return x and isinstance(x[0], str) and x[0] in __complex_patterns__
def match_complex(bound, name, exp, pat, leader, aux, limit):
        # _debug_printf("match_complex  %20s  %10s  %s  %s  %s  %s  %s", bound, name, exp, pat, leader, aux, limit)
        return __complex_patterns__[pat[0][0]](bound, name, exp, pat, leader, aux, limit)
def identity_matcher(bound, name, exp, pat, leader, aux, limit):
        ## This should dispatch over simplicity.
        return match_complex(bound, name, exp, pat[0][1:] + pat[1:], leader, aux, limit)
def matcher_not_implemented(bound, name, exp, pat, leader, aux):
        raise Exception("Not yet capable of matching complex patterns of type %s.", pat[0][0])
###
def segment_match(bound, name, exp, pat, leader, aux, limit, end = None):
        def cut(n, xs):     return xs[0:n], xs[len(xs) if n is None else n:]
        def constant_pat_p(pat):
                def nonconstant_pat_p(x): return listp(x) or nonliteral_atom_p(x)
                return not nonconstant_pat_p(tuple(pat.items())[0][1] if isinstance(pat, dict) else
                                             pat)
        # _debug_printf("segment_match  %20s  %10s  %s  %s  %s  %s  %s", bound, name, exp, pat, leader, aux, limit)
        ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
        seg_pat, rest_pat = pat[0][1:], pat[1:]
        end = (end                        if end is not None                          else
               position(rest_pat[0], exp) if rest_pat and constant_pat_p(rest_pat[0]) else
               0)
        if ((end and end > length(exp)) or ## no boundary variant fitted
            end is None):                  ## a constant pattern was missing
                return fail(bound, exp, pat)
        seg_exp, rest_exp = (cut(end, exp) if rest_pat else
                             (exp, ()))
        aux = (seg_pat + ((some,) + seg_pat,)) if aux is None else aux # We'll MATCH against this
        return coor(crec(lambda:
                                 ((lambda seg_bound, seg_r, seg_fail_pat:
                                           test(seg_fail_pat is None, seg_bound, name, (lambda: seg_r), seg_exp, seg_fail_pat,
                                                if_exists = replace))
                                  (*(  succ(bind((), bound, name), prod((), False)) if seg_exp == () else
                                       fail(bound, exp, pat)                        if limit == 0    else
                                       ## Try biting one more iteration off seg_exp:
                                     _match(bound, name,  seg_exp,      aux,  False,  aux, (limit - 1 if integerp(limit) else
                                                                                            None))))),
                         lambda seg_bound:
                                 _match(seg_bound, None, rest_exp, rest_pat,  False, None, None),
                         leader = leader),
                    lambda: segment_match(  bound, name,      exp,      pat, leader,  aux, limit,
                                            end + 1),
                    leader = leader)

def match_maybe(bound, name, exp, pat, leader, aux, limit):
        return segment_match(bound, name, exp, ((some,) + pat[0][1:],) + pat[1:], leader, aux, 1)

## About the vzy33c0's idea:
## type-driven variable naming is not good enough, because:
## 1. type narrows down the case analysis chain (of which there is a lot)
## 2. expressions also need typing..
def _match(bound, name, exp, pat, leader, aux, limit):
        def maybe_get0Rname(pat):
                ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                (name, pat0), patR = _maybe_destructure_binding(pat[0]), pat[1:]
                return name, pat0, patR, (((pat0,) + patR) if name is not None else
                                                 pat) ## Attempt to avoid consing..
        ## I just caught myself feeling so comfortable thinking about life matters,
        ## while staring at a screenful of code.  In "real" life I'd be pressed by
        ## the acute sense of time being wasted..
        atomp, null = atom(pat), pat == ()
        # _debug_printf("       _match  %20s  %10s  %s  %s  %s  %s  %s", bound, name, exp, pat, leader, aux, limit)
        return \
            (test((match_atom(exp, pat) if atomp else
                   exp == ()),
                  bound, name, lambda: prod(exp, leader), exp, pat) if atomp or null else
             (lambda pat0name, pat0, patR, clean_pat:
                      (equo(name, exp,
                            match_complex(bound, pat0name, exp, clean_pat, leader, aux, limit))
                                                 if complex_pat_p(pat0)    else
                       fail(bound, exp, pat)     if atom(exp) or exp == () else      # pat tupleful, exp tupleful
                       equo(name, exp,
                            crec(lambda:        _match(bound, pat0name, exp[0],  pat0, True,  None, None),
                                 (lambda b0und: _match(b0und, None,     exp[1:], patR, False, None, limit)),
                                 leader = leader))))
             (*maybe_get0Rname(pat)))

def match(exp, pat):
        name, prepped = _maybe_destructure_binding(preprocess(pat))
        return _match(dict(), name, exp, prepped, True, None, None)

print("\n; compiled and loaded.")
###
### app
###
_pp_base_depth = 0
_pp_depth      = 0
_newline        = "newline"
_indent         = "indent"
_form           = "form"
_count_scope    = "count_scope"
def preprocess(pat):
        "Expand syntactic sugar."
        def prep_binding(b):
                k, v = _py.tuple(b.items())[0]
                return {k: preprocess(v)}
        return ((_count_scope, (some,) +
                 preprocess(_py.tuple(pat)))       if typep(pat, _py.list)       else
                prep_binding(pat)                  if typep(pat, _py.dict)       else
                (_form,)                           if pat == _form               else
                (_newline, 0)                      if pat == "\n"                else
                (_newline, pat)                    if integerp(pat)              else
                (_indent, 1)                       if pat == " "                 else
                pat                                if not (pat and _tuplep(pat)) else
                (preprocess(pat[0]),) + preprocess(pat[1:]))
def nonliteral_atom_p(x):
        return x == "name"
def match_atom(exp, pat):
        return integerp(exp)
def prod(x, leader):
        return _py.str(x) if x or leader else ""
def comb(f0, fR, leader):
        global _pp_base_depth, _pp_depth
        acc = "(" if leader else ""
        base_depth_save = _pp_base_depth
        try:
                if leader:
                        _pp_base_depth = _pp_base_depth + _pp_depth + 1
                res0 = f0()
                if res0 is None: return
                acc += res0
                try:
                        _pp_depth += length(res0)
                        resR = fR()
                        if resR is None: return
                        acc += resR
                        acc += ")" if leader else ""
                finally:
                        _pp_depth -= len(res0)
        finally:
                _pp_base_depth = base_depth_save
        return acc
def process_newline(bound, name, exp, pat, leader, aux):
        global _pp_base_depth, _pp_depth
        n, tail = pat[0][1], pat[1:]
        try:
                _pp_base_depth += n
                _pp_depth = 0
                return post(_match(bind(_pp_base_depth, bound, name), None, exp, tail, leader, aux, None),
                            lambda r: "\n" + (" " * _pp_base_depth) + r)
        finally:
                _pp_base_depth -= n
def process_indent(bound, name, exp, pat, leader, aux):
        global _pp_base_depth, _pp_depth
        n, tail = pat[0][1], pat[1:]
        try:
                _pp_depth += n
                return post(_match(bind(_pp_depth, bound, name), None, exp, tail, leader, aux, None),
                            lambda r: (" " * n) + r)
        finally:
                _pp_depth -= n
def _metasex_matcher_init():
        _matcher_init()
        register_complex_matcher(_newline,     process_newline)
        register_complex_matcher(_indent,      process_indent)
        register_complex_matcher(_form,        matcher_not_implemented)
        register_complex_matcher(_count_scope, identity_matcher)
def forc(x, y, leader):
        return (("OR",) if leader else ()) + (x,) + y

_metasex_matcher_init()
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
                _debug_printf("%15s bound: %s", fun.__name__, b)
                _debug_printf("%15s res: %s", fun.__name__, r)
def empty():
        return match((), {"whole":()})
bound_good, result_good, nofail = runtest(empty,
                                          { 'whole': () },
                                          "()")
assert(nofail)
assert(bound_good)
assert(result_good)
print("; EMPTY: passed")

def empty_cross():
        return match((), ({"a":[_name]}, {"b":[(_name,)]},))
bound_good, result_good, nofail = runtest(empty_cross,
                                          { 'a': (), 'b': () },
                                          "()")
assert(nofail)
assert(bound_good)
assert(result_good)
print("; EMPTY-CROSS: passed")

def mid_complex():
        pat = ({"headname":_name},
                  {"headtupname":(_name,)},
                           {"varitupseq":[(_name, [_name])]},
                                                    {"fix1tupseq":[(_name,)]},
                                                                           {"nameseq":[_name]},
                                                                                {"tailname":_name})
        exp =              (1,   (1,),   (1,), (1, 1), (1, 1, 1), (1,), (1,), (1,),   1, 1, 1)
        return match(exp, pat)
bound_good, result_good, nofail = runtest(mid_complex,
                                          { 'headname': 1,
                                            'headtupname': (1,),
                                            'varitupseq': ((1,), (1, 1), (1, 1, 1)),
                                            'fix1tupseq': ((1,), (1,), (1,)),
                                            'nameseq': (1, 1),
                                            'tailname': 1 },
                                          "(1(1)(1)(11)(111)(1)(1)(1)111)"
                                          # "(1 (1) (1) (1 1) (1 1 1) (1) (1) (1) 1 1 1)"
                                          )
assert(nofail)
assert(bound_good)
assert(result_good)
print("; MID-COMPLEX: passed")

def simple_maybe():
        return match((1, 2, 3), ({"a":(_maybe, _name)}, {"b":_name}, (_maybe, {"c":_name})))
bound_good, result_good, nofail = runtest(simple_maybe,
                                          { 'a': (1,), 'b': 2, 'c': 3, },
                                          "(123)")
assert(nofail)
assert(bound_good)
assert(result_good)
print("; SIMPLE-MAYBE: passed")
