import sys as _sys

def _debug_printf(format_control, *format_args):
        print((format_control % format_args + "\n"), file = _sys.stderr)

## utility part
def integerp(x):    return isinstance(x, int)
def stringp(x):     return isinstance(x, str)
def typep(x, type): return isinstance(x, type)
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

def _maybe_destructure_binding(pat):
        return ((None, pat)               if not typep(pat, _py.dict) else
                _py.tuple(pat.items())[0] if _py.len(pat) == 1        else
                error_bad_pattern(pat))

def _error_bad_pattern(pat):
        raise Exception("Bad pattern: %s." % (pat,))

class _matcher():
        @staticmethod
        def bind(value, bound, name, if_exists:{error, replace} = error):
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
        @staticmethod
        def succ(bound, res):      return bound, res, None
        @staticmethod
        def fail(bound, exp, pat): return bound, exp, pat
        @staticmethod
        def post(x, mutator):      return (x[0], mutator(x[1]), None) if x[2] is None else x
        ###
        def test(m, test, bound, name, resf:"() -> result", exp, fail_pat, if_exists:{error, replace} = error):
                return (m.succ(m.bind(exp, bound, name, if_exists = if_exists), resf()) if test else
                        m.fail(bound, exp, fail_pat))
        def equo(m, name, exp, x):
                "Apply result binding, if any."
                b, r, f = x
                return ((m.bind(exp, b, name), r, f) if f is None else
                        x) # propagate failure as-is
        def coor(m, l0ret, lR, leader = False):
                l0b, l0r, l0f = l0ret
                if l0f is None: return m.succ(l0b, l0r)
                lRb, lRr, lRf = lR()
                if lRf is None: return m.succ(lRb, lRr)
                return m.fail(l0b, lRr, m.forc(l0f, lRf, leader))
        def crec(m, l0, lR, leader = False):
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
                result = m.comb(try_produce_0, try_produce_R, leader)
                return (m.succ(boundR, result) if failpat is None else
                        m.fail(boundR or bound0, failex, failpat))
        ###
        def register_complex_matcher(m, name, matcher):
                m.__complex_patterns__[name] = matcher
        def complex_pat_p(m, x):
                return _tuplep(x) and x and stringp(x[0]) and x[0] in m.__complex_patterns__
        def match_complex(m, bound, name, exp, pat, leader, aux, limit):
                # _debug_printf("match_complex  %20s  %10s  %s  %s  %s  %s  %s", bound, name, exp, pat, leader, aux, limit)
                return m.__complex_patterns__[pat[0][0]](bound, name, exp, pat, leader, aux, limit)
        def __init__(m):
                m.__complex_patterns__ = _py.dict()
                m.register_complex_matcher(some, m.segment_match)
                m.register_complex_matcher(_maybe, m.match_maybe)
        def matcher_not_implemented(m, bound, name, exp, pat, leader, aux):
                raise Exception("Not yet capable of matching complex patterns of type %s.", pat[0][0])
        def identity_matcher(m, bound, name, exp, pat, leader, aux, limit):
                ## This should dispatch over simplicity.
                return m.match_complex(bound, name, exp, pat[0][1:] + pat[1:], leader, aux, limit)
        ###
        def segment_match(m, bound, name, exp, pat, leader, aux, limit, end = None):
                def cut(n, xs):     return xs[0:n], xs[len(xs) if n is None else n:]
                def position(x, xs):
                        for i, ix in enumerate(xs):
                                if x == ix: return i
                def constant_pat_p(pat):
                        def nonconstant_pat_p(x): return _tuplep(x) or m.nonliteral_atom_p(x)
                        return not nonconstant_pat_p(_py.tuple(pat.items())[0][1] if typep(pat, _py.dict) else
                                                     pat)
                # _debug_printf("segment_match  %20s  %10s  %s  %s  %s  %s  %s", bound, name, exp, pat, leader, aux, limit)
                ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                seg_pat, rest_pat = pat[0][1:], pat[1:]
                end = (end                        if end is not None                          else
                       position(rest_pat[0], exp) if rest_pat and constant_pat_p(rest_pat[0]) else
                       0)
                if ((end and end > _py.len(exp)) or ## no boundary variant fitted
                    end is None):                   ## a constant pattern was missing
                        return m.fail(bound, exp, pat)
                seg_exp, rest_exp = (cut(end, exp) if rest_pat else
                                     (exp, ()))
                aux = (seg_pat + ((some,) + seg_pat,)) if aux is None else aux # We'll MATCH against this
                return m.coor(m.crec(lambda:
                                             ((lambda seg_bound, seg_r, seg_fail_pat:
                                                       m.test(seg_fail_pat is None, seg_bound, name, (lambda: seg_r), seg_exp, seg_fail_pat,
                                                              if_exists = replace))
                                              (*(m.succ(m.bind((), bound, name), m.prod((), False)) if seg_exp == () else
                                                 m.fail(bound, exp, pat)                            if limit == 0    else
                                                 ## Try biting one more iteration off seg_exp:
                                                 m.match(bound, name,  seg_exp,      aux,  False,  aux, (limit - 1 if integerp(limit) else
                                                                                                         None))))),
                                     lambda seg_bound:
                                             m.match(seg_bound, None, rest_exp, rest_pat,  False, None, None),
                                     leader = leader),
                              lambda: m.segment_match(  bound, name,      exp,      pat, leader,  aux, limit,
                                                        end + 1),
                              leader = leader)
        def match_maybe(m, bound, name, exp, pat, leader, aux, limit):
                return m.segment_match(bound, name, exp, ((some,) + pat[0][1:],) + pat[1:], leader, aux, 1)
        ## About the vzy33c0's idea:
        ## type-driven variable naming is not good enough, because:
        ## 1. type narrows down the case analysis chain (of which there is a lot)
        ## 2. expressions also need typing..
        def match(m, bound, name, exp, pat, leader, aux, limit):
                def maybe_get0Rname(pat):
                        ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                        (name, pat0), patR = _maybe_destructure_binding(pat[0]), pat[1:]
                        return name, pat0, patR, (((pat0,) + patR) if name is not None else
                                                         pat) ## Attempt to avoid consing..
                ## I just caught myself feeling so comfortable thinking about life matters,
                ## while staring at a screenful of code.  In "real" life I'd be pressed by
                ## the acute sense of time being wasted..
                atomp, null = not _tuplep(pat), pat == ()
                # _debug_printf("       _match  %20s  %10s  %s  %s  %s  %s  %s", bound, name, exp, pat, leader, aux, limit)
                return \
                    (m.test((m.match_atom(exp, pat) if atomp else
                             exp == ()),
                            bound, name, lambda: m.prod(exp, leader), exp, pat) if atomp or null else
                     (lambda pat0name, pat0, patR, clean_pat:
                              (m.equo(name, exp,
                                      m.match_complex(bound, pat0name, exp, clean_pat, leader, aux, limit))
                                                         if m.complex_pat_p(pat0)         else
                               m.fail(bound, exp, pat)   if not _tuplep(exp) or exp == () else # pat tupleful, exp tupleful
                               m.equo(name, exp,
                                      m.crec(lambda:        m.match(bound, pat0name, exp[0],  pat0, True,  None, None),
                                             (lambda b0und: m.match(b0und, None,     exp[1:], patR, False, None, limit)),
                                             leader = leader))))
                     (*maybe_get0Rname(pat)))

def _match(matcher, exp, pat):
        name, prepped = _maybe_destructure_binding(matcher.preprocess(pat))
        return matcher.match(_py.dict(), name, exp, prepped, True, None, None)

_newline        = "newline"
_indent         = "indent"
_form           = "form"
_count_scope    = "count_scope"

## This is brutally non-thread-safe.  Horrors.  (Yeah, famous last words..)
_pp_base_depth = 0
_pp_depth = 0

class _metasex_matcher(_matcher):
        def __init__(m):
                _matcher.__init__(m)
                m.register_complex_matcher(_newline,     m.process_newline)
                m.register_complex_matcher(_indent,      m.process_indent)
                m.register_complex_matcher(_form,        m.matcher_not_implemented)
                m.register_complex_matcher(_count_scope, m.identity_matcher)
        def preprocess(m, pat):
                "Expand syntactic sugar."
                def prep_binding(b):
                        k, v = _py.tuple(b.items())[0]
                        return {k: m.preprocess(v)}
                return ((_count_scope, (some,) +
                         m.preprocess(_py.tuple(pat))) if typep(pat, _py.list)       else
                        prep_binding(pat)              if typep(pat, _py.dict)       else
                        (_form,)                       if pat == _form               else
                        (_newline, 0)                  if pat == "\n"                else
                        (_newline, pat)                if integerp(pat)              else
                        (_indent, 1)                   if pat == " "                 else
                        pat                            if not (pat and _tuplep(pat)) else
                        (m.preprocess(pat[0]),) + m.preprocess(pat[1:]))
        @staticmethod
        def nonliteral_atom_p(x):
                return x == _name
        @staticmethod
        def match_atom(exp, pat):
                return integerp(exp)
        @staticmethod
        def prod(x, leader):
                return _py.str(x) if x or leader else ""
        @staticmethod
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
                                _pp_depth += _py.len(res0)
                                resR = fR()
                                if resR is None: return
                                acc += resR
                                acc += ")" if leader else ""
                        finally:
                                _pp_depth -= _py.len(res0)
                finally:
                        _pp_base_depth = base_depth_save
                return acc
        def process_newline(m, bound, name, exp, pat, leader, aux):
                global _pp_base_depth, _pp_depth
                n, tail = pat[0][1], pat[1:]
                try:
                        _pp_base_depth += n
                        _pp_depth = 0
                        return m.post(m.match(m.bind(_pp_base_depth, bound, name), None, exp, tail, leader, aux, None),
                                      lambda r: "\n" + (" " * _pp_base_depth) + r)
                finally:
                        _pp_base_depth -= n
        def process_indent(m, bound, name, exp, pat, leader, aux):
                global _pp_base_depth, _pp_depth
                n, tail = pat[0][1], pat[1:]
                try:
                        _pp_depth += n
                        return m.post(m.match(m.bind(_pp_depth, bound, name), None, exp, tail, leader, aux, None),
                                      lambda r: (" " * n) + r)
                finally:
                        _pp_depth -= n
        @staticmethod
        def forc(x, y, leader):
                return (("OR",) if leader else ()) + (x,) + y

_metasex = _metasex_matcher()

print("\n; compiled and loaded.")
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
        return _match(_metasex, (), {"whole":()})
bound_good, result_good, nofail = runtest(empty,
                                          { 'whole': () },
                                          "()")
assert(nofail)
assert(bound_good)
assert(result_good)
print("; EMPTY: passed")

def empty_cross():
        return _match(_metasex, (), ({"a":[_name]}, {"b":[(_name,)]},))
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
        return _match(_metasex, exp, pat)
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
        return _match(_metasex, (1, 2, 3), ({"a":(_maybe, _name)}, {"b":_name}, (_maybe, {"c":_name})))
bound_good, result_good, nofail = runtest(simple_maybe,
                                          { 'a': (1,), 'b': 2, 'c': 3, },
                                          "(123)")
assert(nofail)
assert(bound_good)
assert(result_good)
print("; SIMPLE-MAYBE: passed")
