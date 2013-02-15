###
### Some utilities, in the spirit of Common Lisp.
###
from cl           import typep, functionp, stringp, mapcar, mapc, identity, remove_if, null, every, some, t
from functools    import reduce, partial
from cl           import prefix_suffix_if, prefix_suffix_if_not
from cl           import map_into_hash
from cl           import not_implemented_error, not_implemented
from cl           import curry, compose
from cl           import servile
from cl           import stream_as_string, file_as_string
from cl           import fprintf, dprintf
import neutrality
import ast
import os                 # listdir(), stat(), path[]
import re
import stat               # S_ISREG
import time               # clock()
import sys                # sys.stdout
import traceback
import threading
import operator

from neutrality   import file_content

## "constants"
Empty = frozenset([])

## types
def type_name(x):         return type(x).__name__

def of_type(x):           return lambda y: typep(y, x)

def nonep(o):             return o is None
def minus1p(o):           return o is -1
def bytesp(o):            return type(o) is bytes
def astp(x):              return typep(x, ast.AST)
def code_object_p(x):     return type(x) is type(code_object_p.__code__)

## pseudo-forms
def fwhen(test, clause):
        if test() if functionp(test) else test:
                return clause() if functionp(clause) else clause

def fcond(*clauses):
        "Like COND, but treat all function values as thunks for lazy evaluation."
        for (test, result) in clauses:
                if test() if functionp(test) else test:
                        return result if not functionp(result) else result()

def fcase(val, *clauses):
        "Like CASE, but treat all function values as thunks for lazy evaluation."
        for (cval, result) in clauses:
                if val == cval or cval is True:
                        return result if not functionp(result) else result()

def ftypecase(val, *clauses):
        "Like TYPECASE, but treat all function values as thunks for lazy evaluation."
        for (ctype, result) in clauses:
                if (ctype is True) or typep(val, ctype):
                        return result if not functionp(result) else result()

## functions
def applying_to(args):
        return lambda fn: fn(*args)

def funcalling_with(*args):
        return lambda fn: fn(*args)

def fnot(x):
        return not x

def in_set(xs):
        return lambda x: x in xs

def xform(bool, f, x):
        return f(x) if bool else x

def xform_if(test, f, x):
        return f(x) if test(x) else x

def arg1(*args): return args[0]
def arg2(*args): return args[1]
def arg3(*args): return args[2]

def logorf(x, y):  return x | y
def logandf(x, y): return x & y
def logxorf(x, y): return x ^ y

## conses
# def single(x):        return (x,)
# def ensure_car(x):    return x[0] if consp(x) else x
# def ensure_cons(x, default = None):
#         return x if consp(x)   else (x, default)

# def lisp_list_p(x):
#         return consp(x) or x is None

# def list_len(xs):
#         return 0 if xs is None else 1 + list_len(cdr(xs))

# def list_find(x, xs):
#         if xs is None:
#                 return None
#         elif x == car(xs):
#                 return True
#         else:
#                 return list_find(x, cdr(xs))

# def list_append(l, *more_lists):
#         if zerop(len(more_lists)):
#                 return l
#         else:
#                 def append_2(xs, ys):
#                         return ys if not xs else cons(car(xs), append_2(cdr(xs), ys))
#                 result = append_2(l, more_lists[0])
#                 if len(more_lists) > 1:
#                         return list_append(result, *more_lists[1:])
#                 else:
#                         return result
        
# def list_upto(x, xs):
#         try:
#                 def rec(xs):
#                         if xs is None:
#                                 raise Exception()
#                         elif x == car(xs):
#                                 return cons(x, None)
#                         else:
#                                 return cons(car(xs), rec(cdr(xs)))
#                 return rec(xs)
#         except:
#                 return None

# def mapl(f, xs):
#         if xs != None:
#                 f(car(xs))
#                 mapl(f, cdr(xs))

def list_set(xs):
        acc = [set()]
        def add(x):
                acc[0].add(x)
        mapl(add, xs)
        return acc[0]

## tuple trees
#  TupleTree = Leaf value            :: value
#            | Node value TupleTree* :: (value, ...)
def descend_tree(accumulate_fn, tree, key = lambda x: x[0], children = lambda x: x[1:], acc = None, leafp = stringp):
        """Visit each node of TREE, in order, while accumulating a
value during descent, by calling ACCUMULATE-FN with two arguments: the
value for the current node, and the value accumulated up to this
point, unless it's the root node, which gets a None in this case."""
        if leafp(tree):
                value  = tree
                childs = []
        else:
                value  = key(tree)
                childs = children(tree)
        this_acc = accumulate_fn(value, acc)
        mapc(lambda c: descend_tree(accumulate_fn, c, key, children, this_acc, leafp), childs)

def ascend_tree(combine_fn, tree, key = lambda x: x[0], children = lambda x: x[1:], leafp = stringp):
        """Return the 'combination' of TREE by COMBINE-FN, which is
defined to be the result returned by COMBINE-FN passed n+1 arguments,
where the first argument is the name of the current node and the
remaining n are the results of recursive application of ASCEND-TREE to
the subtrees."""
        (value, childs) = (key(tree), children(tree)) if not leafp(tree) else (tree, [])
        return combine_fn(value, *mapcar(lambda c: ascend_tree(combine_fn, c, key, children, leafp), childs))

def map_tree(accumulate_fn, combine_fn, tree, key = lambda x: x[0], children = lambda x: x[1:], acc = None, leafp = stringp):
        """Combination of DESCEND-TREE and ASCEND-TREE, with the
difference that the first argument to COMBINE-FN is the accumulated
value for the current node, rather than its raw value."""
        (value, childs) = (key(tree), children(tree)) if not leafp(tree) else (tree, [])
        this_acc = accumulate_fn(value, acc)
        return combine_fn(this_acc, *mapcar(lambda c: map_tree(accumulate_fn, combine_fn, c, key, children, this_acc, leafp), childs))

## sets
def frozensetp(o): return isinstance(o, frozenset)
def setp(o):       return isinstance(o, (set, frozenset))

def mapset_star(f, xs):
        acc = set()
        for x in xs:
                acc.add(f(*x))
        return acc

def mapsetn_star(f, xs):
        acc = set()
        for x in xs:
                acc |= f(*x)
        return acc

def multiset(xs, key):
        r = dict()
        for x in xs:
                multiset_addf(r, key(x), x)
        return r

def multiset_len(x):
        return sum([ len(x[k]) for k in x ])

def multiset_addf(d, k, v):
        if k not in d:
                d[k] = set([v])
        else:
                d[k].add(v)
        return d

def multiset_key_appendf(d, k, xs):
        if k not in d:
                d[k] = set(xs)
        else:
                d[k] |= set(xs)
        return d

def multiset_appendf(d1, d2):
        for k in d2:
                if k not in d1:
                        d1[k] = d2[k]
                else:
                        d1[k] |= d2[k]
        return d1

def multiset_largest(xs):
        (maxk, n_max) = (None, 0)
        for k in xs:
                n_elts = len(xs[k])
                if n_elts > n_max:
                        (maxk, n_max) = (k, n_elts)
        return (maxk, n_max)

def multilist_addf(d, k, v):
        if k not in d:
                d[k] = [v]
        else:
                d[k].append(v)
        return d

multiqueue_push = multilist_addf

def multiqueue_pop(d, k):
        if (k not in d) or not len(d[k]):
                return None
        else:
                return d[k].pop(0)

def append_dict(x, y):
        x_larger = len(x) > len(y)
        new = dict(x) if x_larger else dict(y)
        smaller = dict(y) if x_larger else dict(x)
        for k in smaller:
                new[k] = smaller[k]
        return new

def merge_dict(dest, src):
        for k in src:
                if k not in dest:
                        dest[k] = src[k]
        return dest

def unzip_to_sets(pred, xs):
        yep = set()
        nay = set()
        for x in xs:
                if pred(x):
                        yep.add(x)
                else:
                        nay.add(x)
        return yep, nay

## dicts
def invert_hash(xs):
        acc = dict()
        for k in xs:
                acc[xs[k]] = k
        return acc

def alist_hash_table(xs):
        acc = dict()
        for (k, v) in xs:
                acc[k] = v
        return acc

def hash_table_alist(x):
        acc = []
        for k in x.keys():
                acc.append((k, x[k]))
        return acc

def plist_hash_table(xs):
        assert(evenp(len(xs)))
        acc, xs = dict(), list(xs)
        while xs:
                v, k = xs.pop(), xs.pop()
                acc[k] = v
        return acc

## sequences
def emptyp(x):         return len(x) == 0
def singlep(x):        return len(x) == 1
def len_is(x, o):      return len(o) == x
def lastcar(xs):       return xs[-1]
def listf(*args):      return list(args)
def make_set(*args):   return set(args)
def make_tuple(*args): return args

def separate(n, f, xs):
        ss = tuple(set() for _ in range(n))
        for rss in (f(x) for x in xs):
                for s, rs in zip(ss, rss):
                        s |= rs
        return ss

def unzip(pred, xs):
        yep = []
        nay = []
        for x in xs:
                if pred(x):
                        yep.append(x)
                else:
                        nay.append(x)
        return yep, nay

def mapunzip(fpred, xs):
        yep = set([])
        nay = set([])
        for x in xs:
                res = fpred(x)
                if res:
                        yep.add(res)
                else:
                        nay.add(x)
        return yep, nay

__combiners__ = { set: set.add, list: list.append }
def recombine(spec, f, xss):
        accs  = tuple(f() for f in spec)
        combs = tuple(__combiners__[type(a)] for a in accs)
        for xs in xss:
                for acc, comb, reselt in zip(accs, combs, f(xs)):
                        comb(acc, reselt)
        return accs
def recombine_star(spec, f, *xss):
        accs  = tuple(f() for f in spec)
        combs = tuple(__combiners__[type(a)] for a in accs)
        for xs in zip(*xss):
                for acc, comb, reselt in zip(accs, combs, f(*xs)):
                        comb(acc, reselt)
        return accs

def intersperse(x, xs):
        """Return a sequence of elements, with X inserted between every two
adjacent elements of XS."""
        acc = []
        if xs:
                for ix in xs[:-1]:
                        acc.append(ix)
                        acc.append(x)
                acc.append(xs[-1])
        return acc

def seek(n, iterator):
        for i in range(n):
                next(iterator, nil)

def from_(n, xs):
        iterator = iter(xs)
        for i in range(n):
                next(iterator, nil)
        for x in iterator:
                yield x

termination_marker = object()
def take(n, xs):
        iterator = iter(xs)
        for i in range(n):
                elt = next(iterator, termination_marker)
                if elt is not termination_marker:
                        yield elt

## objects
def slotting(x):             return lambda y: getattr(y, x, None)
def slot_of(x):              return lambda y: getattr(x, y, None)
def slot_equal(slot, val):   return lambda y: getattr(y, slot, None) == val

def map_slot_into_total_set(o, fn, total_slot, recur_slot, recur_key=identity):
        '''Compute the total set for TOTAL_SLOT over RECUR_SLOT, accessed through KEY,
caching it on the way.'''
        stack = set()
        def rec(x):
                computed = getattr(x, total_slot)
                if computed:
                        return computed
                else:
                        if x in stack:
                                # error
                                # raise Exception("loop: {} among {}".format(x, stack))
                                return Empty
                        stack.add(x)
                        total = frozenset(fn(x) | mapsetn(compose(rec, recur_key), getattr(x, recur_slot)))
                        setattr(x, total_slot, total)
                        stack.discard(x)
                        return total
        return rec(o)

##
# A subset specifier is either:
#  1. True, representing 'everything'
#  2. a set/frozenset, representing the set as a subset of 'everything', which can be either:
#    2.1 a subset of 'everything', in which case it stands for itself, or
#    2.2 a set, in which case it stands for the intersection of itself and 'everything'
def pp_subset_spec(x):
        return "<everything>" if x is True else x

def subset_spec_eng(x):
        return "all" if x is True else "some"

def the_subset(set, subset_spec):
        "Return a subset (in the sense of #2.1 above) of SET, as specified by SUBSET-SPEC."
        return set if subset_spec is True else subset_spec

def subset(set, subset_spec):
        "Return a subset (in the sense of #2.2 above) of SET, as specified by SUBSET-SPEC."
        return set if subset_spec is True else set & subset_spec

def coerce_to_subset_spec(true_or_list):
        "Return a subset specifier, given either a list or True."
        return True if true_or_list is True else frozenset(true_or_list)

def in_subsetp(x, subset_spec):
        return (x in subset_spec) if setp(subset_spec) else (subset_spec == True)

def subset_spec_p(x):
        return setp(x) or (x is True)

## time
def clocking(fn):
        start = time.clock()
        result = fn()
        return (result, (time.clock() - start))

def perfcount(cell, fn):
        (ret, time) = clocking(fn)
        cell[0] += time
        return ret

def perfcounting(cell, fn):
        def wrapper(*args):
                (ret, time) = clocking(lambda: fn(*args))
                cell[0] += time
                return ret
        return wrapper

## conditions
def condition_variable_name(x):
        "Yeah, because Python is not really intended for serious use."
        if typep(x, UnboundLocalError):
                return re.search("variable '([^\']*)' referenced", x.args[0]).group(1)
        elif typep(x, NameError):
                return re.search("name '([^\']*)' is not", x.args[0]).group(1)

def print_backtrace():
        exc_type, exc_value, _ = sys.exc_info()
        format(t, "%s: %s", str(exc_type), str(exc_value))
        for line in traceback.format_exc().splitlines():
                format(t, "%s", line)

## files
def subfiles(top, test = identity):
        return [ f for f in os.listdir(top) if stat.S_ISREG(os.stat(os.path.join(top, f))[stat.ST_MODE]) and test(f) ]

def read_python_expr_file(path):
        return eval(neutrality.file_content(path))

def walk(root,
        file_action = None,
        directory_action = None,
        filename_matchers = [], filename_antimatchers = [],
        depth = -1, stop_subroots = [],
        terminate_p = lambda: False,
        on_walk_errors = lambda cond: None):
        """Walk a directory tree under ROOT, calling FILE-ACTION and
DIRECTORY-ACTION, respectively, on files (but only those matching any
of FILENAME-MATCHERS and none of FILENAME-ANTIMATCHERS) and
directories (any of them.

Do not descend more than DEPTH directories deep (unless DEPTH is -1).
Do not descend into STOP-SUBROOTS.
On errors, call ON-WALK-ERRORS, passing it the condition object.""" 
        def make_directory_stop_matcher(ds):
                "directory name -> boolean"
                def fn(directory):
                        for d in ds:
                                if directory.startswith(d) and (len(directory) == len(d) or
                                                                (len(directory) > len(d) and
                                                                 directory[len(d)] == os.sep)):
                                        return d
                        return False
                return fn
        def split_path(x):
                return os.path.splitdrive(x)[1].split(os.sep)
        last_dir, last_file = None, None
        root_split = remove_if(null, split_path(root))
        base_depth = len(root_split)
        max_depth = base_depth + depth
        directory_stop_matcher = make_directory_stop_matcher(mapcar(lambda d: d.rstrip(os.sep), stop_subroots))
        try:
                for (dir_, subdirs, files) in os.walk(root, onerror = on_walk_errors):
                        if depth != -1:
                                if len(remove_if(null, split_path(dir_))) > max_depth:
                                        continue
                        stopentry = directory_stop_matcher(dir_)
                        if stopentry:
                                continue
                        last_dir = dir_
                        if directory_action:
                                for subdir in subdirs:
                                        directory_action((dir_, subdir))
                        if file_action:
                                for file_ in files:
                                        # filename matches?
                                        if (some(funcalling_with(file_), filename_matchers)
                                            and not some(funcalling_with(file_), filename_antimatchers)):
                                                # alright, commit to processing..
                                                last_file = os.path.join(dir_, file_)
                                                file_action((dir_, file_))
                                        if terminate_p():
                                                return
        except Exception as cond:
                print_backtrace()
                format(t, "Error %s, last processed: directory \"%s\", file \"%s\".", cond, last_dir, last_file)

## pretty-printing
def eng_yesno(x):     return "yes" if x else "no"
def eng_s(x):         return "" if x == 1 else "s"
def eng_is(x):        return "is" if x == 1 else "are"
def eng_is_pp(x):     return "was" if x == 1 else "were"
def eng_s_is_pp(x):   return " was" if x == 1 else "s were"
def eng_in(x):        return "in" if not x else ""
def eng_un(x):        return "un" if not x else ""
def eng_nt(x):        return "n't" if not x else ""

def pp_lisp_list(xs, quote = False, key = identity, separator = ", "):
        acc = [""],
        quote = "\"" if quote else ""
        def add(x):
                acc[0] += x
        mapl(lambda x: add(quote + key(x) + quote + separator), xs)
        return acc[0][:-2] if acc[0] else "None"

def pp_iterable(xs, quote = False, key = identity, separator = ", "):
        quote = "\"" if quote else ""
        return reduce(lambda x, y: x + quote + key(y) + quote + separator,
                      list(xs),
                      "")[:-len(separator)]

def pp_list(xs, quote = False, key = identity, separator = ", "):
        return pp_lisp_list(xs, quote, key, separator) if lisp_list_p(xs) else pp_iterable(xs, quote, key, separator)

def pp_percentage(x, total, format = "%d"):
        npercents = x*100.0/(total or 1)
        return ((format % npercents + "%") if plusp(npercents) else ("none" if zerop(x) else "<1%"))

## strings
def string_line_offsets(string):
        acc = [0]
        for i, c in enumerate(string):
                if c == "\n":
                        acc.append(i + 1)
        if acc[-1] == len(string):
                acc.pop()
        return acc

def string_to_integer_in_range(x, min_, max_):
        "Return int(X), if X is a string representation of an integer and satisfies MIN <= X <= MAX.  Return None otherwise."
        try:
                x_int = int(x)
                if min_ <= x_int <= max_:
                        return x_int
        except:
                pass

def string_to_complex(x_str, spec):
        """Validate X-STR against SPEC.  X-STR must be a string, containing one of:
        - a printed string literal,
        - an integer,
        - a tuple, containing objects of any types listed here,
        - a list, containing objects of any types listed here.

        Example:

        string_to_complex("[(\"/usr/bin\", -1), (\"/bin/\", -1)]", [(stringp, integerp)])
        => [(\"/usr/bin\", -1), (\"/bin/\", -1)]"""
        assert(stringp(x_str))
        x = ast.literal_eval(x_str)
        def rec(x, subspec):
                return fcond((functionp(subspec),        lambda: subspec(x)),
                             (isinstance(subspec, list), lambda: isinstance(x, list) and every(lambda x: rec(x, subspec[0]), x)),
                             (   tuplep(subspec),        lambda: tuplep(x) and (len(x) == len(subspec)) and every(lambda v_t: rec(v_t[0], v_t[1]), zip(x, subspec))))
        return x if rec(x, spec) is not False else None

def strconcat(strs):
        return reduce(operator.add, strs)

## streams
def with_output_redirection(fn, file = None):
        old_stdout = sys.stdout
        try:
                sys.stdout = file
                ret = fn()
        finally:
                sys.stdout = old_stdout
        return ret

## numbers
def chunkify(n, size, overlap):
        "Partition N into SIZE-long chunks with OVERLAP bytes."
        acc, i, lim = [], 0, max(n - overlap, overlap)
        while i < lim:
                acc.append((i, min(i + size, n)))
                i += size - overlap
        return acc

## printing
def printf(format_control, *format_args):
        fprintf(sys.stdout, format_control, *format_args)

def syncprintf(format_control, *format_args):
        fprintf(sys.stdout, format_control, *format_args)
        sys.stdout.flush()
