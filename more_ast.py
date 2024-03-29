#! /usr/bin/env python
# -*- coding: utf-8 -*-

###
### AST extensions.
###

import ast
import symtable
import marshal
import sys
import types

import cl

from cl         import t, nil, typep, the, null, integerp, floatp, functionp, stringp,\
                       identity, with_output_to_string, error, \
                       symbol_value, progv
from cl         import not_implemented, string_t
from pergamum   import astp, bytesp, emptyp, ascend_tree, multiset, multiset_appendf, fprintf
from neutrality import py3p
from functools  import reduce


def extract_ast(source, filename='<virtualitty>'):
    return compile(source, filename, 'exec', flags=ast.PyCF_ONLY_AST)

def extract_symtable(source, filename):
    return symtable.symtable(source, filename, 'exec')


###
### Pyzzle-specific AST
###
def ast_strtuple(x, writep = False):
    assert(isinstance(x, tuple))
    return ast.Tuple(elts = [ ast_string(x) for x in x ], ctx = ast_rw(writep))

def ast_marshal(x):
    return ast_funcall(ast_attribute(ast_name("marshal"), "loads"), (ast_bytes if py3p() else ast_string)(marshal.dumps(x)))

def module_ast_function_p(x):
    return x.body and every(lambda x: ast_def_p(x) or ast_import_maybe_from_p(x), x.body)


# predicates
def ast_string_p(x):            return typep(x, ast.Str)
def ast_dict_p(x):              return typep(x, ast.Dict)
def ast_list_p(x):              return typep(x, ast.List)
def ast_tuple_p(x):             return typep(x, ast.Tuple)
def ast_num_p(x):               return typep(x, ast.Num)
def ast_name_p(x):              return typep(x, ast.Name)
def ast_string_equalp(x, s):    return ast_string_p(x) and x.s == s
def ast_assign_p(x, to):        return typep(x, ast.Assign) and to in x.targets
def ast_pass_p(x):              return typep(x, ast.Pass)
def ast_module_p(x):            return typep(x, ast.Module)
def ast_def_p(x):               return typep(x, ast.FunctionDef)
def ast_import_p(x):            return typep(x, ast.Import)
def ast_import_from_p(x):       return typep(x, ast.ImportFrom)
def ast_import_maybe_from_p(x): return ast_import_p(x) or ast_import_from_p(x)
def ast_call_p(x):              return typep(x, ast.Call)
def ast_subscript_p(x):         return typep(x, ast.Subscript)
def ast_attribute_p(x):         return typep(x, ast.Attribute)
def ast_keyword_p(x):           return typep(x, ast.keyword)
def ast_slice_p(x):             return typep(x, ast.Slice)
def ast_extslice_p(x):          return typep(x, ast.ExtSlice)
def ast_index_p(x):             return typep(x, ast.Index)

def ast_mod_p(x):               return typep(x, ast.mod)
def ast_stmt_p(x):              return typep(x, ast.stmt)
def ast_expr_p(x):              return typep(x, ast.expr)

def ast_Expr_p(x):              return typep(x, ast.Expr)

# top-levels
def ast_Expr(node):
        return ast.Expr(value = the(ast.expr, node))

def ast_module(body, lineno = 0):
        assert isinstance(body, list) and all(isinstance(x, ast.AST) for x in body)
        return ast.Module(body = body,
                          lineno = lineno)

def ast_expression(expr):
    assert astp(expr)
    return ast.Expression(body = expr, lineno = 0)

def ast_def(name, args, *body):
    filtered_body = [ x for x in body if x ]
    assert stringp(name) and all(astp(x) for x in filtered_body)
    ast_args = ast.arguments(
                             args=args,
                             defaults=[],
                             kwonlyargs=[],
                             kw_defaults=[],
                             vararg=None,
                             varargannotation=None,
                             kwarg=None,
                             kwargannotation=None,
                            )
    return ast.FunctionDef(name=name, decorator_list=[], args=ast_args, body=filtered_body, returns=None)


## expressions
def _ast_alias(name):                       return ast.alias(name = the(string_t, name), asname = None)
def ast_keyword(name, value):               return ast.keyword(arg = the(string_t, name), value = the(ast.expr, value))

def ast_rw(writep):                         return (ast.Store() if writep else ast.Load())
def ast_name(name, writep = nil):           return ast.Name(id = the(string_t, name), ctx = ast_rw(writep))
def ast_attribute(x, name, writep = nil):   return ast.Attribute(attr = name, value = x, ctx = ast_rw(writep))
def ast_attribute_chain(xs, writep = nil):  return reduce((lambda acc, attr: ast_attribute(acc, attr, writep)),
                                                           xs[1:],
                                                           ast_name(xs[0], writep))
def ast_index(of, index, writep = nil):     return ast.Subscript(value = of, slice = ast.Index(value = index), ctx = ast_rw(writep))
def ast_maybe_normalise_string(x):          return (ast_string(x) if isinstance(x, str) else x)
def ast_bytes(bs):                  return ast.Bytes(s = the(bytes, bs))
def ast_arg(name):                  return ast.Name(arg = the(str, name), ctx = ast.Param())

def ast_dict(keys, values):
    return ast.Dict(keys = keys, values = values)

def ast_func_name(x):
    if typep(x, ast.Name):
        return x.id
    elif ast_subscript_p(x):
        return ast_func_name(x.value) + '[' + ast_func_name(x.slice.value) + ']'
    elif typep(x, ast.Attribute):
        return ast_func_name(x.value) + '.' + x.attr
    else:
        return '<unhandled>'

## statements
def astlist_prog(*body):
    "WARNING: not an actual node, returns a list!"
    return [ x for x in body if x ] or [ast.Pass()]

def ast_expression(node): return ast.Expression(body = the(ast.AST, node))

def ast_import_all_from(name):
    return ast.ImportFrom(module = the(str, name), names=[ast.alias(name='*', asname=None)])

def ast_assign_var(name, value):
    assert stringp(name) and (integerp(value) or stringp(value) or astp(value))
    return ast.Assign(value=value, targets=[ast_name(name, True)])

def ast_append_var(name, value):
    assert stringp(name) and (stringp(value) or astp(value))
    return ast.AugAssign(value=value, target=ast_name(name, True), op=ast.Add())

def ast_when(test, *body):
    return ast.If(test=test, body = [ x for x in body if x ], orelse=[])

def ast_unless(test, *body):
    return ast.If(test = test, body = [], orelse = [ x for x in body if x ])

def ast_try_except(body, except_handlers, *else_body):
    return ast.Try(     body = [ x for x in body if x ],
                    handlers = [ ast.ExceptHandler(name = xname,
                                                   type = ast_name(xtype),
                                                   body = [ x for x in xhandler_body if x ])
                                 for (xtype, xname, xhandler_body) in except_handlers ],
                      orelse = [ x for x in else_body if x ],
                   finalbody = [])

def ast_print(*strings):
    return ast_expr(ast_funcall('print', *strings))


## validation
def ast_invalid_p(x, checks):
    """Perform a series of AST CHECKS on X.  If all is well, return None, otherwise, return an explanation."""

    for check in checks:
        result = check[0](x)
        if not result:
            return check[1] % check[2:]

def ast_fqn_p(x):
    if typep(x, ast.Name):
        return (x.id, )
    elif typep(x, ast.Attribute):
        rec = ast_fqn_p(x.value)
        return (rec + (x.attr, ) if rec else False)
    else:
        return False

def ast_children(x, ast_only = None, lineno_only = None):

        def passes(x):
                return (((not lineno_only) or
                         hasattr(x, "lineno")) and
                        ((not ast_only) or
                         isinstance(x, ast.AST)))
        for slot in x._fields:
                slotval = getattr(x, slot)
                if isinstance(slotval, list):
                        for elt in slotval:
                                if passes(elt):
                                        yield elt
                else:
                        if passes(slotval):
                                yield slotval

def ast_last_lineno(form):
        return max([form.lineno] +
                   [ ast_last_lineno(x)
                     for x in ast_children(form, lineno_only = True)])

def ast_first_subnode_at_lineno(form, lineno):
        """The implied condition is that LINENO definitely points at
some subform of FORM."""
        assert(lineno >= form.lineno)
        def rec(form):
                children = sorted(ast_children(form, lineno_only = t),
                                  key = slotting("lineno"))
                for c in children[::-1]:
                        if lineno >= c.lineno:
                                return rec(c)
                return form
        return rec(form)

def pp_ast(o, stream = sys.stdout):
    def do_pp_ast_rec(x, name, pspec):
        lstr = ['']
        def lmesg(msg):
            lstr[0] += msg
            if msg[-1] == '\n'[0]:
                print(lstr[0], file = stream, end = '')
                lstr[0] = ''
        def pp_prefix(spec):
            for i in spec:
                lmesg((' |  ' if i else '    '))
        pp_prefix(pspec)
        if name:
            lmesg('<' + name + '>: ')
        if x is None:
            lmesg('<None>\n')
        elif isinstance(x, str):
            lmesg("'" + x + "'\n")
        elif isinstance(x, (bytes, int, float, bool)):
            lmesg(str(x) + '\n')
        elif isinstance(x, list) and len(x) == 0:
            lmesg('[]\n')
        else:
            child_slot_names = type(x)._fields
            child_slots = [ (k, getattr(x, k))
                            for k in child_slot_names ]
            lmesg(type(x).__name__ + '  ')
            for slot, val in child_slots:
                if isinstance(val, str):
                    lmesg("<%s>: '%s', " % (slot, val))
            lmesg('\n')
            child_list_slots = list(reversed(sorted([(k, v) for (k, v) in child_slots if isinstance(v, list)],
                                                    key = lambda x: x[0])))
            child_list_slots_nr = len(child_list_slots)
            for (k, v) in child_slots:
                if not isinstance(v, list) and not isinstance(v, str):
                    do_pp_ast_rec(v, k, pspec + (([True] if child_list_slots_nr > 0 else [False])))
            for ((k, v), i) in zip(child_list_slots, range(0, child_list_slots_nr)):
                pp_prefix(pspec)
                lmesg(' ^[' + k + ']\n')
                subprefix = pspec + (([True] if i < child_list_slots_nr - 1 else [False]))
                for sub in v:
                    do_pp_ast_rec(sub, '', subprefix)
    do_pp_ast_rec((compile(o, "<ast>", 'exec', flags=ast.PyCF_ONLY_AST).body[0] if isinstance(o, str) else
                   o), '', [])
    return o

class NotImplemented(Exception):
        def __init__(self, action, x):
                self.action, self.x = action, x
        def __str__(self):
                return "%s %s is not implemented." % (action.capitalize(), x)

def chain(fn, xs, initial):
        # Why do I always come up with crazy functional shit while drunk?
        # ..in this case after physical endurance exercise..
        # .."endura", Cathari legacy..
        # Just for the record: this is indeed insane batshit.
        # It essentially boils down to reduce(fn, append(*xs), initial)
        # Its only saving grace is that it's non-consing.
        return reduce(lambda val, x: reduce(fn, x, val), xs, initial)

def _type_name(x):
        return type(x).__name__

import operator
def assign_meaningful_locations(node, lineno = 1):
        # Here and thereafter, lineno means 'next free lineno'
        def advance0(lineno): return lineno
        def advance1(lineno): return lineno + 1
        def normally(xs):  return [(x,    advance0) for x in xs]
        def advancing(xs): return [(x,    advance1) for x in xs]
        def dir(x):        return [(None, (x                           if functionp(x) else
                                           (lambda lineno: lineno + x) if integerp(x)  else
                                           error("Invalid direct advancement specifier: %s.", x.__repr__())))]
        def rec(lineno: int, xspec: (ast.AST, types.FunctionType)) -> int:
                if not isinstance(xspec, tuple):
                        error("Advancement specifier not a tuple: %s", xspec if not isinstance(xspec, ast.AST) else pp_ast_as_code(xspec))
                x, advance = xspec
                isinstance(advance, types.FunctionType) or error("Invariant failed in AST location walker: advance is: %s.", advance)
                def attrs(*attrs): return reduce(operator.add, (([x] if not isinstance(x, list) else x)
                                                                for x in (getattr(x, attr) for attr in attrs)
                                                                if x is not None))
                def make_binder(node):
                        def binder(lineno):
                                if isinstance(node, ast.AST):
                                        node.lineno = lineno
                                return lineno
                        return binder
                def self_binder(): return make_binder(x)
                def handle_body_orelse(x, prechain, intrachain = []):
                        chainables = ([prechain, dir(self_binder()), dir(1), normally(x.body), intrachain] +
                                      ([normally(x.orelse)] if hasattr(x, "orelse") and x.orelse else
                                       []))
                        return chain(rec, chainables, lineno)
                reductios = {
                        type(None):        (lambda lineno, x: lineno),
                        ast.Module:        (lambda lineno, x: reduce(rec, dir(self_binder()) + normally(x.body), lineno)),
                        ast.FunctionDef:   (lambda lineno, x: chain(rec, [advancing(x.decorator_list), dir(self_binder()),
                                                                          normally(attrs("name", "args", "returns")), dir(1),
                                                                          normally(x.body)],
                                                                    lineno)),
                        ast.ClassDef:      (lambda lineno, x: chain(rec, [advancing(x.decorator_list), dir(self_binder()),
                                                                          normally(attrs("name", "bases", "keywords",
                                                                                         "starargs", "kwargs")), dir(1),
                                                                          normally(x.body)],
                                                                    lineno)),
                        ## WARNING: ignores With items after the first one!
                        ast.With:          (lambda lineno, x: handle_body_orelse(x, normally([x.items[0].context_expr, x.items[0].optional_vars]))),
                        ast.For:           (lambda lineno, x: handle_body_orelse(x, normally([x.target, x.test]))),
                        ast.If:            (lambda lineno, x: handle_body_orelse(x, normally([x.test]))),
                        ast.While:         (lambda lineno, x: handle_body_orelse(x, normally([x.test]))),
                        ast.ExceptHandler: (lambda lineno, x: handle_body_orelse(x, normally([x.type, x.name]))),
                        ast.Try:           (lambda lineno, x: chain(rec, [dir(self_binder()), dir(1),
                                                                          normally(x.body), dir(1),
                                                                          normally(x.handlers), dir(1),
                                                                          normally(x.finalbody)], lineno)) }
                def default_reductio(lineno: int, x: ast.AST) -> int:
                        if not isinstance(x, ast.AST):
                                return lineno
                        x.lineno = lineno
                        for f in x._fields:
                                fv = getattr(x, f)
                                [ rec(lineno, (fvv, advance0))
                                  for fvv in (fv if isinstance(fv, list) else [fv])
                                  if isinstance(fvv, ast.AST) ]
                        return lineno + (1 if isinstance(x, ast.stmt) else 0)
                return advance(reductios.get(type(x), default_reductio)(lineno, x))
        return (rec(lineno, (node, advance0))       if isinstance(node, ast.AST)       else
                reduce(rec, normally(node), lineno) if isinstance(node, (list, tuple)) else
                error("AST location assigner only accepts singular AST nodes and lists thereof, got: %s.", node))

cl.string_set("*AST-PP-DEPTH*", 0, force_toplevel = t, globals = globals())
def pp_ast_as_code(x, tab = " " * 8, line_numbers = nil, ndigits = 3, annotate_written_names = None):
        fmtctl = "%%%dd " % ndigits
        def indent(ast_or_lineno, offset = 0):
                lineno = (ast_or_lineno.lineno if hasattr(ast_or_lineno, "lineno") else 0)
                return ((fmtctl % (lineno + offset)) if line_numbers else
                        "") + tab * symbol_value(_ast_pp_depth_)
        def iterate(xs):
                return [ rec(x) for x in xs ]
        def rec(x):
                ## Expressions
                def pp_call(x):
                        return ("%s(%s%s%s%s)" % (pp_ast_as_code(x.func),
                                                  ", ".join(iterate(x.args)),
                                                  (", " if x.keywords else "") + ", ".join(iterate(x.keywords)),
                                                  (", " if x.starargs and (x.args or x.keywords) else "") +
                                                  ("*%s" % pp_ast_as_code(x.starargs)) if x.starargs else "",
                                                  (", " if x.kwargs and (x.args or x.keywords or x.starargs) else "") +
                                                  ("**%s" % pp_ast_as_code(x.kwargs)) if x.kwargs else ""))
                def pp_attribute(x):
                        return "%s.%s" % (pp_ast_as_code(x.value), x.attr)
                def pp_name(x):
                        return x.id + (":w" if annotate_written_names and isinstance(x.ctx, ast.Store) else "")
                def pp_arg(x):
                        return x.arg + ((": " + rec(x.annotation)) if x.annotation else "")
                def pp_alias(x):
                        return x.name + ((" as " + x.asname) if x.asname else "")
                def pp_keyword(x):
                        return "%s = %s" % (x.arg, pp_ast_as_code(x.value))
                def pp_subscript(x):
                        return "%s[%s]" % (pp_ast_as_code(x.value),
                                           pp_ast_as_code(x.slice))
                def pp_index(x):
                        return "%s" % pp_ast_as_code(x.value)
                def pp_slice(x):
                        l, u, s = x.lower or "", x.upper or "", x.step
                        if x.step:
                                return "%s:%s:%s" % tuple(iterate((l, u, s)))
                        else:
                                return "%s:%s" % tuple(iterate((l, u)))
                def pp_iterable(x):
                        l, r = { ast.List: ("[", "]"), ast.Tuple: ("(", ")"), ast.Set: ("{", "}"), ast.Dict: ("{", "}"), } [type(x)]
                        return "%s%s%s%s" % (l,
                                             ", ".join(iterate(x.elts)
                                                       if not ast_dict_p(x) else
                                                       [ "%s: %s" % (rec(k), rec(v))
                                                         for k, v in zip(x.keys, x.values) ]),
                                             "," if (ast_tuple_p(x) and len(x.elts) == 1) else "",
                                             r)
                def pp_string(x):
                        q = "'''" if "\n" in x.s else "'"
                        val = with_output_to_string(lambda s: print(x.s, file = s, end = ""))
                        return q + val + q
                def pp_num(x):     return str(x.n)
                op_print_map = dict(# binary
                                    Add = "+", Sub = "-", Mult = "*", Div = "/",
                                    Mod = "%", Pow = "**", LShift = "<<", RShift = ">>",
                                    BitOr = "|", BitXor = "^", BitAnd = "&",
                                    FloorDiv = "//",
                                    # unary
                                    Invert = "~", Not = "not", UAdd = "+", USub = "-",
                                    # bool
                                    And = "and", Or = "or",
                                    # cmpop
                                    Eq = "eq", NotEq = "not eq",
                                    Lt = "<", LtE = "<=", Gt = ">", GtE = ">=",
                                    Is = "is", IsNot = "is not",
                                    In = "in", NotIn = "not in")
                def pp_binop(x):
                        return (pp_ast_as_code(x.left) +
                                (" %s " % (op_print_map[type(x.op).__name__])) +
                                pp_ast_as_code(x.right))
                def pp_augassign(x):
                        return (pp_ast_as_code(x.target) +
                                (" %s= " % (op_print_map[type(x.op).__name__])) +
                                pp_ast_as_code(x.value))
                def pp_unop(x):
                        return (("%s " % (op_print_map[type(x.op).__name__])) +
                                pp_ast_as_code(x.operand))
                def pp_boolop(x):
                        return ((" %s " % (op_print_map[type(x.op).__name__])).join(iterate(x.values)))
                def pp_compare(x):
                        return rec(x.left) + "".join(" %s %s" % (op_print_map[type(op).__name__], rec(comp)) for op, comp in zip(x.ops, x.comparators))
                def pp_ifexp(x):
                        return ("(" + rec(x.body) + " if " +
                                rec(x.test) + " else " +
                                rec(x.orelse) + ")")
                def pp_args(args):
                        (args, vararg,
                         kwonlyargs, kwarg,
                         defaults,
                         kw_defaults) = [ getattr(args, a)
                                          for a in [ "args", "vararg", "kwonlyargs", "kwarg",
                                                     "defaults", "kw_defaults" ]]
                        fixs = len(args) - len(defaults)
                        return ", ".join([ rec(x) for x in args[:fixs] ]
                                         + [ rec(var) + " = " + val
                                             for var, val in zip(args[fixs:], iterate(defaults)) ]
                                         + ([("*" + vararg)] if vararg else [])
                                         + [ rec(var) + " = " + val
                                             for var, val in zip(kwonlyargs, iterate(kw_defaults)) ]
                                         + ([("**" + rec(kwarg))] if kwarg else []))
                def pp_lambda(x):
                        args = rec(x.args)
                        return "(lambda%s: %s)" % (" " + args if args else "", rec(x.body))
                ## Linefuls
                def pp_Expr(x):
                        return indent(x) + rec(x.value)
                def pp_import(x):
                        return indent(x) + "import " + ", ".join(iterate(x.names))
                def pp_assign(x):
                        return indent(x) + "%s = %s" % (", ".join(iterate(x.targets)),
                                                        rec(x.value))
                def make_trivial_pper(name, slots = ["value"]):
                        ## Assert Break Continue Pass Raise Return
                        return lambda y: (indent(x) + name +
                                          ",".join((" " + rec(getattr(y, slot)))
                                                   for slot in slots
                                                   if hasattr(y, slot) and getattr(y, slot)))
                def make_comp_pper(lbr, rbr):
                        return lambda comp: (lbr + " " +
                                             rec(comp.elt) + " " + " ".join(rec(x) for x in comp.generators)
                                             + " " + rbr)
                def pp_dictcomp(comp):
                        return ("{ " +
                                rec(comp.key) + ": " + rec(comp.value) + " ".join(rec(x) for x in comp.generators)
                                + " }")
                def pp_comprehension(x):
                        return "for %s in %s%s" % (rec(x.target), rec(x.iter), "".join(" if %s" % rec(x) for x in x.ifs))
                ## Multilines
                def pp_subprogn(body):
                        with progv({_ast_pp_depth_: symbol_value(_ast_pp_depth_) + 1}):
                                return "\n".join(iterate(body))
                def pp_module(x):
                        return "\n".join(iterate(x.body))
                def pp_functiondef(x):
                        "XXX: ignores __annotations__"
                        return ("\n".join(indent(d) + "@" + rec(d) for d in x.decorator_list) +
                                ("\n" if x.decorator_list else "") +
                                indent(x) + "def " + x.name + "(" + rec(x.args) + "):" +
                                (("-> %s" % rec(x.returns)) if x.returns is not None else "") + "\n" +
                                pp_subprogn(x.body))
                def pp_try(x):
                        return (indent(x) + "try:\n" +
                                pp_subprogn(x.body) + "\n" +
                                "\n".join((indent(subh.body[0], -1) + "except" + ((" %s as %s" % (rec(subh.type), subh.name))
                                                                                  if subh.type else
                                                                                  "") + ":\n" +
                                           pp_subprogn(subh.body))
                                          for subh in x.handlers) +
                                ((indent(x.finalbody[0], -1) + "finally:\n" + pp_subprogn(x.finalbody)) if x.finalbody else
                                 "") +
                                ((indent(x.orelse[0],    -1) + "else:\n"    + pp_subprogn(x.orelse))    if x.orelse    else
                                 ""))
                def pp_for(x):
                        return (indent(x) + "for " + rec(x.target) + " in " + rec(x.iterator) + ":\n" +
                                pp_subprogn(x.body) +
                                ((indent(x.orelse[0], -1) + "else:\n" + pp_subprogn(x.orelse)) if x.orelse else ""))
                def pp_with(x):
                        ## WARNING: ignores With items after the first one!
                        return (indent(x) + "with " + rec(x.items[0].context_expr) + (" as %s" % rec(x.items[0].optional_vars) if x.items[0].optional_vars else "") + ":\n" +
                                pp_subprogn(x.body))
                def pp_while(x):
                        return (indent(x) + "while " + rec(x.test) + ":\n" +
                                pp_subprogn(x.body) +
                                ((indent(x.orelse[0], -1) + "else:\n" + pp_subprogn(x.orelse)) if x.orelse else ""))
                def pp_if(x):
                        def irec(x, firstp):
                                chainp = len(x.orelse) is 1 and typep(x.orelse[0], ast.If)
                                return (indent(x) + ("" if firstp else "el") + "if " + rec(x.test) + ":\n" +
                                        pp_subprogn(x.body) +
                                        ("" if not x.orelse else
                                          ("\n" + (irec(x.orelse[0], False) if chainp else
                                                   (indent(x.orelse[0], -1) + "else:\n" +
                                                    pp_subprogn(x.orelse))))))
                        return irec(x, True)
                map = { list:            lambda x: ", ".join(rec(ix) for ix in x),
                        str:             lambda x: x,
                        ast.Module:      pp_module,
                        ast.For:         pp_for,
                        ast.With:        pp_with,
                        ast.While:       pp_while,
                        ast.If:          pp_if,
                        ast.Try:         pp_try,
                        ast.FunctionDef: pp_functiondef,
                        ast.arguments:   pp_args,

                        ast.Assert:      make_trivial_pper("assert", slots = ["test", "msg"]),
                        ast.Break:       make_trivial_pper("break"),
                        ast.Continue:    make_trivial_pper("continue"),
                        ast.Pass:        make_trivial_pper("pass"),
                        ast.Raise:       make_trivial_pper("raise", slots = ["exc", "cause"]),
                        ast.Return:      make_trivial_pper("return"),
                        ast.Delete:      make_trivial_pper("delete"),
                        ast.Global:      make_trivial_pper("global", slots = ["names"]),
                        ast.Nonlocal:    make_trivial_pper("nonlocal", slots = ["names"]),
                        ast.Import:      pp_import,
                        ast.Assign:      pp_assign,
                        ast.AugAssign:   pp_augassign,

                        ast.Lambda:      pp_lambda,

                        ast.ListComp:      make_comp_pper("[", "]"),
                        ast.SetComp:       make_comp_pper("{", "}"),
                        ast.GeneratorExp:  make_comp_pper("(", ")"),
                        ast.DictComp:      pp_dictcomp,
                        ast.comprehension: pp_comprehension,

                        ast.Expr:        pp_Expr,
                        ast.Call:        pp_call,
                        ast.Attribute:   pp_attribute,
                        ast.Subscript:   pp_subscript,
                        ast.IfExp:       pp_ifexp,
                        ast.Index:       pp_index,
                        ast.Slice:       pp_slice,
                        ast.Name:        pp_name,
                        ast.Starred:     lambda x: "*" + rec(x.value),

                        ast.List:        pp_iterable,
                        ast.Tuple:       pp_iterable,
                        ast.Set:         pp_iterable,
                        ast.Dict:        pp_iterable,
                        ast.Str:         pp_string,
                        ast.Num:         pp_num,

                        ast.BoolOp:      pp_boolop,
                        ast.BinOp:       pp_binop,
                        ast.UnaryOp:     pp_unop,
                        ast.Compare:     pp_compare,

                        ast.arg:         pp_arg,
                        ast.alias:       pp_alias,
                        ast.keyword:     pp_keyword,
                        }
                def fail(x): not_implemented("pretty-printing AST node %s: %s" % (type(x), x))
                try:
                        return map.get(type(x), fail)(x) if x else ""
                except NotImplemented:
                        raise
                # except Exception as cond:
                #         error("ERROR: %s, while pretty-printing %s.  Slots: %s",
                #               cond, x, dir(x))
        return rec(x)

## symbols
symbol_attributes = [
    'referenced',
    'assigned',
    'global',
    'free',
    'parameter',
    'local',
    'imported',
    'declared_global',
    'namespace',
    ]

def pp_symbol(o):
    mesg("   symbol '" + o.get_name() + "': %s", reduce(lambda x, y: x + ((' ' + y if getattr(o, 'is_' + y)() else '')), symbol_attributes, ''))

def pp_symtable(o):
    symtab_attributes = [
        'get_id',
        'get_lineno',
        'is_optimized',
        'is_nested',
        'has_children',
        'has_exec',
        'has_import_star',
        'get_identifiers',
        'get_symbols',
        'get_children',
        ]
    fnsymtab_attributes = ['get_parameters', 'get_locals', 'get_globals', 'get_frees']
    attributes = symtab_attributes + ((fnsymtab_attributes if typep(o, symtable.Function) else []))
    mesg('   ' + o.get_type() + " symtab '" + o.get_name() + "':\n%s", reduce(lambda x, y: x + '\n        ' + y + ': ' \
         + str(getattr(o, y)()), attributes, ''))
    for s in o.get_symbols():
            pp_symbol(s)

def totalise_symtable(symtab):
    return ascend_tree(lambda x, *xs: reduce(multiset_appendf, xs, multiset(x.get_symbols(), symtable.Symbol.get_name)),
                       symtab,
                       key=identity,
                       children=lambda x: x.get_children() or [],
                       leafp=lambda l: not l.has_children())

def sym_bound_p(s):
    return s.is_parameter or s.is_assigned
