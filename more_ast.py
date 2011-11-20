#! /usr/bin/env python
# -*- coding: utf-8 -*-

###
### AST extensions.
###

import ast
import symtable
import marshal
import sys

import cl

from cl         import typep, null, listp, integerp, floatp, boolp, sequencep, stringp, mapcar, mapc,\
                       remove_if, sort, car, identity, every, find, with_output_to_string, error, reduce,\
                       defvar, symbol_value, progv
from cl         import _ast_rw as ast_rw, _ast_alias as ast_alias, _ast_string as ast_string, _ast_name as ast_name, _ast_attribute as ast_attribute, _ast_index as ast_index
from cl         import _ast_funcall as ast_funcall, _ast_maybe_normalise_string as ast_maybe_normalise_string
from cl         import _ast_Expr as ast_Expr, _ast_list as ast_list
from cl         import _not_implemented as not_implemented
from pergamum   import astp, bytesp, emptyp, ascend_tree, multiset, multiset_appendf, tuplep, fprintf
from neutrality import py3p


def extract_ast(source, filename='<virtualitty>'):
    return compile(source, filename, 'exec', flags=ast.PyCF_ONLY_AST)

def extract_symtable(source, filename):
    return symtable.symtable(source, filename, 'exec')


###
### Pyzzle-specific AST
###
def ast_strtuple(x, writep = False):
    assert(tuplep(x))
    return ast.Tuple(elts = mapcar(ast_string, x), ctx = ast_rw(writep))

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
def ast_module(body):
    assert listp(body) and all(mapcar(astp, body))
    return ast.Module(body = body, lineno = 0)

def ast_expression(expr):
    assert astp(expr)
    return ast.Expression(body = expr, lineno = 0)

def ast_def(name, args, *body):
    filtered_body = remove_if(null, body)
    assert stringp(name) and all(mapcar(astp, filtered_body))
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
def ast_bytes(bs):                  return ast.Bytes(s = the(bytes, bs))
def ast_arg(name):                  return ast.Name(arg = the(str, name), ctx = ast.Param())

def ast_tuple(xs, writep=False):
    assert listp(xs) and all(mapcar(astp, xs))
    return ast.Tuple(elts=xs, ctx=ast_rw(writep))

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
    return remove_if(null, body) or [ast.Pass()]

def ast_return(node):     return ast.Return(value = the(ast.AST, node))
def ast_expression(node): return ast.Expression(body = the(ast.AST, node))

def ast_import(*names):
    assert all(mapcar(stringp, names))
    return ast.Import(names=mapcar(ast_alias, names))

def ast_import_all_from(name):
    return ast.ImportFrom(module = the(str, name), names=[ast.alias(name='*', asname=None)], level=0)

def ast_assign(to, value):
    assert listp(to) and all(mapcar(astp, to)) and astp(value)
    return ast.Assign(value=value, targets=to)

def ast_assign_var(name, value):
    assert stringp(name) and (integerp(value) or stringp(value) or astp(value))
    return ast.Assign(value=value, targets=[ast_name(name, True)])

def ast_append_var(name, value):
    assert stringp(name) and (stringp(value) or astp(value))
    return ast.AugAssign(value=value, target=ast_name(name, True), op=ast.Add())

def ast_when(test, *body):
    return ast.If(test=test, body=remove_if(null, body), orelse=[])

def ast_unless(test, *body):
    return ast.If(test=test, body=[], orelse=remove_if(null, body))

def ast_try_except(body, except_handlers, *else_body):
    return ast.TryExcept(body=remove_if(null, body),
                         handlers=[ast.ExceptHandler(name=xname,
                                                     type=ast_name(xtype),
                                                     body=remove_if(null, xhandler_body)) for (xtype, xname, xhandler_body) in except_handlers],
                         orelse=remove_if(null, else_body))

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

def pp_ast(o, stream = sys.stdout):
    """Pretty-print AST O."""

    def do_pp_ast_rec(x, name, pspec):
        lstr = ['']

        def lmesg(msg):
            lstr[0] += msg
            if msg[-1] == '\n'[0]:
                fprintf(stream, (lstr[0]))
                lstr[0] = ''

        def pp_prefix(spec):
            for i in spec:
                lmesg((' |  ' if i else '    '))

        pp_prefix(pspec)
        if name:
            lmesg('<' + name + '>: ')
        if x is None:
            lmesg('<None>\n')
        elif stringp(x):
            lmesg("'" + x + "'\n")
        elif bytesp(x) or integerp(x) or floatp(x) or boolp(x):
            lmesg(str(x) + '\n')
        elif sequencep(x) and emptyp(x):
            lmesg('[]\n')
        else:
            child_slot_names = type(x)._fields
            child_slots = [(k, getattr(x, k)) for k in child_slot_names]
            lmesg(type(x).__name__ + '  ')

            for (k, v) in child_slots:
                if stringp(v):
                    lmesg("<%s>: '%s', "%(k, v))

            lmesg('\n')
            child_list_slots = list(reversed(sort([(k, v) for (k, v) in child_slots if listp(v)], key=car)))
            child_list_slots_nr = len(child_list_slots)

            for (k, v) in child_slots:
                if not listp(v) and not stringp(v):
                    do_pp_ast_rec(v, k, pspec + (([True] if child_list_slots_nr > 0 else [False])))

            for ((k, v), i) in zip(child_list_slots, range(0, child_list_slots_nr)):
                pp_prefix(pspec)
                lmesg(' ^[' + k + ']\n')
                subprefix = pspec + (([True] if i < child_list_slots_nr - 1 else [False]))
                for sub in v:
                    do_pp_ast_rec(sub, '', subprefix)

    do_pp_ast_rec(o, '', [])
    return o

class NotImplemented(Exception):
        def __init__(self, action, x):
                self.action, self.x = action, x
        def __str__(self):
                return "%s %s is not implemented." % (action.capitalize(), x)

defvar("_ast_pp_depth_", 0)
def pp_ast_as_code(x, tab = " " * 8):
        def indent():
                return tab * symbol_value("_ast_pp_depth_")
        def iterate(xs):
                return mapcar(rec, xs)
        def rec(x):
                def pp_call(x):
                        return "%s(%s%s%s%s)" % (pp_ast_as_code(x.func),
                                                 ", ".join(iterate(x.args)),
                                                 ", ".join(iterate(x.keywords)),
                                                 (", *%s" % pp_ast_as_code(x.starargs)) if x.starargs else "",
                                                 (", **%s" % pp_ast_as_code(x.kwargs)) if x.kwargs else "")
                def pp_attribute(x):
                        return "%s.%s" % (pp_ast_as_code(x.value), x.attr)
                def pp_name(x):
                        return x.id
                def pp_arg(x):
                        return x.arg + ((":" + x.annotation) if x.annotation else "")
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
                        return "%s %s%s %s" % (l,
                                             ", ".join(iterate(x.elts)
                                                       if not ast_dict_p(x) else
                                                       mapcar(lambda k, v: "%s: %s" % (k, v),
                                                              x.keys,
                                                              x.values)),
                                             "," if (ast_tuple_p(x) and len(x.elts) == 1) else "",
                                             r)
                def pp_string(x):
                        q = "'''" if find("\n", x.s) else "'"
                        val = with_output_to_string(lambda s: print(x.s, file = s, end = ""))
                        return q + val + q
                def pp_num(x):     return str(x.n)
                binop_print_map = dict(Add = "+", Sub = "-", Mult = "*", Div = "/",
                                       Mod = "%", Pow = "**", LShift = "<<", RShift = ">>",
                                       BitOr = "|", BitXor = "^", BitAnd = "&",
                                       FloorDiv = "//")
                def pp_binop(x):
                        return (pp_ast_as_code(x.left) +
                                (" %s " % (binop_print_map[type(x.op).__name__])) +
                                pp_ast_as_code(x.right))
                def pp_module(x):
                        return "\n".join(iterate(x.body))
                def pp_functiondef(x):
                        "XXX: ignores __annotations__"
                        def pp_args(args):
                                (args, vararg,
                                 kwonlyargs, kwarg,
                                 defaults,
                                 kw_defaults) = mapcar(lambda a: getattr(args, a),
                                                       ["args", "vararg", "kwonlyargs", "kwarg",
                                                        "defaults", "kw_defaults"])
                                fixs = len(args) - len(defaults)
                                return ", ".join(mapcar(rec, args[:fixs]) +
                                                 mapcar(lambda var, val: rec(var) + " = " + val,
                                                        args[fixs:], iterate(defaults)) +
                                                 ([("*" + rec(vararg))] if vararg else []) +
                                                 mapcar(lambda var, val: rec(var) + " = " + val,
                                                        kwonlyargs, iterate(kw_defaults)) +
                                                 ([("**" + rec(kwarg))] if kwarg else []))
                        res = indent() + "def " + x.name + "(" + pp_args(x.args) + "):\n"
                        with progv(_ast_pp_depth_ = symbol_value("_ast_pp_depth_") + 1):
                                res += "\n".join(iterate(x.body))
                        return res + "\n"
                def pp_for(x):
                        res = indent() + "for " + rec(x.target) + " in " + rec(x.iterator) + ":\n"
                        with progv(_ast_pp_depth_ = symbol_value("_ast_pp_depth_") + 1):
                                res += "\n".join(iterate(x.body))
                        if x.orelse:
                                res += indent() + "else:\n"
                                with progv(_ast_pp_depth_ = symbol_value("_ast_pp_depth_") + 1):
                                        res += "\n".join(iterate(x.orelse))
                        return res + "\n"
                # def pp_if(x):
                #         res = indent() + "if " + rec(x.target) + " in " + rec(x.iterator) + ":\n"
                #         with progv(_ast_pp_depth_ = symbol_value("_ast_pp_depth_") + 1):
                #                 res += "\n".join(iterate(x.body))
                #         if x.orelse:
                #                 res += indent() = "else:\n"
                #                 with progv(_ast_pp_depth_ = symbol_value("_ast_pp_depth_") + 1):
                #                         res += "\n".join(iterate(x.orelse))
                #         return res + "\n"
                def pp_Expr(x):
                        return indent() + pp_ast_as_code(x.value)
                def pp_assign(x):
                        return indent() + "%s = %s" % (", ".join(iterate(x.targets)),
                                                       pp_ast_as_code(x.value))
                def make_trivial_pper(x):
                        return (indent() + x +
                                      ((" " + rec(x.value))
                                       if hasattr(x, "value") and x.value else
                                       "") +
                                      "\n")
                def pp_import(x):
                        return indent() + "import " + ", ".join(iterate(x.names))
                map = { ast.Module:      pp_module,
                        ast.FunctionDef: pp_functiondef,
                        ast.For:         pp_for,
                        ast.Expr:        pp_Expr,
                        ast.Call:        pp_call,
                        ast.Attribute:   pp_attribute,
                        ast.Name:        pp_name,
                        ast.arg:         pp_arg,
                        ast.alias:       pp_alias,
                        ast.keyword:     pp_keyword,
                        ast.Assign:      pp_assign,
                        ast.Subscript:   pp_subscript,
                        ast.Index:       pp_index,
                        ast.Slice:       pp_slice,
                        ast.List:        pp_iterable,
                        ast.Tuple:       pp_iterable,
                        ast.Set:         pp_iterable,
                        ast.Dict:        pp_iterable,
                        ast.Str:         pp_string,
                        ast.Num:         pp_num,
                        ast.BinOp:       pp_binop,
                        ast.Return:      make_trivial_pper("return"),
                        ast.Raise:       make_trivial_pper("raise"),
                        ast.Import:      pp_import,
                        }
                def fail(x): not_implemented("pretty-printing AST node %s" % (type(x),))
                try:
                        return map.get(type(x), fail)(x) if x else ""
                except Exception as cond:
                        if typep(cond, NotImplemented):
                                raise
                        else:
                                error("ERROR: %s, while pretty-printing %s.  Slots: %s",
                                      cond, x, dir(x))
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
    mapc(pp_symbol, o.get_symbols())

def totalise_symtable(symtab):
    return ascend_tree(lambda x, *xs: reduce(multiset_appendf, xs, multiset(x.get_symbols(), symtable.Symbol.get_name)),
                       symtab,
                       key=identity,
                       children=lambda x: x.get_children() or [],
                       leafp=lambda l: not l.has_children())

def sym_bound_p(s):
    return s.is_parameter or s.is_assigned
