#! /usr/bin/env python
# -*- coding: utf-8 -*-

###
### AST extensions.
###

import ast
import symtable
import marshal
import sys
from functools import reduce

from cl         import typep, null, listp, integerp, floatp, boolp, sequencep, stringp, mapcar, mapc,\
                       remove_if, sort, car, identity, every
from pergamum   import astp, bytesp, emptyp, ascend_tree, multiset, multiset_appendf, tuplep
from neutrality import py3p, fprintf


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
def ast_num_p(x):               return typep(x, ast.Num)
def ast_name_p(x):              return typep(x, ast.Name)
def ast_string_equalp(x, s):    return ast_string_p(x) and x.s == s
def ast_assign_p(x, to):        return typep(x, ast.Assign) and to in x.targets
def ast_module_p(x):            return typep(x, ast.Module)
def ast_def_p(x):               return typep(x, ast.FunctionDef)
def ast_import_p(x):            return typep(x, ast.Import)
def ast_import_from_p(x):       return typep(x, ast.ImportFrom)
def ast_import_maybe_from_p(x): return ast_import_p(x) or ast_import_from_p(x)
def ast_expr_p(x):              return typep(x, ast.Expr)
def ast_call_p(x):              return typep(x, ast.Call)
def ast_attribute_p(x):         return typep(x, ast.Attribute)
def ast_keyword_p(x):           return typep(x, ast.keyword)

# top-levels
def ast_module(body):
    assert listp(body) and all(mapcar(astp, body))
    return ast.Module(body=body, lineno=0)


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


# expressions
def ast_bytes(bs):
    if not bytesp(bs):
        mesg('ast_bytes(), not an immutable byte vector: %s', str(bs))
    assert bytesp(bs)
    return ast.Bytes(s=bs)


def ast_string(s):
    if not stringp(s):
        mesg('ast_string(), not a string: %s', str(s))
    assert stringp(s)
    return ast.Str(s=s)


def ast_num(n):
    assert integerp(n)
    return ast.Num(n=n)


def ast_rw(writep):
    return (ast.Store() if writep else ast.Load())


def ast_name(name, writep=False):
    assert stringp(name)
    return ast.Name(id=name, ctx=ast_rw(writep))


def ast_arg(name):
    assert stringp(name)
    return ast.Name(arg=name, ctx=ast.Param())


def ast_alias(name):
    assert stringp(name)
    return ast.alias(name=name, asname=None)


def ast_list(xs):
    assert listp(xs) and all(mapcar(astp, xs))
    return ast.List(elts=xs, ctx=ast.Load())


def ast_tuple(xs, writep=False):
    assert listp(xs) and all(mapcar(astp, xs))
    return ast.Tuple(elts=xs, ctx=ast_rw(writep))


def ast_dict(keys, values):
    return ast.Dict(keys=keys, values=values)


def ast_attribute(x, name, writep=False):
    return ast.Attribute(attr=name, value=x, ctx=ast_rw(writep))


def ast_index(of, index, writep=False):
    return ast.Subscript(value=of, slice=ast.Index(value=index), ctx=(ast.Store() if writep else ast.Load()))


def ast_maybe_normalise_string(x):
    return (ast_string(x) if stringp(x) else x)


def ast_funcall(name, *args):
    if not all(mapcar(lambda x: stringp(x) or astp(x) or x is None, args)):
        err('In call to %s: improper arglist %s', name, str(args))
    return ast.Call(func=(ast_name(name) if stringp(name) else name),
                    args=mapcar(ast_maybe_normalise_string, args),
                    keywords=[],
                    starargs=None,
                    kwargs=None)


def ast_func_name(x):
    if typep(x, ast.Name):
        return x.id
    elif typep(x, ast.Subscript):
        return ast_func_name(x.value) + '[' + ast_func_name(x.slice.value) + ']'
    elif typep(x, ast.Attribute):
        return ast_func_name(x.value) + '.' + x.attr
    else:
        return '<unhandled>'


# statements
def astlist_prog(*body):
    """WARNING: not an actual node, returns a list!"""
    return remove_if(null, body) or [ast.Pass()]


def ast_return(node):
    assert astp(node)
    return ast.Return(value=node)


def ast_expr(node):
    assert astp(node)
    return ast.Expr(value=node)


def ast_import(*names):
    assert all(mapcar(stringp, names))
    return ast.Import(names=mapcar(ast_alias, names))


def ast_import_all_from(name):
    assert stringp(name)
    return ast.ImportFrom(module=name, names=[ast.alias(name='*', asname=None)], level=0)


def ast_import_from(module_name, names):
    assert stringp(module_name)
    assert listp(names) and all(mapcar(stringp, names))
    return ast.ImportFrom(module=module_name, names=mapcar(ast_alias, names), level=0)


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


# validation
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