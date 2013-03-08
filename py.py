import cl
import primitives as p

import ast
import more_ast
import builtins
import collections
import inspect
import imp
import operator
import sys
import threading
import types

from cl import error, list_, list__, gensym, gensymname, intern, append, identity, reduce, gethash, progv as _progv, defun
from cl import find_global_variable, format, do_find_if
from cl import gensym_tn, make_keyword_tn, undefined_function
from cl import attrify_args, defaulted, defaulted_to_var, dprintf, report, pp_base_depth
from cl import compiler_debugless_traceless_frame
from cl import typep, the, check_type, consp, functionp
from cl import or_t, eql_t, string_t, pyseq_t, pytuple_t, pylist_t, symbol_t, pyanytuple_t, maybe_t, satisfies_t
from cl import integer_t, float_t, cons_t, function_t, stream_t
from cl import symbol_value, symbol_name, symbol_package, make_symbol, make_keyword
from cl import package_name, find_package
from cl import defun as _defun_, defclass as _defclass_
from cl import interpreted_function_name_symbol, lisp_symbol_name_rtname, get_function_rtname, unit_variable_rtname
from cl import consify_linear, xmap_to_vector, validate_function_args, validate_function_keys
from cl import _if, _primitive, _list, _quote, _setf, _allow_other_keys_
from cl import ir_funcall, ir_apply, ir_cl_call
from cl import _debug_io_
from cl import t, nil

from primitives import machine, prim, det, indet, body, stmt, expr
from primitives import expr_spill, maybe_expr_spill, efless, potconst, const, literal
from primitives import defmachine, defprim, maybe, satisfies
from primitives import exprp, genname

## Entire registry, essentially.  Automatable?  Worth it?
from primitives import string, symbol, literal_list
from primitives import name, integer, float_num
from primitives import add, sub, mul, div, mod, shl, shr, logior, logxor, logand, lognot, floor, expt
from primitives import eq, lt, le, gt, ge
from primitives import not_
from primitives import cons, car, cdr, rplaca, rplacd, vector, index
from primitives import assign, return_, progn, if_, loop, unwind_protect, catch, throw, resignal
from primitives import function, funcall, apply
from primitives import special_ref, special_setq, progv

NoneType = type(None)

for x in ["SETQ"]:
        globals()[x.lower()] = intern(x.lower())

def py_error(kind, control, *args):
        error("While PY-lowering %s: " + control, kind, *args)

###
### Python AST
###
##
## Toolkit
##
def astp(x):        return isinstance(x, ast.AST)

def ast_compiled_name(name, *body, function = nil, **keys):
        mod, globals, locals = py_compile_and_load(*body, **keys)
        return locals[function or name]

def coerce_to_ast_type(type_):
        return ((type_ if subtypep(type_, ast.AST) else error("Provided type %s is not a proper subtype of ast.AST", type_))
                if isinstance(type_, type) else
                (ast.__dict__[type_] if type_ in ast.__dict__ else error("Unknown AST type '%s'.", type_))
                if isinstance(type_, str)  else
                error("Invalid AST type specifier: %s, %s, %s.", type_, type, isinstance(type_, type)))

def text_ast(text):
        return compile(text, "", 'exec', flags = ast.PyCF_ONLY_AST).body

def function_ast(fn, disable_condition_system = nil):
        fn_ast = text_ast(without_condition_system(lambda: inspect.getsource(fn)) if disable_condition_system else
                          inspect.getsource(fn))[0]
        return fn_ast.args, fn_ast.body

def function_body_pass_p(fn):
        fn_body_ast = function_ast(fn)[1]
        return len(fn_body_ast) == 1 and isinstance(fn_body_ast[0], ast.Pass)

### literals
def ast_num(n):
        return ast.Num(n = the(integer_t, n))
def ast_bool(n):
        return ast.Bool(n = the(integer_t, n))
def ast_string(s):
        return ast.Str(s = the(string_t, s))
def ast_set(xs,   writep = nil):
        return ast.Set(elts   = the((pylist_t, ast.AST), xs), ctx = ast_rw(writep))
def ast_list(xs,  writep = nil):
        return ast.List(elts  = the((pylist_t, ast.AST), xs), ctx = ast_rw(writep))
def ast_tuple(xs, writep = nil):
        return ast.Tuple(elts = the((pylist_t, ast.AST), xs), ctx = ast_rw(writep))

__ast_efless_types__                       = { ast.Num, ast.Str, ast.Name, ast.Bytes }
__ast_potentially_efless_recursive_types__ = { ast.Tuple:  lambda x: x.elts,
                                               ast.List:   lambda x: x.elts,
                                               ast.Set:    lambda x: x.elts,
                                               ast.Dict:   lambda x: x.keys + x.values,
                                               }
def ast_efless_p(x):
        "This is fairly conservative."
        ty = type(x)
        return (ty in __ast_efless_types__
                or (ty in __ast_potentially_efless_recursive_types__
                    and all(ast_efless_p(x)
                            for x in __ast_potentially_efless_recursive_types__[ty](x))))

################################# recurse? AST-ifier
__astifier_map__ = { str:       (nil, ast_string),
                     int:       (nil, ast_num),
                     bool:      (nil, ast_num),
                     NoneType:  (nil, lambda x: ast_name("None")),
                     list:      (t,   ast_list),
                     tuple:     (t,   ast_tuple),
                     set:       (t,   ast_set),
                     ## symbol: see below
                     }
def register_astifier_for_type(type, recurse, astifier):
        "Please, list the added astifiers above."
        __astifier_map__[type] = (recurse, astifier)

def unregister_astifier_for_type(type):
        del __astifier_map__[type]

def astifiable_p(x):
        return type(x) in __astifier_map__

def try_astify_constant(x):
        if astp(x):
                return x, t
        (rec, astifier), astifiable = gethash(type_of(x), __astifier_map__,
                                              ((nil, nil), nil))
        return (astifier([ astify_constant(x) for x in x ] if rec else
                         x), t) if astifiable else (None, None)

def astify_constant(x):
        ast, successp = try_astify_constant(x)
        return (ast if successp else
                error("Cannot convert value %s to AST.  Is it a literal?",
                      prin1_to_string(x)))

def coerce_to_ast(x):
        return astify_constant(x) if not astp(x) else x

### expressions
def ast_funcall(name, args = [], keys = {}, starargs = None, kwargs = None):
        check_type(args, (pylist_t, (or_t, ast.AST, NoneType, (satisfies_t, astifiable_p))))
        return ast.Call(func = (ast_name(name) if isinstance(name, str) else name),
                        args = [ coerce_to_ast(x) for x in args ],
                        keywords = [ ast_keyword(k, v) for k, v in keys.items() ],
                        starargs = starargs or None,
                        kwargs = kwargs or None)

def ast_and(*args):
        return ast.BoolOp(ast.And(), list(args))

def ast_or(*args):
        return ast.BoolOp(ast.Or(), list(args))

### statements
def ast_import(*names):
        return ast.Import(names = [ _ast_alias(x) for x in the((homotuple_t, string_t), names) ])
def ast_import_from(module_name, names):
        return ast.ImportFrom(module = the(string_t, module_name),
                              names = [ _ast_alias(x) for x in the((pylist_t, string_t), names) ],
                              level = 0)

def ast_assign(to, value):
        return ast.Assign(targets = the((pylist_t, ast.AST), to),
                          value = the(ast.AST, value))
def ast_return(node):
        return ast.Return(value = the(ast.AST, node))

## ast.FunctionDef tools
#
# arguments = (arg* args, identifier? vararg, expr? varargannotation,
#              arg* kwonlyargs, identifier? kwarg,
#              expr? kwargannotation, expr* defaults,
#              expr* kw_defaults)
#
# arg = (identifier arg, expr? annotation)
#
# keyword = (identifier arg, expr value)

def argspec_nfixargs(paramspec):
        return len(paramspec.args) - len(paramspec.defaults or []) # ILTW Python implementors think..

def argspec_lambda_spec(spec, astify_defaults = t):
        # args, varargs, varkw, defaults, kwonlyargs, kwonlydefaults, annotations
        nfixargs = argspec_nfixargs(spec)
        default_xform = astify_constant if astify_defaults else identity
        return (spec.args[:nfixargs],
                list(zip(spec.args[nfixargs:],
                         [ default_xform(x) for x in spec.defaults or [] ])),
                spec.varargs,
                list(zip(spec.kwonlyargs,
                         [ default_xform(x) for x in spec.kwonlydefaults or [] ])),
                spec.varkw)

def function_lambda_list(fn, astify_defaults = t):
        "Returns: FIXPARMS, OPTIONAL-WITH-DEFAULTS, VARARGS, KEYS-WITH-DEFAULTS, KWARGS."
        return argspec_lambda_spec(inspect.getfullargspec(fn), astify_defaults = astify_defaults)

##
## DEFAST
##
ast_info = collections.namedtuple("_ast_info",
                                  ["type",
                                   "fields",     # each field is dict(name, type, walk, [default])
                                   "nfixed"])
__ast_infos__                = dict()
def find_ast_info(type):     return __ast_infos__[type]

def defast(fn):
        ### generic tools
        def lambda_list_names(lambda_list, remove_optional = t):
                (fixed, optional, args, keyword, keys) = lambda_list
                xform = (lambda x: x[0]) if remove_optional else identity
                return (tuple(fixed) +
                        tuple(xform(x) for x in optional) + (() if not args else (args,)) +
                        tuple(xform(x) for x in keyword)  + (() if not keys else (keys,)))
        ### end-of-generic-tools
        def validate_defast_name(name):
                if not name.startswith("ast_"):
                        error("In DEFAST %s: the AST name must be prefixed with \"ast_\"", name)
                name = name[4:]
                ast_type, therep = gethash(name, ast.__dict__)
                if not therep:
                        error("In DEFAST %s: '%s' does not denote a known AST type", name, name)
                return name, ast_type
        name, ast_type = validate_defast_name(fn.__name__)
        def validate_defast_lambda_list(ast_type, lambda_list, annotations):
                (fixed, optional, args, keyword, keys) = lambda_list
                if args or keyword or keys:
                        err("only fixed and optional arguments are allowed")
                ast_field_names = fixed + [ x[0] for x in optional ]
                ast_field_names_with_defaults = fixed + optional
                ast_field_types = [ annotations[name] for name in ast_field_names ]
                if len(ast_field_types) != len(ast_type._fields):
                        error("In DEFAST %s:the amount of provided type specifiers (%d) does not match the AST fields: %s",
                              name, len(ast_field_types), ast_type._fields)
                type_specifier_type = (or_t, pyanytuple_t, type)
                if not all(typep(x, type_specifier_type) for x in ast_field_types):
                        mismatched = [ x for x in ast_field_types
                                       if not typep(x, type_specifier_type) ]
                        error("In DEFAST %s: the AST field type specifiers must be of type %s, found: %s",
                              name, type_specifier_type, ", ".join(str(x) for x in mismatched))
                for i, (fname, ast_fname) in enumerate(zip(ast_field_names, ast_type._fields)):
                        if fname != ast_fname:
                                error("In DEFAST %s: the provided name for the %d'th field (%s) does not match its actual name (%s), expected field names: %s",
                                      name, i, fname, ast_fname, ast_type._fields)
                return ast_field_types
        def arglist_field_infos(parameters, nfix, with_defaults, ast_field_types):
                ## Used to be:  fields = without_condition_system(lambda: collections.OrderedDict())
                fields = collections.OrderedDict()
                def process_ast_field_arglist_entry(name, type, default, fixed = t):
                        fields[p] = (dict(name = name, type = type) if fixed else
                                     dict(name = name, type = type, default = default))
                for p, type, defaulted in zip(parameters[:nfix], ast_field_types[:nfix], with_defaults[:nfix]):
                        process_ast_field_arglist_entry(p, type, None,         fixed = t)
                for p, type, defaulted in zip(parameters[nfix:], ast_field_types[nfix:], with_defaults[nfix:]):
                        process_ast_field_arglist_entry(p, type, defaulted[1], fixed = nil)
                return fields
        lambda_list = (fixed, optional, args, keyword, keys) = function_lambda_list(fn, astify_defaults = nil)
        ast_field_types = validate_defast_lambda_list(ast_type, lambda_list, fn.__annotations__)
        parameters, with_defaults = (lambda_list_names(lambda_list),
                                     lambda_list_names(lambda_list, remove_optional = nil))
        args_ast, body_ast = function_ast(fn)
        fields = arglist_field_infos(parameters, len(fixed), with_defaults, ast_field_types)
        __ast_infos__[ast_type] = ast_info(type       = ast_type,
                                            fields     = fields,
                                            nfixed     = len(fixed))

## AST + Symbols
register_astifier_for_type(symbol_t, nil, (lambda sym: ast_funcall("find_symbol_or_fail", [symbol_name(sym)])))

##
## Definitions
##
# mod = Module(stmt* body)
#     | Interactive(stmt* body)
#     | Expression(expr body)
@defast
def ast_Module(body: (pylist_t, ast.stmt)): pass
@defast
def ast_Interactive(body: (pylist_t, ast.stmt)): pass
@defast
def ast_Expression(body: ast.expr): pass
# stmt = FunctionDef(identifier name,
#                    arguments args,
#                    stmt* body,
#                    expr* decorator_list,
#                    expr? returns)
# >>> a = 0
# >>> def damage():
# ...         a = 42
# ...         def inner():
# ...                 if 2:
# ...                         a = 1
# ...                 if 0:
# ...                         global a
# ...                 # elif 0: # errs: SyntaxError: name 'a' is nonlocal and global
# ...                 #         nonlocal a
# ...         inner()
# ...
# <stdin>:7: SyntaxWarning: name 'a' is assigned to before global declaration
# >>> damage()
# >>> print(a)
# 1
# >>> def damage():
# ...         a = 42
# ...         def inner(a):
# ...                 if 2:
# ...                         a = 1
# ...                 if 0:
# ...                         global a
# ...                 # elif 0: # errs: SyntaxError: name 'a' is nonlocal and global
# ...                 #         nonlocal a
# ...         inner(1)
# ...
# <stdin>:7: SyntaxWarning: name 'a' is assigned to before global declaration
#   File "<stdin>", line 3
# SyntaxError: name 'a' is parameter and global
@defast
def ast_FunctionDef(name:            string_t,
                     args:            ast.arguments,
                     body:           (pylist_t, ast.stmt),
                     decorator_list: (pylist_t, ast.expr) = list(),
                     returns:        (maybe_t,  ast.expr) = None): ...
#       | ClassDef(identifier name,
#                  expr* bases,
#                  keyword* keywords,
#                  expr? starargs,
#                  expr? kwargs,
#                  stmt* body,
#                  expr* decorator_list)
@defast
def ast_ClassDef(name:            string_t,
                  bases:          (pylist_t, ast.expr),
                  keywords:       (pylist_t, ast.keyword),
                  starargs:       (maybe_t,  ast.expr),
                  kwargs:         (maybe_t,  ast.expr),
                  body:           (pylist_t, ast.stmt),
                  decorator_list: (pylist_t, ast.expr)): ...
#       | Return(expr? value)
@defast
def ast_Return(value: (maybe_t, ast.expr)): pass
#       | Delete(expr* targets)
@defast
def ast_Delete(targets: (pylist_t, ast.expr)): pass
        # targets do ref, in this case!
#       | Assign(expr* targets, expr value)
@defast
def ast_Assign(targets: (pylist_t, ast.expr),
                value:    ast.expr): ...
#       | AugAssign(expr target, operator op, expr value)
@defast
def ast_AugAssign(target: ast.expr,
                   op:     ast.operator,
                   value:  ast.expr): ...

#       | For(expr target, expr iter, stmt* body, stmt* orelse)
@defast
def ast_For(target:  ast.expr,
             iter:    ast.expr,
             body:   (pylist_t, ast.stmt),
             orelse: (pylist_t, ast.stmt)): ...

#       | While(expr test, stmt* body, stmt* orelse)
@defast
def ast_While(test:    ast.expr,
               body:   (pylist_t, ast.stmt),
               orelse: (pylist_t, ast.stmt)): ...

#       | If(expr test, stmt* body, stmt* orelse)
@defast
def ast_If(test:    ast.expr,
            body:   (pylist_t, ast.stmt),
            orelse: (pylist_t, ast.stmt)): ...

#       | Raise(expr? exc, expr? cause)
@defast
def ast_Raise(exc:   (maybe_t, ast.expr),
               cause: (maybe_t, ast.expr)): pass

#       | Assert(expr test, expr? msg)
@defast
def ast_Assert(test: ast.expr,
                msg:  ast.expr = None): pass
#       | Import(alias* names)
@defast
def ast_Import(names: (pylist_t, ast.alias)):
        declare((walk, names))
#       | ImportFrom(identifier? module, alias* names, int? level)
@defast
def ast_ImportFrom(module: (maybe_t, string_t),
                    names:  (pylist_t, ast.alias),
                    level:  (maybe_t, integer_t)): ...

#       | Global(identifier* names)
@defast
def ast_Global(names: (pylist_t, string_t)): ...

#       | Nonlocal(identifier* names)
@defast
def ast_Nonlocal(names: (pylist_t, string_t)): ...

#       | Expr(expr value)
@defast
def ast_Expr(value: ast.expr): pass
#       | Pass | Break | Continue
@defast
def ast_Pass(): pass
@defast
def ast_Break(): pass
@defast
def ast_Continue(): pass
# expr = BoolOp(boolop op, expr* values)
@defast
def ast_BoolOp(op:      ast.boolop,
                values: (pylist_t, ast.expr)): pass
#      | BinOp(expr left, operator op, expr right)
@defast
def ast_BinOp(left:  ast.expr,
               op:    ast.operator,
               right: ast.expr): pass
#      | UnaryOp(unaryop op, expr operand)
@defast
def ast_UnaryOp(op:      ast.unaryop,
                 operand: ast.expr): pass
#      | Lambda(arguments args, expr body)
@defast
def ast_Lambda(args: ast.arguments,
                body: ast.expr): ...

#      | IfExp(expr test, expr body, expr orelse)
@defast
def ast_IfExp(test:   ast.expr,
               body:   ast.expr,
               orelse: ast.expr): pass
#      | Dict(expr* keys, expr* values)
@defast
def ast_Dict(keys:   (pylist_t, ast.expr),
              values: (pylist_t, ast.expr)): pass
#      | Set(expr* elts)
@defast
def ast_Set(elts: (pylist_t, ast.expr)): pass
#      | ListComp(expr elt, comprehension* generators)

@defast
def ast_ListComp(elt:         ast.expr,
                  generators: (pylist_t, ast.comprehension)): ...

#      | SetComp(expr elt, comprehension* generators)
@defast
def ast_SetComp(elt:         ast.expr,
                 generators: (pylist_t, ast.comprehension)): ...

#      | DictComp(expr key, expr value, comprehension* generators)
@defast
def ast_DictComp(key:        ast.expr,
                  value:      ast.expr,
                  generators: (pylist_t, ast.comprehension)): ...

#      | GeneratorExp(expr elt, comprehension* generators)
@defast
def ast_GeneratorExp(elt:         ast.expr,
                      generators: (pylist_t, ast.comprehension)): ...

#      | Yield(expr? value)
@defast
def ast_Yield(value: (maybe_t, ast.expr) = None): pass

#      | Compare(expr left, cmpop* ops, expr* comparators)
@defast
def ast_Compare(left:         ast.expr,
                 ops:         (pylist_t, ast.cmpop),
                 comparators: (pylist_t, ast.expr)): pass
#      | Call(expr func, expr* args, keyword* keywords, expr? starargs, expr? kwargs)
@defast
def ast_Call(func:      ast.expr,
              args:     (pylist_t, ast.expr),
              keywords: (pylist_t, ast.keyword),
              starargs: (maybe_t, ast.expr) = None,
              kwargs:   (maybe_t, ast.expr) = None): ...

#      | Num(object n) -- a number as a PyObject.
@defast
def ast_Num(n: (or_t, integer_t, float_t)): pass
#      | Str(string s) -- need to specify raw, unicode, etc?
@defast
def ast_Str(s: string_t): pass
#      | Bytes(string s)
@defast
def ast_Bytes(s: string_t): pass
#      | Ellipsis
@defast
def ast_Ellipsis(): pass
#      | Attribute(expr value, identifier attr, expr_context ctx)
@defast
def ast_Attribute(value: ast.expr,
                   attr:  string_t,
                   ctx:   ast.expr_context): pass
#      | Subscript(expr value, slice slice, expr_context ctx)
@defast
def ast_Subscript(value: ast.expr,
                   slice: ast.slice,
                   ctx:   ast.expr_context):
        declare((walk, slice))
#      | Starred(expr value, expr_context ctx)
@defast
def ast_Starred(value: ast.expr,
                 ctx:   ast.expr_context): pass
#      | Name(identifier id, expr_context ctx)
@defast
def ast_Name(id:  string_t,
              ctx: ast.expr_context): ...

#      | List(expr* elts, expr_context ctx)
@defast
def ast_List(elts: (pylist_t, ast.expr),
              ctx:   ast.expr_context): pass
#      | Tuple(expr* elts, expr_context ctx)
@defast
def ast_Tuple(elts: (pylist_t, ast.expr),
               ctx:   ast.expr_context): pass
# expr_context = Load | Store | Del | AugLoad | AugStore | Param
@defast
def ast_Load(): pass
@defast
def ast_Store(): pass
@defast
def ast_AugLoad(): pass
@defast
def ast_AugStore(): pass
@defast
def ast_Param(): pass
# slice = Slice(expr? lower, expr? upper, expr? step)
@defast
def ast_Slice(lower: (maybe_t, ast.expr) = None,
               upper: (maybe_t, ast.expr) = None,
               step:  (maybe_t, ast.expr) = None): pass
#       | ExtSlice(slice* dims)
@defast
def ast_ExtSlice(dims: (pylist_t, ast.slice)):
        declare((walk, dims))
#       | Index(expr value)
@defast
def ast_Index(value: ast.expr): pass
# boolop = And | Or
@defast
def ast_And(): pass
@defast
def ast_Or(): pass
# operator = Add | Sub | Mult | Div | Mod | Pow | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv
@defast
def ast_Add(): pass
@defast
def ast_Sub(): pass
@defast
def ast_Mult(): pass
@defast
def ast_Div(): pass
@defast
def ast_Mod(): pass
@defast
def ast_Pow(): pass
@defast
def ast_LShift(): pass
@defast
def ast_RShift(): pass
@defast
def ast_BitOr(): pass
@defast
def ast_BitXor(): pass
@defast
def ast_BitAnd(): pass
@defast
def ast_FloorDiv(): pass
# unaryop = Invert | Not | UAdd | USub
@defast
def ast_Invert(): pass
@defast
def ast_Not(): pass
@defast
def ast_UAdd(): pass
@defast
def ast_USub(): pass
# cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
@defast
def ast_Eq(): pass
@defast
def ast_NotEq(): pass
@defast
def ast_Lt(): pass
@defast
def ast_LtE(): pass
@defast
def ast_Gt(): pass
@defast
def ast_GtE(): pass
@defast
def ast_Is(): pass
@defast
def ast_IsNot(): pass
@defast
def ast_In(): pass
@defast
def ast_NotIn(): pass
# comprehension = (expr target, expr iter, expr* ifs)
@defast
def ast_comprehension(target: ast.expr,
                       iter:   ast.expr,
                       ifs:   (pylist_t, ast.expr)): ...

# excepthandler = ExceptHandler(expr? type, identifier? name, stmt* body)
@defast
def ast_ExceptHandler(type: (maybe_t, ast.expr),
                       name: (maybe_t, string_t),
                       body: (pylist_t, ast.stmt)): ...

# arguments = (arg* args, identifier? vararg, expr? varargannotation,
#              arg* kwonlyargs, identifier? kwarg,
#              expr? kwargannotation, expr* defaults,
#              expr* kw_defaults)
@defast
### These MAYBEs suggest a remapping facility.
def ast_arguments(args:             (pylist_t, ast.arg),
                   vararg:           (maybe_t, string_t),
                   varargannotation: (maybe_t, ast.expr),
                   kwonlyargs:       (pylist_t, ast.arg),
                   kwarg:            (maybe_t, string_t),
                   kwargannotation:  (maybe_t, ast.expr),
                   defaults:         (pylist_t, ast.expr),
                   kw_defaults:      (pylist_t, ast.expr)): ...

# arg = (identifier arg, expr? annotation)
@defast
def ast_arg(arg:         string_t,
             annotation: (maybe_t, ast.expr) = None): ...

# keyword = (identifier arg, expr value)
@defast
def ast_keyword(arg:   string_t,
                 value: ast.expr): ...

# alias = (identifier name, identifier? asname)
@defast
def ast_alias(name:    string_t,
               asname: (maybe_t, string_t) = None): ...

# Validation: %AST-VALIDATE

__ast_walkable_field_types__ = set([ast.stmt, (pylist_t, ast.expr), (maybe_t, ast.expr),
                                    ast.expr, (pylist_t, ast.stmt)])
def ast_info_check_args_type(info, args):
        if len(args) < info.nfixed:
                error("AST type '%s' requires %s %d arguments, but only %d were provided: %s.",
                      info.type.__name__, "exactly" if len(info.fields) == info.nfixed else "at least", info.nfixed,
                      len(args), args)
        for i, (field, arg) in enumerate(zip(info.fields.values(), args)):
                if not typep(arg, field["type"]):
                        error("Argument %d (field %s) of AST '%s' must correspond to type %s, but was an instance of %s, instead: %s.",
                              i, repr(field["name"]), info.type.__name__, field["type"], type_of(arg), repr(arg))
        return t

def ast_validate(_ast):
        def rec(x, context):
                if isinstance(x, (str, int, float, NoneType)):
                        return
                if not isinstance(x, ast.AST):
                        error("%s %s is invalid: not an AST, where an AST was expected.", context, x)
                args = [ (field, getattr(x, field))
                         for field in x._fields
                         if hasattr(x, field) ]
                missing_args = [ field for field in x._fields
                                 if not hasattr(x, field) ]
                info = find_ast_info(type(x))
                if missing_args:
                        error("Argument field %s of AST '%s' is missing.",
                              repr(missing_args[0]), info.type.__name__)
                ast_info_check_args_type(info, [ fval for _, fval in args ])
                for field, ixs in args:
                        for ix in ixs if isinstance(ixs, list) else [ixs]:
                                rec(ix, "ast.%s.%s" % (type(x).__name__, field))
        rec(_ast, "root AST")
        return _ast

###
### AST fixery
###
fixupp = gensym("FIXUPP")

class name_context_fixer(ast.NodeTransformer):
        def visit_Name(w, o):
                return (ast.Name(o.id, ast.Store()) if symbol_value(fixupp) else
                        o)
        def visit_Assign(w, o):
                with _progv({ fixupp: t }):
                        targets = [ w.visit(x)
                                    for x in o.targets ]
                return ast.Assign(targets = targets,
                                   value = w.visit(o.value))
        def visit_AugAssign(w, o):
                with _progv({ fixupp: t }):
                        target = w.visit(o.target)
                return ast.AugAssign(target = target,
                                      op = o.op,
                                      value = w.visit(o.value))
        def visit_For(w, o):
                with _progv({ fixupp: t }):
                        target = w.visit(o.target)
                return ast.For(target = target,
                                iter = w.visit(o.iter),
                                body = [ w.visit(x) for x in o.body ],
                                orelse = [ w.visit(x) for x in o.orelse ])
        def visit_With(w, o):
                with _progv({ fixupp: t }):
                        wis = [ (w.visit(wi.optional_vars) if wi.optional_vars else None)
                                for wi in o.items ]
                return ast.With(items = [ ast.withitem(w.visit(o.items[0].context_expr),
                                                       wis[0]) ], ## WARNING: ignoring non-first with items
                                body  = [ w.visit(x) for x in o.body ])
        def visit_comprehension(w, o):
                with _progv({ fixupp: t }):
                        target = w.visit(o.target)
                return ast.comprehension(target = target,
                                          iter = w.visit(o.iter),
                                          ifs = [ w.visit(x) for x in o.ifs ])
        def visit_Subscript(w, o):
                writep = symbol_value(fixupp)
                with _progv({ fixupp: nil }):
                        return ast.Subscript(value = w.visit(o.value),
                                              slice = w.visit(o.slice),
                                              ctx = ast.Store() if writep else o.ctx)
        def visit_Attribute(w, o):
                writep = symbol_value(fixupp)
                with _progv({ fixupp: nil }):
                        return ast.Attribute(value = w.visit(o.value),
                                              attr = o.attr,
                                              ctx = ast.Store() if writep else o.ctx)

name_context_fixer = name_context_fixer()

###
### Primitives: lowering to Python AST
###
def find_indet_method_pool(indet_name):
        return p.__primitives_by_pyname__[indet_name].methods

def defmethod(prim, *tags):
        def do_defmethod(method):
                ## Unregistered Issue METHOD-NEED-DIFFERENTIATION
                primitive_method_pool = find_indet_method_pool(method.__name__)
                for tag in tags: ## HOW DO YOU FUCKING ACCESS THE CLASS OF THE METHOD IT IS BEING DEFINED ON?  NO WAI.
                        primitive_method_pool[tag].add(method)
                return method
        return do_defmethod

def identity_method(*keys):
        return (identity, keys) # just a marker, to be processed by defprim

def defstrategy(*ign, test = lambda *_, **__: True, keys = None, xform = None):
        assert(not ign)
        assert(keys or xform)
        return (defstrategy, test, (xform  if xform                  else
                                    keys   if isinstance(keys, list) else
                                    [keys]))

def defpy(cls):
        def maybe_process_as_strategy(cls, name, spec):
                def strategyp(x): return (isinstance(x, tuple) and x and x[0] is defstrategy and x[1:]
                                          #         See defstrategy above, for the definition of x[1:]
                                          or (None, None))
                # dprintf("trying to process %s as strategy in %s", spec, cls)
                test, xform_or_keys = strategyp(spec)
                # Due to the definition of defstrategy, test cannot be a false-equivalent.
                if test and xform_or_keys:
                        # dprintf("  DEFPY %s, setting strategy %s to %s", cls, name, (test, xform_or_keys))
                        # implementation strategy
                        cls.help_strategies.append((name, test, xform_or_keys))
                        cls.help_strategies.sort()
                        return t
        def primitive_add_method_keys(primitive_method_pool, method, keys):
                for key in keys:
                        primitive_method_pool[key].add(method)
        clsname = cls.__name__
        prim_maybe_cls = getattr(p, clsname, None)
        this, cls = cls, (prim_maybe_cls if isinstance(prim_maybe_cls, type) else
                          globals().get(clsname, None))
        if not cls:
                error("In DEFPY %s: unknown primitive.", clsname.upper())
        # dprintf("DEFPY: found %s (of type %s) for method stuffing", cls, type(cls))
        class_key_supers = cls.__mro__[0:cls.__mro__.index(prim)]
        help_stdmethod = cls.__dict__.get("help", None)
        # dprintf("DEFPY %s/%x:   %s", cls, id(cls), cls.__dict__.items())
        for n, method_spec in this.__dict__.items():
                if (n in ("__module__", "__locals__", "__doc__", "__new__", "__qualname__",
                          "methods", "help_strategies", "form_specifier", "help", "value", "cfg") or
                    maybe_process_as_strategy(cls, n, method_spec)):
                        if n is "help":
                                # decorating all the stuff into staticmethod in-text would be a real shame
                                cls.help = staticmethod(method_spec)
                        continue
                ## otherwise, must be an indet method
                method, identityp, keys = \
                    ((method_spec,    nil, ())  if isinstance(method_spec, (types.FunctionType,
                                                                            builtins.staticmethod)) else
                     (cls, t,   method_spec[1]) if (isinstance(method_spec, tuple) and
                                                               method_spec[0] is identity) else
                     error("Invalid method specifier: %s", method_spec))
                indet_method_pool = find_indet_method_pool(n)
                primitive_add_method_keys(indet_method_pool, method, keys)
                primitive_add_method_keys(indet_method_pool, method, class_key_supers)
        return globals().get(clsname) ## Preserve original globals, if any.

def find_method(cls, tags):
        "Find *the* single method matching all tags."
        assert(tags)
        this, *rest = tags
        sett = set(cls.methods[this]) ## The lack of the 'everything' set.  Oh.
        while rest:
                this, *rest = rest
                sett &= cls.methods[this]
        if not sett:
                error("Could not find primitive method %s for tags %s.", cls.__name__, tags)
        if len(sett) > 1:
                error("Ambiguous method specification: primitive %s for tags %s.", cls.__name__, tags)
        return sett.pop()
p.prim.find_method = classmethod(find_method)

def determine(cls, args, keys):
        for name, test, xform_or_keys in cls.help_strategies:
                desc = "applicability predicate for method %s.%s" % (cls.__name__.upper(), name)
                validate_function_args(desc, test, args)
                validate_function_keys(desc, test, keys)
                if test(*args, **keys):
                        ## Simplify.
                        xf = (xform_or_keys if not isinstance(xform_or_keys, list) else
                              cls.find_method(xform_or_keys))
                        # if cls is progn:
                        #         # dprintf("PROGN-DET args %s", args)
                        # dprintf("xf %s", xf)
                        return xf(*args, **keys)
        else:
                error("Unhandled primitive form: %s / %s", (cls.__name__.upper(),) + args, keys)

def redetermining_as(cls, **keys):
        return lambda x: determine(cls, x.args, keys)

_compiler_trace_primitives_ = cl._compiler_trace_primitives_

def help(x) -> ([stmt], expr):
        if not isinstance(x, prim):
                return [], x
        with _progv({ cl._pp_base_depth_: pp_base_depth() + 3 }):
                meth_name = type(x).__name__.upper()
                def handler(cond):
                        error("While calling %s.%s, caught:\n%s", meth_name, x.help, cond)
                validate_function_keys("%s.HELP" % meth_name, x.help, x.keys)
                validate_function_args("%s.HELP" % meth_name, x.help, x.args)
                r = x.help(*x.args, **x.keys)
                # r = handler_bind(lambda: x.help(*x.args, **x.keys),
                #                  (Exception, handler))
        p, v = (([], r) if not isinstance(r, ast.stmt) and isinstance(r, ast.AST) else
                ## list(r) if isinstance(r, tuple) else
                ## Unregistered Issue SLOW-CHECK
                r if typep(r, (pytuple_t, (pylist_t, ast.stmt), ast.expr))        else
                error("Invalid output from lowerer for %s (%s/%s)\n-- %s.", x, x.help, defun.help, r))
        if not isinstance(x, name) and symbol_value(cl._compiler_trace_subastification_):
                ssp = sex_space()
                dprintf("%s---- helpery %s --->\n"
                           "%s%s\n"
                           "%s%s\n",
                           ssp, pp_chain_of_frame(caller_frame(-1), callers = 15),
                           ssp, x,
                           ssp, ("\n" + ssp).join(pp_ast_as_code(x) for x in p + [v]))
        return p or [], v

def help_expr(x) -> expr:
        p, v = help(x)
        p and error("Helped %s to non-expr %s, %s, where an expression result was expected.", x, p, v)
        return v

def help_exprs(xs) -> [expr]:
        return [ help_expr(x) for x in xs ]

def help_prog(xs) -> [stmt]:
        p_acc = []
        for x in the((pyseq_t, prim), xs):
                p, v = help(x)
                p_acc.extend(p)
                if not ast_efless_p(v):
                        p_acc.append(ast.Expr(v))
        return p_acc

def help_progn(xs) -> ([stmt], expr):
        assert(xs)
        xs, vf = xs[:-1], xs[-1]
        p_acc = help_prog(xs)
        p, v = help(vf)
        p_acc.extend(p)
        return p_acc, v

def help_args(fixed, opts, optvals, args, keys, keyvals, restkey):
        assert(len(opts) == len(optvals) and
               len(keys) == len(keyvals))
        return ast.arguments(
                args             = [ ast.arg(x.value(), None) for x in fixed + opts ],
                vararg           = args.value()    if args    else None,
                varargannotation = None,
                kwonlyargs       = [ ast.arg(x.value(), None) for x in         keys ],
                kwarg            = restkey.value() if restkey else None,
                kwargannotation  = None,
                defaults         = help_exprs(optvals),
                kw_defaults      = help_exprs(keyvals))

def help_ctx(writep):
        return (ast.Store if writep else ast.Load)()

def help_nil():
        return help(p.prim_nil() if symbol_value(p._valueless_primitive_statement_must_yield_nil_) else
                    name("None"))[1]
###
### Spilling
###
##
## Spill theory
##
## Q1: what end goals are we trying to attain?
## seeming candidates are:
## - determine indeterminate primitives
##   - maximise the use of expressions
##   - punt to statements where we cannot
## - preserve the correct order and time of evaluation
##
## Problematic primitives.  Basically, anything with pieces having different time of evaluation.
##
## (IF spillable unspillable unspillable)
##   ..we still need to spill unspillables, but.. thunk allocation, in normal mode, just to delay..
##   Better to solve unspillables through stmt form.
##   ..still we'd like to be able to spill the spillable, independent of the expr-ness of the chosen
##   primitive.
##
## (LAMBDA (const &OPTIONAL (const spillable)...) unspillable)
##   same logic for unspillables leading to a stmt form.
##   The spillables present a problem for preservation of evaluation order.
##
## The transient logic of skies:
##
## HELP needs final expr-ness for dispatch decision making.
## Exprness, thus, must be an immediate property of the primitive.
## Exprness is a product of the primitive's kind and its spills.
## Expression spills is a recursive property.
## Spill computation is expensive and needs to be obtained at different nest levels, and thus must be cached.
## Primitive, thus, at the time it reaches this stage, must be an object, to store the cached property.
## Only applicatively positioned subforms can be conveniently spilled (proven to be true only partially).
##
def prim_check_and_spill(primitive) -> prim:
        def check_prim_type(arg, type) -> bool:
                if not typep(arg, type):
                        raise p.primitive_mismatch("type mismatch",
                                                   prim = primitive, pspec = primitive.form_specifier,
                                                   spec = type, form = arg)
        def tuple_spills(spec, args, force_spill) -> ([stmt], [expr]):
                specialp = spec and spec[0] in [maybe, satisfies]
                if specialp:
                        if spec[0] is maybe:
                                if args is None:
                                        return [], None
                                return process(spec[1], args, force_spill)
                        if spec[0] is satisfies:
                                return type_check((satisfies_t, spec[1]), args, None)
                segmentp = spec and isinstance(spec[-1], list)
                check_prim_type(args, (or_t, tuple, list))
                nspec, nargs = len(spec), len(args)
                if not (segmentp and nargs >= (nspec - 1)
                        or nspec is nargs):
                        error("Invalid primitive %s: subform %s has %d elements, but %s was/were expected.",
                              primitive, args, nargs,
                              ("at least %s" % (nspec - 1)) if segmentp else
                              ("exactly %d" % nspec))
                a_fixed, a_segment, s_spec = ((args[:nspec - 1], args[nspec - 1:], spec[-1][0]) if segmentp else
                                              (args,             [],               None))
                def expr_tuple_spill_partition(spec, args):
                        pre_spills = []
                        for s, a in zip(spec, a_fixed):
                                pre_spills.append(process(s,      a, force_spill))
                        for a in a_segment:
                                pre_spills.append(process(s_spec, a, force_spill))
                        ## only allowable spills will land here
                        last_spilled_posns = [ i
                                               for i, x in reversed(list(enumerate(pre_spills)))
                                               if x[0] ]
                        last_spilled_posn = last_spilled_posns[0] if last_spilled_posns else None
                        n_spilled = (last_spilled_posn + 1 if last_spilled_posn is not None else
                                     0)
                        for_spill, unspilled = args[:n_spilled], args[n_spilled:]
                        n_segspill = max(0, n_spilled - len(a_fixed))
                        return for_spill, ((spec[:-1] + (s_spec,) * n_segspill)
                                           if n_segspill else
                                           spec[:n_spilled]), unspilled
                for_spill_as, for_spill_ss, unspilled = expr_tuple_spill_partition(spec, args)
                ## Re-collecting spills, while forcing spill for unspilled spillables.
                spills, subforms = [], []
                for s, a in zip(for_spill_ss, for_spill_as):
                        spill, subform = process(s, a, for_spill_as)
                        spills.extend(spill)
                        subforms.append(subform)
                return (spills,
                        tuple(subforms) + tuple(unspilled))
        def maybe_spill(spec, arg, force_spill) -> ([stmt], prim):
                maybep = spec is maybe_expr_spill
                check_prim_type(arg, ((or_t, (eql_t, nil), expr, stmt) if maybep else
                                      (or_t, expr, stmt)))
                # Debug this: if (isinstance(arg, expr) and (not force_spill or (maybep and arg is nil))):
                ## Spill, iff any of the conditions hold:
                if (maybep and arg is nil
                    or not (force_spill
                            or isinstance(arg, stmt))):
                        return ([],
                                arg)
                tn = genname("EXP") ## Temporary Name.
                return ([ assign(tn, arg) ],
                        tn)
        def type_check(spec, arg, force_spill) -> ([stmt], t):
                check_prim_type(arg, spec)
                return ([],
                        arg)
        def process(spec, arg, force_spill) -> ([stmt], (or_t, prim, [expr])):
                isinstance(spec, list) and error("List type specifier (%s), outside of appropriate context.", spec)
                return (maybe_spill  if spec in (expr_spill, maybe_expr_spill) else
                        tuple_spills if isinstance(spec, tuple)                else
                        type_check)(spec, arg, force_spill)
        spills, primitive.args  = tuple_spills(primitive.form_specifier, primitive.args, nil)
        return (progn(*spills + [primitive]) if spills else
                primitive)

####
#### Python-specific IR definitions
####
###
### Handled by non-immediate constant separation pass.
###
## STRING SYMBOL LITERAL-LIST
@defpy
class string(literal):
        def help(name): return ast.Str(name)

@defpy
class symbol(literal):
        def value(x, writep = nil):
                return x
        def help(x, writep = nil):
                return ast.Name(x, help_ctx(writep))

@defpy
class literal_list(literal):
        def help(*xs):
                return reduce(lambda cdr, car: ast.List([help_expr(car), cdr], help_ctx(nil)),
                              ## Namespace separation leak:
                              reversed(xs + (help_nil(),)))

###
### Immediate values
###
## NAME INTEGER FLOAT-NUM
@defpy
class name(expr):
        def help(x, writep = nil, globalp = nil):
                return ast.Name(x, help_ctx(writep))

@defpy
class integer(literal):
        def help(x): return ast.Num(x)

@defpy
class float_num(literal):
        def help(x): return ast.Num(x)

###
### Operations
###
## + - * / MOD << >> LOGIOR LOGXOR LOGAND LOGNOT FLOOR EXPT
@defpy
class add(potconst):
        def help(*xs):  return help_binop_seq(xs, ast.Add, 0)

@defpy
class sub(potconst):
        def help(*xs):  return help_binop_seq(xs, ast.Sub, 0)

@defpy
class mul(potconst):
        def help(*xs):  return help_binop_seq(xs, ast.Mult, 1)

@defpy
class div(potconst):
        def help(*xs):  return help_binop_seq(xs, ast.Div, 1)

@defpy
class mod(potconst):
        def help(x, y): return help_binop(ast.Mod, x, y)

@defpy
class shl(potconst):
        def help(x, y): return help_binop(ast.LShift, x, y)

@defpy
class shr(potconst):
        def help(x, y): return help_binop(ast.RShift, x, y)

@defpy
class logior(potconst):
        def help(x, y): return help_binop(ast.BitOr, x, y)

@defpy
class logxor(potconst):
        def help(x, y): return help_binop(ast.BitXor, x, y)

@defpy
class logand(potconst):
        def help(x, y): return help_binop(ast.BitAnd, x, y)

@defpy
class lognot(potconst):
        def help(x):    return help_unop(ast.Invert, x)

@defpy
class floor(potconst):
        def help(x, y): return help_binop(ast.FloorDiv, x, y)

@defpy
class expt(potconst):
        def help(x, y): return help_binop(ast.Pow, x, y)

## EQ < <= > >=

@defpy
class eq(potconst):
        def help(x, y): return help_compare(ast.Is, x, [y])

@defpy
class lt(potconst):
        def help(x, y): return help_compare(ast.Lt, x, [y])

@defpy
class le(potconst):
        def help(x, y): return help_compare(ast.LtE, x, [y])

@defpy
class gt(potconst):
        def help(x, y): return help_compare(ast.Gt, x, [y])

@defpy
class ge(potconst):
        def help(x, y):          return help_compare(ast.GtE, x, [y])

## NOT -- the only logical op worth a primitive

@defpy
class not_(potconst):
        ## Optimisation: fold (NOT (EQ X Y)) to ast.IsNot
        def help(x): return help_unop(ast.Not, x)

###
### Data structures
###
## CONS CAR CDR RPLACA RPLACD VECTOR INDEX
@defpy
class cons(expr):
        def help(car, cdr): return ast.List([ help_expr(car),
                                              help_expr(cdr) ])

@defpy
class car(expr):
        def help(cons): return ast.Subscript(help_expr(cons), ast.Index(0), ast.Load())

@defpy
class cdr(expr):
        def help(cons): return ast.Subscript(help_expr(cons), ast.Index(1), ast.Load())

@defpy
class rplaca(expr):
        def help(cons, value):
                return help(assign(index(cons, integer(0), writep = t),
                                   value))

@defpy
class rplacd(expr):
        def help(cons, value):
                return help(assign(index(cons, integer(1), writep = t),
                                   value))

@defpy
class vector(expr):
        def help(*xs):
                return ast.List([ help_expr(x) for x in xs ], help_ctx(nil))

@defpy
class index(expr):
        def help(x, index, writep = nil, deletep = nil):
                return ast.Subscript(help_expr(x), ast.Index(help_expr(index)), getattr(ast, ("Del"   if deletep else
                                                                                              "Store" if writep  else
                                                                                              "Load"))())

###
### Control
###
## ASSIGN RETURN PROGN IF LOOP UNWIND-PROTECT CATCH THROW RESIGNAL

@defpy
class assign(stmt):
        def help(name, value):
                p, v = help(value)
                return (p
                        + [ ast.Assign([ help_expr(name) ], v) ]
                        ), help_expr(name)

@defpy
class return_(stmt):
        def help(x):
                return [ ast.Return(help_expr(x)) ], help_nil()

@defpy
class progn(body):
        def help(*body):
                return help_progn(body)
        progn = identity_method()

@defpy
class if_(indet):
        a_expr = defstrategy(test = lambda _, conseq, ante: (exprp(conseq) and
                                                             exprp(ante)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

@defprim(intern("IF-EXPR")[0],
         (expr_spill, expr, expr))
class if_expr(expr): ...

@defpy
class if_expr(expr):
        def help(*tca):
                return ast.IfExp(*[help_expr(x) for x in tca]) # Test, Consequent, Antecedent.
        if_ = identity_method()

@defprim(intern("IF-STMT")[0],
         (expr_spill, prim, prim))
class if_stmt(body): ...

@defpy
class if_stmt(body):
        def help(*tca):
                (_, tv), (cp, cv), (ap, av) = [ help(x) for x in tca ]
                tn = genname("IFVAL")
                return [ ast.If(tv,
                                cp + [ ast.Assign([ help_expr(tn) ], cv)],
                                ap + [ ast.Assign([ help_expr(tn) ], av)]
                                ) ], help_expr(tn)
        if_ = identity_method()

@defpy
class loop(body):
        def help(body):
                return [ ast.While(help_expr(name("True")),
                                   help_prog([body]),
                                   []) ], help_nil()

@defpy
class unwind_protect(body):
        def help(protected_form, body):
                # need a combinator for PRIM forms
                if body:
                        tn = genname("UWP_VALUE")
                        return [ ast.Try(     body = help(assign(tn, protected_form))[0],
                                         finalbody = help_prog([body]),
                                          handlers = [],
                                            orelse = []
                                         ) ], help_expr(tn)
                else:
                        return help(protected_form)

@defpy
class catch(body):
        ## Lift this to a known?
        def help(tag, body):
                val_tn, ex_tn = genname("BODY_VALUE"), genname("EX")
                return [ ast.Try(     body = help(assign(val_tn, body))[0],
                                  handlers = [ ast.ExceptHandler(help_expr(impl_ref("__catcher_throw__")),
                                                                 ex_tn.value(),
                                                                 help(if_(eq(attr(ex_tn, string("ball")),
                                                                             tag),
                                                                          progn(funcall(impl_ref("__catch_maybe_reenable_pytracer"),
                                                                                        ex_tn),
                                                                                assign(val_tn,
                                                                                       attr(ex_tn, string("value")))),
                                                                          resignal()))[0]) ],
                                 finalbody = [],
                                    orelse = [])
                         ], help_expr(val_tn)

@defpy
class throw(expr):
        def help(tag, value):
                return help_expr(funcall(impl_ref("throw"), tag, value))

@defpy
class resignal(stmt):
        def help():
                return [ ast.Raise(exc = None,
                                   cause = None) ], help_nil() ## Python behavior mismatches doc: exc/cause documented as expr?.

###
### Functions
###
## FUNCTION FUNCALL APPLY
@defpy
class function(indet):
        "NOTE: default value form evaluation is not delayed."
        a_expr = defstrategy(test = (lambda name, args, body, clambda = None, id = nil, pydecorators = []:
                                             exprp(body) and not (name or pydecorators)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)


@defprim(intern("LAMBDA-EXPR")[0],
         (NoneType, ((maybe, name), [name]),
          expr))
class lambda_expr(expr): ...

@defpy
class lambda_expr(expr):
        def help(name, args, expr, clambda = None, id = nil, pydecorators = []):
                assert not (name or pydecorators)
                assert clambda
                maybe_rest, *fixed_args = args
                return ast.Lambda(help_args(fixed_args, [], [], maybe_rest, [], [], None),
                                  help_expr(expr))
        function = identity_method()

@defprim(intern("DEFUN")[0],
         (name, ((maybe, name), [name]),
          prim))
class defun(body): ...

@defpy
class defun(body):
        def help(name, args, body, clambda = None, pydecorators = []):
                if not clambda:
                        dprintf("No clambda in DEFUN %s %s %s", name, args, body)
                        assert(clambda)
                maybe_rest, *fixed_args = args
                return [ ast.FunctionDef(
                                name = name.value(),
                                args = help_args(fixed_args, [], [], maybe_rest, [], [], None),
                                decorator_list = help_exprs(pydecorators),
                                returns = None,
                                body = help(return_(body))[0])
                         ], help_expr(name)
        @defmethod(function)
        def function(name, args, expr, clambda = None, id = nil, pydecorators = []):
                return defun(name or genname((id) if id else "DEFLAM"), args, expr,
                             clambda = clambda, pydecorators = pydecorators)

@defpy
class funcall(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, *fixed_args):
                return ast.Call(help_expr(func),
                                help_exprs(fixed_args), [], None, None)

@defpy
class apply(expr):
        ## NOT LINKED UP -- deferred for usage by higher levels.
        def help(func, arg, *args):
                fixed_args, restarg = (((arg,) + args[:-1], args[-1]) if args else
                                       ([],                 arg))
                return ast.Call(help_expr(func), help_exprs(fixed_args), [], help_expr(restarg), None)

###
### Dynamic scope
###
## SPECIAL-REF SPECIAL-SETQ PROGV
@defpy
class special_ref(efless):
        def help(name):
                return help(funcall(impl_ref("symbol_value"), name))

@defpy
class special_setq(expr):
        def help(nom, value):
                return help(funcall(impl_ref("do_set"), nom, value, name("None")))

@defpy
class progv(body):
        def help(vars, vals, body):
                tn = genname("VALUE")
                return [ ast.With(items = [ ast.withitem(help_expr(funcall(impl_ref("progv"),
                                                                           pyhash(*reduce(operator.add, zip(vars, vals))))),
                                                         []) ],
                                   body = help(assign(tn, body))[0])
                         ], help_expr(tn)

###
### Python-specific primitive declarations and definitions: Part I: Assorti
###
## FILTERMAP GENERATOR IMPL-REF BLIN-REF IMPORT DELETE GLOBAL NONLOCAL ASSERT
@defprim(intern("FILTERMAP")[0],
         (name, expr_spill, prim, (maybe, prim)))
class filtermap(indet): ...

@defpy
class filtermap(indet):
        a_expr = defstrategy(test = lambda name, iter, body, condition = None: (exprp(body)
                                                                                and (exprp(condition) or condition is None)),
                             keys = expr)
        b_stmt = defstrategy(keys = body)

@defprim(intern("FILTERMAP-EXPR")[0],
         (name, expr_spill, expr, (maybe, expr)))
class filtermap_expr(expr): ...

@defpy
class filtermap_expr(expr):
        def help(var, iter, body, condition):
                conditional = isinstance(condition, name) and condition.value == "None"
                return ast.ListComp(help_expr(body),
                                    [ ast.comprehension(help_expr(var), help_expr(iter),
                                                        [ help_expr(condition) ] if conditional else
                                                        []) ] )
        filtermap = identity_method()

@defprim(intern("FILTERMAP-STMT")[0],
         (name, expr_spill, prim, (maybe, prim)))
class filtermap_prim(body): ...

@defpy
class filtermap_prim(body):
        def help(name, iter, body, condition = None):
                tn = genname("FILTERMAP_THUNK")
                return help(progn(defun(tn, (None, name),
                                        body),
                                  filtermap(name, iter, funcall(tn, name), condition)))
        filtermap = identity_method()

@defprim(intern("GENERATOR")[0],
         (expr, [(expr, expr, [expr])]))
class generator(expr): ...

@defpy
class generator(expr):
        def help(result, *comps):
                return ast.GeneratorExp(help_expr(result),
                                        [ ast.comprehension(help_expr(target), help_expr(iter),
                                                            [ help_expr(if_) for if_ in ifs ])
                                          for target, iter, *ifs in comps ])

@defprim(intern("IMPL-REF")[0],
         (str,))
class impl_ref(expr): ...

@defpy
class impl_ref(expr):
        def help(x):
                return more_ast.ast_attribute_chain(["cl", x])

@defprim(intern("BUILTIN-REF")[0],
         (string_t,))
class blin_ref(expr): ...

@defpy
class blin_ref(expr):
        def help(x):
                return help_expr(name(x))

@defprim(intern("IMPORT")[0],
         ([string_t],))
class import_(stmt): ...

@defpy
class import_(stmt):
        def help(*xs):
                return [ ast.Import([ ast.alias(x, None) for x in xs ])
                         ], help_nil()

@defprim(intern("DEL")[0],
         ([expr],))
class delete(stmt): ...

@defpy
class delete(stmt):
        def help(*exprs):
                return [ ast.Delete([ help_expr(x) for x in exprs ])
                         ], help_nil()

@defprim(intern("GLOBAL")[0],
         ([name],))
class global_(stmt): ...

@defpy
class global_(stmt):
        def help(*xs):
                return [ ast.Global([ x.value() for x in xs ]
                                  ) ], help_nil()

@defprim(intern("NONLOCAL")[0],
         ([name],))
class nonlocal_(stmt): ...

@defpy
class nonlocal_(stmt):
        def help(*xs):
                return [ ast.Nonlocal([ x.value() for x in xs ]
                                      ) ], help_nil()

@defprim(intern("ASSERT")[0], (expr_spill, expr_spill))
class assert_(stmt): ...

@defpy
class assert_(stmt):
        def help(condition, description):
                return [ ast.Assert(help_expr(condition), help_expr(description)
                                    ) ], help_nil()

###
### Python-specific primitive declarations and definitions: Part II: Data structures
###
## RAW PYHASH PYTUPLE PYSET
def wrap_raw_ast(x):
        def wrap(x):
                return ([ rec(x) for x in x ]
                        if isinstance(x, list) else
                        rec(x))
        def rec(x):
                return (raw(type(x).__name__, *(wrap(getattr(x, slot))
                                                for slot in type(x)._fields))
                        if isinstance(x, ast.AST) else
                        x)
        return rec(x)

@defprim(intern("RAW")[0],
         ((satisfies, lambda x: hasattr(ast, x) and issubclass(getattr(ast, x), ast.AST)),
          [t]))
class raw(indet): ...

@defpy
class raw(indet):
        a_stmt = defstrategy(test = lambda cls_name, *args, **keys: issubclass(getattr(ast, cls_name), ast.stmt),
                             keys = stmt)
        b_expr = defstrategy(keys = expr)

@defprim(intern("RAW-EXP")[0],
         ((satisfies, lambda x: hasattr(ast, x) and issubclass(getattr(ast, x), ast.AST)),
          [t]))
class raw_expr(expr): ...

@defpy
class raw_expr(expr):
        def help(cls_name, *args, **keys):
                cls = getattr(ast, cls_name)
                return cls( *(([ help_expr(x)
                                 for x in x ] if isinstance(x, (tuple, list)) else
                               help_expr(x))
                              for x in args),
                             **{k: help_expr(v)
                                   for k, v in keys.items()})
        raw = identity_method()

@defprim(intern("RAW-STMT")[0],
         ((satisfies, lambda x: hasattr(ast, x) and issubclass(getattr(ast, x), ast.AST)),
          [t]))
class raw_stmt(stmt): ...

@defpy
class raw_stmt(stmt):
        def help(cls_name, *args, **keys):
                cls = getattr(ast, cls_name)
                return [ cls ( *((tuple(help_expr(x) for x in x) if isinstance(x, tuple) else
                                 help_expr(x))
                               for x in args),
                               **{ k: help_expr(v)
                                   for k, v in keys.items() })
                         ], ast.Name("None", ast.Load())
        raw = identity_method()

@defprim(intern("PYHASH")[0],    ([expr_spill],))
## Unregistered Issue EXTREME-NICETY-OF-AUTOMATIC-RECLASSIFICATION-TO-A-NARROWER-TYPE
class pyhash(expr): ...

@defpy
class pyhash(expr):
        def help(*ksvs):
                if len(ksvs) % 2:
                        error("In LITERAL-HASH-TABLE-EXPR: odd number of arguments: %s", pp_consly(ksvs))
                keys, vals = ksvs[0::2], ksvs[1::2]
                return ast.Dict(help_exprs(keys), help_exprs(vals))

@defprim(intern("PYTUPLE")[0],
         ([expr_spill],))
class pytuple(expr): ...

@defpy
class pytuple(expr):
        def help(*xs):
                return ast.Tuple([ help_expr(x) for x in xs ], help_ctx(nil))

@defprim(intern("PYSET")[0],
         ([expr_spill],))
class pyset(expr): ...

@defpy
class pyset(expr):
        def help(*xs):
                return ast.Set([ help_expr(x) for x in xs ])

###
### Python-specific primitive declarations and definitions: Part III, Operations
###
## NEQ AND OR EQUAL NEQUAL IN NOT-IN ATTR SLICE
def help_unop(op, x):        return ast.UnaryOp(op(), help_expr(x))
def help_binop(op, x, y):    return ast.BinOp(help_expr(x), op(), help_expr(y))
def help_compare(op, x, ys): return ast.Compare(help_expr(x), [op()] * len(ys), [ help_expr(y) for y in ys ])

def help_binop_seq(args, type, one):
        init, rest = ((args[0], args[1:]) if args else (one, args))
        return reduce(lambda x, y: ast.BinOp(x, type(), help_expr(y)),
                      rest, help_expr(init))

def help_boolop(op, xs):     return ast.BoolOp(op(), [ help_expr(x) for x in xs ])


@defprim(intern("NEQ")[0], (expr_spill, expr_spill))
class neq(potconst): ...

@defpy
class neq(potconst):
        def help(x, y):          return help_compare(ast.IsNot, x, [y])

### logic

@defprim(intern("AND")[0], ([expr_spill],))
class and_(potconst): ...

@defpy
class and_(potconst):
        def help(*xs): return help_boolop(ast.And, xs)

@defprim(intern("OR")[0], ([expr_spill],))
class or_(potconst): ...

@defpy
class or_(potconst):
        def help(*xs): return help_boolop(ast.Or, xs)

### high-level
@defprim(intern("EQUAL")[0], (expr_spill, [expr_spill]))
class equal(potconst): ...

@defpy
class equal(potconst):
        def help(x, *ys): return help_compare(ast.Eq, x, ys)

@defprim(intern("NOT-EQUAL")[0], (expr_spill, [expr_spill]))
class nequal(potconst): ...

@defpy
class nequal(potconst):
        def help(x, *ys): return help_compare(ast.NotEq, x, ys)

@defprim(intern("IN")[0], (expr_spill, expr_spill))
class in_(potconst): ...

@defpy
class in_(potconst):
        def help(x, y): return help_compare(ast.In, x, [y])

@defprim(intern("NOT-IN")[0], (expr_spill, expr_spill))
class not_in(potconst): ...

@defpy
class not_in(potconst):
        def help(x, y): return help_compare(ast.NotIn, x, [y])

@defprim(intern("ATTR-REF")[0],
         (prim, prim))
class attr(indet): ...

@defpy
class attr(indet):
        a_const = defstrategy(test = lambda _, attr: isinstance(attr, string),
                              keys = "const attr")
        b_var   = defstrategy(keys = efless)

@defprim(intern("CONST-ATTR-REF")[0],
         (expr_spill, string))
class const_attr(efless): ...

@defpy
class const_attr(efless):
        ## We assume, that within the domain of emitted code, objects do not have accessors defined.  So, efless.
        def help(x, attr, writep = nil): return ast.Attribute(help_expr(x), attr.value(), help_ctx(writep))
        attr = identity_method("const attr")

@defprim(intern("VAR-ATTR-REF")[0],
         (expr_spill, expr_spill))
class var_attr(efless): ...

@defpy
class var_attr(efless):
        def help(x, attr):
                return help(funcall(name("getattr"), help_expr(x), help_expr(attr)))
        attr = identity_method()

def prim_attr_chain(xs, writep = nil):
        return reduce((lambda acc, attr: const_attr(acc, attr, writep = writep)),
                      xs[1:],
                      xs[0])

@defprim(intern("SLICE")[0],
         (expr_spill, expr_spill, maybe_expr_spill, maybe_expr_spill))
class slice(expr): ...

@defpy
class slice(expr):
        def help(x, start, end, step, writep = nil):
                return ast.Subscript(help_expr(x), ast.Slice(help_expr(start),
                                                             help_expr(end if end is not nil else
                                                                       name("None")),
                                                             help_expr(step if step is not nil else
                                                                       name("None"))),
                                     help_ctx(writep))

###
### Code activation
###
def load_code_object_as_module(name, co, filename = "", builtins = None, globals = None, locals = None, register = True):
        check_type(co, type(load_code_object_as_module.__code__))
        mod = imp.new_module(name)
        mod.__filename__ = filename
        if builtins:
                mod.__dict__["__builtins__"] = builtins
        if register:
                sys.modules[name] = mod
        globals = defaulted(globals, mod.__dict__)
        locals  = defaulted(locals, mod.__dict__)
        exec(co, globals, locals)
        return mod, globals, locals

def load_text_as_module(name, text, filename = "", **keys):
        return load_code_object_as_module(name, pyb.compile(text, filename, "exec"),
                                           filename = filename, **keys)[0]

def reregister_module_as_package(mod, parent_package = None):
        # this line might need to be performed before exec()
        mod.__path__ = (parent_package.__path__ if parent_package else []) + [ mod.name.split(".")[-1] ]
        if parent_package:
                dotpos = mod.name.rindex(".")
                assert dotpos
                postdot_name = mod.name[dotpos + 1:]
                setattr(parent_package, postdot_name, mod)
                parent_package.__children__.add(mod)
                mod.__parent__ = parent_package
        if packagep:
                mod.__children__ = set()

def load_module_bytecode(bytecode, func_name = nil, filename = ""):
        mod, globals, locals = load_code_object_as_module(filename, bytecode, register = nil)
        if func_name:
                sf = the((or_t, symbol_t, function_t), # globals[get_function_pyname(name)]
                         mod.__dict__[get_function_rtname(func_name)])
                func = sf if functionp(sf) else symbol_function(sf)
                # without_condition_system(pdb.set_trace) # { k:v for k,v in globals().items() if v is None }
                func.name = func_name # Debug name, as per F-L-E spec.
        else:
                func = nil
        # dprintf("; L-M-B globals: %x, content: %s",
        #               id(globals), { k:v for k,v in globals.items() if k != '__builtins__' })
        return func, globals, dict(globals)

def py_compile_and_load(*body, modname = "", filename = "", lineno = 0, **keys):
        return load_code_object_as_module(
                modname,
                compile(ast.fix_missing_locations(ast_module(list(body), lineno = lineno)), filename, "exec"),
                register = nil,
                filename = filename,
                **keys)
###
### Runtime
###
##
## Frames
##
def all_threads_frames():
        return sys._current_frames()

def this_frame():
        return sys._getframe(1)

frame = type(this_frame())

def framep(x):
        return isinstance(x, frame)

def next_frame(f):
        return f.f_back if f.f_back else error("Frame \"%s\" is the last frame.", pp_frame(f, lineno = True))

def caller_frame(caller_relative = 0):
        return sys._getframe(caller_relative + 2)

def frames_calling(f = None, n = -1):
        "Semantics of N are slightly confusing, but the implementation is so simple.."
        f = caller_frame() if f is None else the(frame, f)
        acc = [f]
        while f.f_back and n:
                f, n = f.f_back, n - 1
                acc.append(f)
        return acc

def caller_name(n = 0):
        return fun_name(frame_fun(sys._getframe(n + 2)))

def caller_args(n = 0):
        return frame_locals(sys._getframe(n + 2))

def exception_frame():
        return sys.exc_info()[2].tb_frame

def top_frame():
        return caller_frame()

def frame_info(f):
        "Return frame (function, lineno, locals, globals, builtins)."
        return (f.f_code,
                f.f_lineno,
                f.f_locals,
                f.f_globals,
                f.f_builtins,
                )

# Issue FRAME-CODE-OBJECT-IS-NOT-FUN
def frame_fun(f):               return f.f_code
def frame_lineno(f):            return f.f_lineno
def frame_locals(f):            return f.f_locals
def frame_globals(f):           return f.f_globals
def frame_local_value(f, name): return f.f_locals[name]

__ordered_frame_locals__ = dict()
def frame_ordered_locals(f):
        global __ordered_frame_locals__
        if f not in __ordered_frame_locals__:
                __ordered_frame_locals__[f] = list(f.f_locals.keys())
        return __ordered_frame_locals__[f]

def fun_info(f):
        "Return function (name, params, filename, lineno, nlines)."
        return (f.co_name or "<unknown-name>",
                f.co_varnames[:f.co_argcount], # parameters
                f.co_filename or "<unknown-file>",
                f.co_firstlineno,
                1 + max(f.co_lnotab or [0]),        # lines
                f.co_varnames[f.co_argcount:], # non-parameter bound locals
                f.co_freevars,
                )
def fun_name(f):        return f.co_name
def fun_filename(f):    return f.co_filename
def fun_firstlineno(f): return f.co_firstlineno
def fun_bytecode(f):    return f.co_code
def fun_constants(f):   return f.co_consts

### Frame pretty-printing
def frame_fun_name(f):          return f.f_code.co_name

def print_function_arglist(f):
        argspec = inspect.getargspec(f)
        return ", ".join(argspec.args +
                         (["*" + argspec.varargs]   if argspec.varargs  else []) +
                         (["**" + argspec.keywords] if argspec.keywords else []))

def pp_frame(f, align = None, handle_overflow = None, lineno = None, frame_id = None):
        fun = frame_fun(f)
        fun_name, fun_params, filename = fun_info(fun)[:3]
        align = ((align or 10) if handle_overflow else
                 defaulted(align, 0))
        return ("%s%s%s %s(%s)" % (((frame_id(f)[:4] + " ") if frame_id else ""),
                                   filename + ("" if align else ":") + (" " * (align - (len(filename) % align if align else 0))),
                                   ("%d:" % frame_lineno(f)) if lineno else "",
                                   fun_name, ", ".join(fun_params)))

def print_frame(f, stream = None, **keys):
        write_string(pp_frame(f, **keys), defaulted_to_var(stream, _debug_io_))

def print_frames(fs, stream = None, frame_ids = None):
        for i, f in enumerate(fs):
                format(defaulted_to_var(stream, _debug_io_), "%2d: %s\n",
                       i, pp_frame(f, lineno = True, frame_id = frame_ids))

def pp_frame_chain(xs, source_location = None, all_pretty = None, print_fun_line = None):
        def pp_frame_in_chain(f, pretty = None):
                fun = frame_fun(f)
                return (fun_name(fun) if not pretty else
                        ("%s%s@%s:%d" % (fun_name(fun),
                                         (":" + str(frame_lineno(f) - fun_firstlineno(fun))) if print_fun_line else "",
                                         fun_filename(fun),
                                         frame_lineno(f))))
        return ("..".join((pp_frame_in_chain(f, t) for f in xs) if all_pretty else
                          ([pp_frame_in_chain(f) for f in xs[:-1]] +
                           [pp_frame_in_chain(xs[-1], t)])))

def pp_chain_of_frame(x, callers = 5, *args, **keys):
        fs = frames_calling(x, callers)
        fs.reverse()
        return pp_frame_chain(fs, *args, **keys)

def escape_percent(x):
        return x.replace("%", "%%")

##
## Higher-level debug trace functions
##
def frame_chain_hash(f, ignore_callers = set(["<lambda>"])):
        "Return an MD5 digest of the caller name chain, with callers listed in IGNORE-CALLERS omitted."
        def f_digestible(f):
                name = f.f_code.co_name
                return name.encode() if name not in ignore_callers else b''
        fchain = frames_calling(f)[1:]
        retv = reduce((lambda acc, f:
                               acc.update(f_digestible(f)) or acc),
                      fchain, hashlib.new("md5")).hexdigest()
        # fprintf(lf, "%s %s\n", [ f_str(x) for x in reversed(chain) ], r)
        return retv

def frame_id(f):
        return hashlib.new("md5", ("%x" % id(f)).encode()).hexdigest()

def here(note = None, *args, callers = 5, stream = None, default_stream = sys.stderr, frame = None, print_fun_line = None, all_pretty = None, offset = 0):
        stream = defaulted(stream, t)
        def do_format(x, args):
                try:
                        return x % args
                except cold_error_type as cond:
                        return "#<error formatting %s into %s: %s>" % (args.__repr__(), note.__repr__(), cond)
        def format_args():
                return (""           if not note else
                        " - " + note if not args else
                        # Unregistered Issue IDEA-MAPXFORM-IF
                        do_format(note, args))
        return format(stream, "    (%s)  %s:\n      %s\n",
                      threading.current_thread().name.upper(),
                      pp_chain_of_frame(defaulted(frame, caller_frame(offset)),
                                        callers = callers - 1,
                                        print_fun_line = print_fun_line,
                                        all_pretty = all_pretty),
                      without_condition_system(format_args),
                      # defaulted(stream, default_stream)
                      )

##
## Pytracer
##
__tracer_hooks__   = dict() # allowed keys: "call", "line", "return", "exception", "c_call", "c_return", "c_exception"
def set_tracer_hook(type, fn):        __tracer_hooks__[type] = fn
def     tracer_hook(type):     return __tracer_hooks__[type] if type in __tracer_hooks__ else None

def pytracer(frame, event, arg):
        method = tracer_hook(event)
        if method:
                method(arg, frame)
        return pytracer

def pytracer_enabled_p(): return sys.gettrace() is pytracer
def enable_pytracer():    sys.settrace(pytracer)
def disable_pytracer():   sys.settrace(None)

def without_condition_system(body, reason = ""):
        if pytracer_enabled_p():
                try:
                        disable_pytracer()
                        return body()
                finally:
                        enable_pytracer()
        else:
                return body()

##
## Essential runtime: symbol, global variable and function availability
##
def make_undefined_function_stub(name):
        stub = lambda *_, **__: undefined_function(name)
        stub.__name__ = "undefined_function_stub_%s" % name
        return stub

### Global compiler state carry-over, and module state initialisation.
# Unregistered Issue SEPARATE-COMPILATION-IN-FACE-OF-NAME-MAPS
def fop_make_symbol_available(globals, package_name, symbol_name, setfp,
                               function_rtname, symbol_rtname,
                               gfunp, gvarp):
        # dprintf("FOP-M-S-A: %11s:%17s%5s %25s%25s%s%s",
        #         package_name, symbol_name, " SETF" * setfp, function_rtname, symbol_rtname,
        #         " gfun" * gfunp, " gvar" * gvarp)
        symbol = (intern(symbol_name, find_package(package_name))[0] if package_name is not None else
                  make_symbol(symbol_name))
        if function_rtname is not None:
                fslot, slot, fname = (("setf_function", "setf_function_rtname", list_(_setf, symbol)) if setfp else
                                      ("function",      "function_rtname",      symbol))
                setattr(symbol, slot, function_rtname)
                # dprintf("   c-t %%U-S-G-F-P %s FUN: %s  - %s, %s %s",
                #               "G" if gfunp else "l", symbol_name, function_rtname, symbol.function, symbol.macro_function)
                if gfunp:
                        # dprintf("FOP-M-S-A symbol %s, fslot %s, value %s, rtname %s",
                        #         symbol, fslot, (getattr(symbol, fslot)
                        #                         or (symbol.macro_function and not setfp)
                        #                         or (lambda *_, **__: undefined_function(symbol))), function_rtname)
                        globals[function_rtname] = (getattr(symbol, fslot)
                                                    or (symbol.macro_function and not setfp)
                                                    ## It is NOT a valid situation, when the function is not defined at
                                                    ## the beginning of load-time for a given compilation unit.
                                                    ## Unregistered Issue ABSURD-NOT-YET-DEFINED-FUNCTION-WITHIN-COMPILATION-UNIT
                                                    or make_undefined_function_stub(symbol))
        if gvarp:
                if find_global_variable(symbol):
                        value = symbol_value(symbol)
                        assert(value is not None)
                        globals[unit_variable_rtname(symbol)] = value
                else:
                        ## Unregistered Issue UNDEFINED-GLOBAL-REFERENCE-ERROR-DETECTION
                        pass # This will fail obscurely.
        if symbol_rtname is not None:
                symbol.symbol_rtname = symbol_rtname
                globals[symbol_rtname] = symbol

def fop_make_symbols_available(globals, package_names, symbol_names, setfps,
                                function_rtnames, symbol_rtnames,
                                gfunps, gvarps):
        for fop_msa_args in zip(package_names, symbol_names, setfps,
                                function_rtnames, symbol_rtnames,
                                gfunps, gvarps):
                fop_make_symbol_available(globals, *fop_msa_args)

##
## Essential runtime: complex lambda list support
##
def parse_keyword_args(rest):
        acc = dict()
        for k, v in zip(rest[0::2], rest[1::2]):
                if k not in acc:
                        acc[k] = v
        return acc

def validate_keyword_args(allowed_set, keymap):
        if _allow_other_keys_ in keymap and keymap[_allow_other_keys_] is not nil:
                return
        wrong_keys = keymap.keys() - allowed_set - set([_allow_other_keys_])
        if wrong_keys:
                error("Unknown &KEY arguments: %s", ", ".join(str(x) for x in wrong_keys))

###
### The Python machine definition
###
@defmachine
class pymach(machine):
        def globals(self):
                return globals()
        ## Known level
        def vector_consifier(self, x):
                return ir_cl_call("consify_linear", x)
        def vararg_count(self, vararg_name):
                return ir_funcall("len", vararg_name)
        def vararg_subseq(self, vararg_name, start):
                return list_(_primitive, slice, vararg_name, start, None, None)
        def keyword_binding_checking(self, optless_rest, keys, defaults, must_check_keys = True):
                l, l_ = list_, list__
                ksyms        = [ make_keyword_tn(symbol_name(x)) for x in keys ]
                keyset_gsym  = gensym("KEYSET-") if must_check_keys else nil
                key_map_gsym = gensym_tn("KWHASH-")
                return append(
                        l_(l(key_map_gsym, ir_apply(list_(_quote, list_("py", "parse_keyword_args")), optless_rest)),
                           consify_linear(l(name, l(_if, l(_primitive, not_in, ksym, key_map_gsym),
                                                    def_expr,
                                                    l(_primitive, p.index, key_map_gsym, ksym)))
                                          for name, ksym, def_expr in zip(keys, ksyms, defaults))),
                        (l(l(keyset_gsym, ir_apply("set", ir_cl_call("vectorise_linear", ir_funcall(_list, *ksyms)))),
                           l(gensym("DUMMY-"), ir_apply(list_(_quote, list_("py", "validate_keyword_args")),
                                                        keyset_gsym, key_map_gsym)))
                         if must_check_keys else nil))
        ## Primitive level
        __supported_primitives__ = {
                string, symbol, literal_list,
                name, integer, float_num,
                add, sub, mul, div, mod, shl, shr, logior, logxor, logand, lognot, floor, expt,
                eq, lt, le, gt, ge,
                not_,
                cons, car, cdr, rplaca, rplacd, vector, index,
                assign, return_, progn, if_, loop, unwind_protect, catch, throw, resignal,
                function, funcall, apply,
                special_ref, special_setq, progv,

                ## Specific for this backend.
                defun, lambda_expr,
                progn,
                if_expr, if_stmt,
                attr, const_attr, var_attr,
                slice,
                filtermap, filtermap_expr, filtermap_prim, generator,
                impl_ref, blin_ref,
                import_, delete, global_, nonlocal_, assert_,
                raw, raw_expr, raw_stmt,
                pyhash, pytuple, pyset,
                neq, and_, or_, equal, nequal, in_, not_in,
                }
        def make_indeterminate_primitive(_, cls, args, keys):
                return determine(cls, args, keys)
        def after_initialise_determinate_primitive(_, x):
                return prim_check_and_spill(x)
        def primitivise_implref(_, x):
                return prim_attr_chain([ p.name(x[1][0][0]) ]
                                       + xmap_to_vector(p.string, x[1][0][1]))
        def codify_primitive_tree(self, prim, lexenv):
                ## We ignore the lexenv, because.. we don't need it for anything:
                ##  - no closure analysis, because the backend can directly describe all closures we want,
                ##    which it can then directly execute
                asts = help_prog([prim])
                if symbol_value(cl._compiler_validate_ast_):
                        [ ast_validate(a) for a in asts ]
                with _progv({ fixupp: nil }):
                        fixed_asts = [ name_context_fixer.visit(the(ast.AST, a))
                                       for a in asts ]
                if symbol_value(cl._compiler_trace_ast_):
                        report(fixed_asts, "ast", form_id = id(form), desc = "%LOWER")
                return fixed_asts
        def compilation_unit_prologue(m, funs, syms, gfuns, gvars):
                def wrap_str_or_None(x):
                        return (m.string(x)    if isinstance(x, str) else
                                m.name("None") if x is None          else
                                error("Bad wrap: %s.", x))
                def wrap_bool(x):
                        return m.name("True" if x else "False")
                with _progv(compiler_debugless_traceless_frame):
                         names = sorted(funs | syms, key = lambda x: str(x if isinstance(x, symbol_t) else x[1]))
                         names = [ (interpreted_function_name_symbol(x), x, isinstance(x, tuple)) for x in names]
                         prologue = m.progn(
                                 m.import_("cl", "py"),
                                 m.funcall(
                                         m.const_attr(m.name("py"), m.string("fop_make_symbols_available")),
                                         m.funcall(m.name("globals")),
                                         m.vector(*((m.string(package_name(symbol_package(sym))) if symbol_package(sym) else
                                                    m.name("None"))
                                                    for sym, x, _ in names)),
                                         m.vector(*(m.string(symbol_name(sym))            for sym, x, _     in names)),
                                         m.vector(*(wrap_bool(setfp)                      for _, __, setfp  in names)),
                                         m.vector(*(wrap_str_or_None(sym.setf_function_rtname if setfp else
                                                                     sym.function_rtname) for sym, x, setfp in names)),
                                         m.vector(*(wrap_str_or_None(sym.symbol_rtname)   for sym, x, _     in names)),
                                         m.vector(*(wrap_bool(x in gfuns)                 for sym, x, _     in names)),
                                         m.vector(*(wrap_bool(sym in gvars)               for sym, x, _     in names))))
                         # dprintf("prologue:\n%s", pp_consly(prologue))
                         with _progv({ p._valueless_primitive_statement_must_yield_nil_: nil }):
                                 return m.codify_primitive_tree(prologue, nil)
        def assemble(m, _ast: [ast.stmt], form: cons_t, filename = "") -> "code":
                if symbol_value(cl._compiler_validate_ast_):
                        [ ast_validate(a) for a in _ast ]
                more_ast.assign_meaningful_locations(_ast)
                if symbol_value(cl._compiler_trace_module_ast_):
                        report(_ast, "ast", form_id = id(form), desc = "%ASSEMBLE")
                bytecode = compile(ast.fix_missing_locations(more_ast.ast_module(_ast)), filename, "exec")
                if symbol_value(cl._compiler_trace_bytecode_):
                        report(bytecode, "bytecode", form_id = id(form), desc = "%ASSEMBLE")
                return bytecode
        def execute_bytecode(m, bytecode, object_name = None, filename = ""):
                function, broken_globals, good_globals = load_module_bytecode(bytecode, object_name, filename)
                ## Critical Issue NOW-WTF-IS-THIS-SHIT?!
                broken_globals.update(good_globals)
                # dprintf("; D-P: globals: %x, content: %s",
                #               id(globals), { k:v for k,v in globals.items() if k != '__builtins__' })
                return function
###
### CL level services
###
@_defun_
def backtrace(x = -1, stream = None, frame = None, frame_ids = None, offset = 0):
        print_frames(frames_calling(defaulted(frame, this_frame()))[1 + offset:x],
                     defaulted_to_var(stream, _debug_io_),
                     frame_ids = frame_ids)

@_defun_
def boundp(symbol):
        # Unregistered Issue COMPLIANCE-BOUNDP-ACCEPTS-STRINGS
        return t if find_dynamic_frame(the(symbol_t, symbol)) else nil

@_defclass_
class broadcast_stream_t(stream_t):
        def __init__(self, *streams):
                self.streams  = streams
        def write(self, data):
                for component in self.streams:
                        component.write(data)
        def flush(self):
                for component in self.streams:
                        component.flush()
        def readable(self): return nil
        def writable(self): return t

@_defclass_
class synonym_stream_t(stream_t):
        def __init__(self, symbol):
                self.symbol  = symbol
        def stream():
                return symbol_value(self.symbol)
        def read(self, amount):
                return self.stream().read(amount)
        def write(self, data):
                return self.stream().write(data)
        def flush(self):
                return self.stream().flush()
        def readable(self): return self.stream.readable()
        def writable(self): return self.stream.writable()

def compose(f, g):
        return lambda *args, **keys: f(g(*args, **keys))

@_defun_
def count(elt, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end, _test, _test_not])
        key, start, end, from_end, test, test_not = [ keys.get(k, df) for k, df
                                                      in [ (_key_,     identity),
                                                           (_start,    0),
                                                           (_end,      nil),
                                                           (_from_end, nil),
                                                           (_test,     nil),
                                                           (_test_not, nil) ] ]
        not_implemented()

@_defun_
def count_if(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        not_implemented()

@_defun_
def count_if_not(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        not_implemented()

@_defun_
def find_if(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        return do_find_if(p, xs, keys)

@_defun_
def find_if_not(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        return do_find_if(lambda x: not p(x), xs, keys)

@_defun_
def export(symbols, package = None):
        symbols, package = symbols if isinstance(symbols, list) else [symbols], coerce_to_package(package)
        assert(all(isinstance(x, symbol_t)
                   for x in symbols))
        symdict = map_into_hash(lambda x: (x.name, x), symbols)
        for user in package.packages_using:
                use_package_symbols(user, package, symdict)
        # No conflicts?  Alright, we can proceed..
        symset = set(symdict.values())
        for_interning = symset & set(package.inherited)
        for sym in for_interning:
                del package.inherited[sym]
                self.internal.add(sym)
        package.external |= symset
        return True

@_defun_
def fboundp(name):
        """fboundp name => generalized-boolean

Pronunciation:

[,ef'bandpee]

Arguments and Values:

NAME---a function name.

GENERALIZED-BOOLEAN---a generalized boolean.

Description:

Returns true if NAME is fbound; otherwise, returns false."""
        return t if (the(symbol_t, name).function or
                     name.macro_function) else nil

## @_defun_ def function was moved lower, due to dependency on @_defun_ and CL:T

@_defun_
def file_position(x):
        return x.seek(0, 1)

def symbols_not_accessible_error(package, syms):
        def pp_sym_or_string(x):
                return "\"%s\"" % x if isinstance(x, str) else print_nonkeyword_symbol(x)
        error(simple_package_error_t, "These symbols are not accessible in the %s package: (%s).",
              package_name(package), ", ".join((pp_sym_or_string(x) for x in syms)))

def find_symbol_or_fail(x, package = None):
        sym = really_do_find_symbol(x, coerce_to_package(package))
        return (sym if sym is not None else
                symbols_not_accessible_error(p, [x]))

@_defun_
def get_universal_time():
        # Issue UNIVERSAL-TIME-COARSE-GRANULARITY
        # time.time() returns microseconds..
        return int(time.time())

@_defun_
def hash_table_alist(xs):
        return mapcon(lambda kv: [k, [v, nil]],
                      consify_linear(the(dict, xs).items()))

@_defun_("IMPORT")
def import__(symbols, package = None, populate_module = True):
        p = coerce_to_package(package)
        symbols = vectorise_linear(ensure_list(symbols))
        module = find_module(lisp_symbol_name_rtname(package_name(p)),
                              if_does_not_exist = "continue")
        for s in symbols:
                ps, accessible = gethash(s.name, p.accessible)
                if ps is s:
                        continue
                elif accessible: # conflict
                        symbol_conflict_error("IMPORT", s, p, s, ps)
                else:
                        p.imported.add(s)
                        p.accessible[s.name] = s
                        if module:
                                not_implemented("Namespace merging.")
                                # Issue SYMBOL-VALUES-NOT-SYNCHRONISED-WITH-PYTHON-MODULES
                                # rtname = frost.lisp_symbol_name_rtname(s.name)
                                # module.__dict__[rtname] = ?
        return t

@_defun_
def open_stream_p(x):
        return not the(stream_t, x).closed

@_defun_
def input_stream_p(x):
        return open_stream_p(x) and x.readable()

@_defun_
def output_stream_p(x):
        return open_stream_p(x) and x.writable()

@_defun_
def invoke_restart(restart, *args, **keys):
        """
Calls the function associated with RESTART, passing arguments to
it. Restart must be valid in the current dynamic environment.
"""
        assert(isinstance(restart, str) or restartp(restart))
        restart = restart if restartp(restart) else find_restart(restart)
        return restart.function(*args, **keys)

@_defun_
def invoke_restart_interactively(restart):
        """
INVOKE-RESTART-INTERACTIVELY calls the function associated with
RESTART, prompting for any necessary arguments. If RESTART is a name,
it must be valid in the current dynamic environment.

INVOKE-RESTART-INTERACTIVELY prompts for arguments by executing the
code provided in the :INTERACTIVE KEYWORD to RESTART-CASE or
:INTERACTIVE-FUNCTION keyword to RESTART-BIND.

If no such options have been supplied in the corresponding
RESTART-BIND or RESTART-CASE, then the consequences are undefined if
the restart takes required arguments. If the arguments are optional,
an argument list of nil is used.

Once the arguments have been determined, INVOKE-RESTART-INTERACTIVELY
executes the following:

 (apply #'invoke-restart restart arguments)
"""
        assert(isinstance(restart, str) or restartp(restart))
        restart = restart if restartp(restart) else find_restart(restart)
        return invoke_restart(restart, *restart.interactive_function())

@_defun_
def make_hash_table(default_constructor = None):
        return (collections.defaultdict(default_constructor) if default_constructor else
                dict())

@_defun_
def make_list(length, *rest):
        elt = extract_keywords(rest, [_initial_element]).get(initial_element, nil)
        acc = nil
        for i in range(length):
                acc = [elt, acc]
        return acc

## Unregistered Issue COMPLIANCE-HOST-TYPE-WRONG
@_defun_
def make_pathname(*args, host = None, device = None, directory = None, name = None, type = None, version = None,
                  default = None, case = make_keyword("LOCAL")):
        assert not args
        default = default or pathname_t(**_defaulted_keys(
                        host = pathname_host(symbol_value(_default_pathname_defaults_)),
                        device = nil, directory = nil, name = nil, type = nil, version = nil))
        effective_host = defaulted(host, default.host)
        supplied_pathname = dict(
                (k, effective_host.apply_case(case, v) if isinstance(v, str) else v)
                for k, v in only_specified_keys(host = host, device = device, directory = directory, name = name, type = type, version = version).items())
        ## Unregistered Issue RESEARCH-COMPLIANCE-MAKE-PATHNAME-CANONICALISATION
        return merge_pathnames(supplied_pathname, default)

@_defun_
def member(x, xs):
        keys = extract_keywords(rest, [_key_, test, test_not])
        key, test, test_not = [ keys.get(k, df) for k, df
                                in [ (key_,     identity),
                                     (test,     nil),
                                     (test_not, nil) ] ]
        not_implemented()

@_defun_
def member_if(test, xs):
        key = extract_keywords(rest, [_key_]).get(key_, identity)
        not_implemented()

@_defun_
def member_if_not(test, xs):
        key = extract_keywords(rest, [_key_]).get(key_, identity)
        not_implemented()

@_defun_
def parse_integer(xs, junk_allowed = nil, radix = 10):
        l = len(xs)
        def hexcharp(x): return x.isdigit() or x in ["a", "b", "c", "d", "e", "f"]
        (test, xform) = ((str.isdigit, identity)      if radix == 10 else
                         (hexcharp,    float.fromhex) if radix == 16 else
                         not_implemented("PARSE-INTEGER only implemented for radices 10 and 16."))
        for end in range(0, l):
                if not test(xs[end]):
                        if junk_allowed:
                                end -= 1
                                break
                        else:
                                error("Junk in string \"%s\".", xs)
        return int(xform(xs[:(end + 1)]))

@_defun_
def zerop(x):
        return t if x is 0 else nil

@_defun_
def plusp(x):
        return t if x > 0 else nil

@_defun_
def read_line(stream = None, eof_error_p = t, eof_value = nil):
        stream = defaulted_to_var(stream, _standard_input_)
        return handler_case(lambda: stream.readline(),
                            (error_t,
                             lambda c: error(end_of_file_t, "end of file on %s" % (stream,))))

@_defun_
def read_from_string(string, eof_error_p = t, eof_value = nil,
                           start = 0, end = None, preserve_whitespace = None):
        stream = io.StringIO(string)
        try:
                return cold_read(stream, eof_error_p = eof_error_p, eof_value = eof_value,
                                  start = start, end = end, preserve_whitespace = preserve_whitespace)
        finally:
                close(stream)

@_defun_
def remove_if(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end, _count])
        return do_remove_if(p, xs, keys)

@_defun_
def remove_if_not(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end, _count])
        return do_remove_if(lambda x: not p(x), xs, keys)

@_defun_
def sleep(x):
        return time.sleep(x)

@_defun_
def some(f, xs, *xss):
        if not xss:
                while xs:
                        if f(xs[0]):
                                 return t
                        xs = xs[1]
                return nil
        else:
                not_implemented("SOME: multiple-list case")

@_defun_
def stable_sort(xs, predicate):
        return sorted(xs, key = functools.cmp_to_key(predicate))

def with_standard_io_syntax(body):
        """Within the dynamic extent of the BODY of forms, all reader/printer
control variables, including any implementation-defined ones not
specified by this standard, are bound to values that produce standard
READ/PRINT behavior. The values for the variables specified by this
standard are listed in the next figure.

Variable                     Value
*package*                    The CL-USER package
*print-array*                t
*print-base*                 10
*print-case*                 :upcase
*print-circle*               nil
*print-escape*               t
*print-gensym*               t
*print-length*               nil
*print-level*                nil
*print-lines*                nil
*print-miser-width*          nil
*print-pprint-dispatch*      The standard pprint dispatch table
*print-pretty*               nil
*print-radix*                nil
*print-readably*             t
*print-right-margin*         nil
*read-base*                  10
*read-default-float-format*  single-float
*read-eval*                  t
*read-suppress*              nil
*readtable*                  The standard readtable
"""
        with progv(__standard_io_syntax__):
                return body()

@_defun_
def write_char(c, stream = t):
        write_string(c, stream)
        return c

@_defun_
def write_line(string, stream = t):
        return write_string(string + "\n", stream)
