import ast
from cl import defast

# 3.3.. | With(withitem* items, stmt* body)
@defast
def ast_With(items: (pylist_t, ast.withitem),
             body:  (pylist_t, ast.stmt)): ...

# 3.3.. | Try(stmt* body, excepthandler* handlers, stmt* orelse, stmt* finalbody)
@defast
def ast_Try(body:      (pylist_t, ast.stmt),
            handlers:  (pylist_t, ast.excepthandler),
            orelse:    (pylist_t, ast.stmt),
            finalbody: (pylist_t, ast.stmt)): ...

# 3.3.. | YieldFrom(expr value)
@defast
def ast_YieldFrom(value: ast.expr): pass

# withitem = (expr context_expr, expr? optional_vars)
@defast
def ast_withitem(context_expr:  ast.expr,
                 optional_vars: (maybe_t, ast.expr)): ...
