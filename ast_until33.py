import ast
from cl import defast, maybe_t, pylist_t

# ..3.2 | With(expr context_expr, expr? optional_vars, stmt* body)
@defast
def ast_With(context_expr:   ast.expr,
             optional_vars: (maybe_t, ast.expr),
             body:          (pylist_t, ast.stmt)): ...


# ..3.2 | TryExcept(stmt* body, excepthandler* handlers, stmt* orelse)
@defast
def ast_TryExcept(body:     (pylist_t, ast.stmt),
                  handlers: (pylist_t, ast.excepthandler),
                  orelse:   (pylist_t, ast.stmt)): ...

# ..3.2 | TryFinally(stmt* body, stmt* finalbody)
@defast
def ast_TryFinally(body:      (pylist_t, ast.stmt),
                   finalbody: (pylist_t, ast.stmt)): ...

