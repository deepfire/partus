def _python_builtins_dictionary():
        return _builtins.getattr(__builtins__, "__dict__", __builtins__)

def _defaulted(value, default):
        return value if value is not None else default

def _load_code_object_as_module(name, co, filename = "", builtins = None, globals_ = None, locals_ = None, register = True):
        mod = _imp.new_module(name)
        mod.__filename__ = filename
        if builtins:
                mod.__dict__["__builtins__"] = builtins
        if register:
                _sys.modules[name] = mod
        globals_ = _defaulted(globals_, mod.__dict__)
        locals_  = _defaulted(locals_, mod.__dict__)
        _builtins.exec(co,
                       globals_,
                       locals_)
        return mod, globals_, locals_

def _setf_python_builtins_dictionary(value):
        global __builtins__
        if hasattr(__builtins__, "__dict__"):
                # vars(__builtins__)["__dict__"] = value
                print("hasattr case")
                __builtins__.__dict__ = value
        else:
                print("not hasattr case:", dir(__builtins__))
                __builtins__ = value
                # __builtins__.__dict__ = value
        return value

import imp         as _imp
import sys         as _sys
import builtins    as _builtins
import collections as _collections

class _dictator(_collections.UserDict):
        def __hasattr__(self, name): return name in self.data
        def __getattr__(self, name): return self.data[name]
        def __setitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __delitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __setattr__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __init__(self, dict):
                self.__dict__.update(data = dict)

_python = _dictator(_python_builtins_dictionary())
_setf_python_builtins_dictionary(dict(_python_builtins_dictionary()))

# Obtained by the means of: list(sorted(sys.modules["__main__"].__builtins__.__dict__.keys()))
_python_builtin_names = ['ArithmeticError', 'AssertionError', 'AttributeError',
                         'BaseException', 'BufferError', 'BytesWarning',
                         'DeprecationWarning',
                         'EOFError', 'Ellipsis', 'EnvironmentError', 'Exception',
                         ############ 'False' cannot be deleted
                         'False', 'FloatingPointError', 'FutureWarning',
                         'GeneratorExit',
                         'IOError', 'ImportError', 'ImportWarning', 'IndentationError', 'IndexError',
                         'KeyError', 'KeyboardInterrupt',
                         'LookupError',
                         'MemoryError',
                         ############ 'None' cannot be deleted
                         'NameError', 'None', 'NotImplemented', 'NotImplementedError',
                         'OSError', 'OverflowError',
                         'PendingDeprecationWarning',
                         'ReferenceError', 'ResourceWarning', 'RuntimeError', 'RuntimeWarning',
                         'StopIteration', 'SyntaxError', 'SyntaxWarning', 'SystemError', 'SystemExit',
                         ############ 'True' cannot be deleted
                         'TabError', 'True', 'TypeError',
                         'UnboundLocalError', 'UnicodeDecodeError', 'UnicodeEncodeError', 'UnicodeError', 'UnicodeTranslateError', 'UnicodeWarning', 'UserWarning',
                         'ValueError',
                         'Warning',
                         'ZeroDivisionError',
                         ###### These are both sufficiently important and _-prefixed.
                         # '_', '__build_class__', '__debug__', '__doc__', '__import__', '__name__', '__package__',
                         'abs', 'all', 'any', 'ascii',
                         'bin', 'bool', 'bytearray', 'bytes',
                         'callable', 'chr', 'classmethod', 'compile', 'complex', 'copyright', 'credits',
                         'delattr', 'dict', 'dir', 'divmod',
                         'enumerate', 'eval', 'exec', 'exit',
                         'filter', 'float', 'format', 'frozenset',
                         'getattr', 'globals',
                         'hasattr', 'hash', 'help', 'hex',
                         'id', 'input', 'int', 'isinstance', 'issubclass', 'iter',
                         'len', 'license', 'list', 'locals',
                         'map', 'max', 'memoryview', 'min',
                         'next',
                         'object', 'oct', 'open', 'ord',
                         'pow', 'print', 'property',
                         'quit',
                         'range', 'repr', 'reversed', 'round',
                         'set', 'setattr', 'slice', 'sorted', 'staticmethod', 'str', 'sum', 'super',
                         'tuple', 'type',
                         'vars',
                         'zip']
### Clean up the namespace.
def _distance_oneself_from_python(name, filename):
        global _python
        sys, py = _sys, _python
        old, new = (sys.modules[name],
                    _load_code_object_as_module(name, compile("", filename, "exec"), filename,
                                                builtins = dict(),
                                                register = False)[0])
        old.__dict__.clear()
        # old.__dict__.update(new.__dict__)
        sys.modules[name] = new
        return old, new

print(_distance_oneself_from_python("foo", "foo.py"))
print(int)
