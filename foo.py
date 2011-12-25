def _python_builtins_dictionary():
        return _builtins.getattr(__builtins__, "__dict__", __builtins__)

def _setf_python_builtins_dictionary(value):
        global __builtins__
        if hasattr(__builtins__, "__dict__"):
                vars(__builtins__)["__dict__"] = value
                # __builtins__.__dict__ = value
        else:
                __builtins__ = value
        return value

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
_setf_python_builtins_dictionary(_python.dict(_python_builtins_dictionary()))

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
def _distance_oneself_from_python():
        for name in _python_builtin_names:
                del _python_builtins_dictionary()[name]

_distance_oneself_from_python()

print(zip)
