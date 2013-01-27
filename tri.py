## Basic block
##
class bblock():
        __slots__ = ("code", "ins", "outs")
        def __init__(self):
                self.code = []
                self.ins, self.outs = [], []
        def grow(self, x):
                self.code.append(x)

## Triop
##
class triop():
        pass

## Literals
##
class value(triop):
        pass

class string(value):     pass
class number(value):     pass
class vector(value):     pass

## Operators
##
class operator(triop):
        pass

class logand(operator): pass
class logior(operator): pass
class logxor(operator): pass
class lognot(operator): pass
class ash(operator):    pass
class add(operator):    pass
class sub(operator):    pass
class mul(operator):    pass
class floor(operator):  pass
class ceil(operator):   pass
class mod(operator):    pass

class index(operator):   pass

class concat(operator):  pass
class slice(operator):   pass

## Operators yielding bool
##
class test(operator):
        pass

class gt(test):     pass
class lt(test):     pass
class ge(test):     pass
class le(test):     pass
class equals(test): pass

class eq(test):    pass

class or_(test):  pass
class and_(test): pass
class not_(test): pass

## Control structures
##
class branch(triop):
        pass

class branch_if_eq(branch):     pass
class branch_if_ne(branch):     pass
class branch_if_equal(branch):  pass
class branch_if_nequal(branch): pass

## Memory access: globals, stack
##
class memory(triop):
        pass

class store_global(memory):  pass
class load_global(memory):   pass
class push(memory):          pass
class pop(memory):           pass

## Higher level
##
class hir(triop):
        pass

class call(branch, hir):      pass

class equal(test, hir):       pass

class hash_table(value, hir): pass

class gethash(operator, hir): pass
class puthash(operator, hir): pass
