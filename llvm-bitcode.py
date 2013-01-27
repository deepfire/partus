import struct

def error(x, args):
        raise Exception(x % args)

def the(type, x):
        if not isinstance(x, type):
                error("%s is not of type %s.", x, type)

def ceil(x, mask):
        return ((x | mask) + 1) if x & mask else x

##
##
class llvm_bytecode_object():
        def __len__(self):
                return self.size

class block():
        ## unmet expectations: __len__ and encode for all content
        def __init__(self, type, content):
                self.type = the(int, type)
                self.content_size = len(content)
                self.size = 8 + ceil(self.content_size, 0b11)
                self.content = content
        def encode(self):
                if self.type & ~0xffffffff: error("Invalid block type: 0x%x", self.type)
                if self.size & ~0xffffffff: error("Invalid block size: 0x%x", self.size)
                return (struct.pack("II", self.type, self.size)
                        + encode(content)
                        + bytes([0]) * (ceil(self.content_size) - self.content_size))

class llist():
        def __init__(self, content):
                self.content_size = len(content)
                self.size = 4 + self.content_size ??
                self.content = content
        def encode(self):
                if self.size & ~0xffffffff: error("Invalid llist size: 0x%x", self.size)
                return (struct.pack("I", self.size)
                        + encode(content))

class zlist():
        def __init__(self, content):
                self.content_size = len(content)
                self.size = self.content_size +  ??
                self.content = content
        def encode(self):
                return encode(content)
                
class vbr():
        def __init__(self, x):
                nbits = int.bit_length(x) + (1 if x < 0 else 0)
                self.size = (nbits//7) + 1
                self.content = x
        def encode(self):
                bits = (self.content << 1) | (self.content < 0)
                return (bytes(0x80 | ((bits & (0x7f << shift)) >> shift)
                              for shift in range(7, , 7))
                        + bytes(bits & 0x7f))
