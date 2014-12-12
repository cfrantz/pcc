######################################################################
#
# Abstract Instruction Set
#
#
#
######################################################################
from copy import copy
from cStringIO import StringIO
import typeinfo

defsz = typeinfo.size['int'][0]

class Instruction(object):
    _fields = []
    _defval = []
    def __init__(self, **kwargs):
        for k, defval in zip(self._fields, self._defval):
            v = kwargs.pop(k, None)
            if v is None:
                v = copy(defval)
            setattr(self, k, v)

        if kwargs:
            raise Exception('Unknown initializers for class %s: %r' % (
                self.__class__.__name__, kwargs)) 

    def __repr__(self):
        args = []
        for k in self._fields:
            v = getattr(self, k)
            if v is not None:
                args.append('%s=%r' % (k, v))
        return '%s(%s)' % (self.__class__.__name__, ', '.join(args))
        
class EffectiveAddr(Instruction):
    _fields = ['target', 'symbol']
    _defval = [None, None]

class Load(Instruction):
    _fields = ['target', 'addr', 'size', 'signed']
    _defval = [None, None, defsz, True]

class Store(Instruction):
    _fields = ['src0', 'addr', 'size']
    _defval = [None, None, defsz]

class Constant(Instruction):
    _fields = ['target', 'val', 'size', 'signed']
    _defval = [None, None, defsz, True]

class Move(Instruction):
    _fields = ['target', 'src0', 'size', 'signed']
    _defval = [None, None, None, None, None]

class TwoOp(Instruction):
    _fields = ['target', 'src0']
    _defval = [None, None, None]

class Negate(TwoOp): pass
class Complement(TwoOp): pass
class Not(TwoOp): pass

class ThreeOp(Instruction):
    _fields = ['target', 'src0', 'src1']
    _defval = [None, None, None]

class Add(ThreeOp): pass
class Sub(ThreeOp): pass
class Mul(ThreeOp): pass
class Div(ThreeOp): pass
class Mod(ThreeOp): pass
class And(ThreeOp): pass
class Or(ThreeOp): pass
class Xor(ThreeOp): pass
class Shl(ThreeOp): pass
class Shr(ThreeOp): pass

class Eq(ThreeOp): pass
class Ne(ThreeOp): pass
class Gt(ThreeOp): pass
class Ge(ThreeOp): pass
class Lt(ThreeOp): pass
class Le(ThreeOp): pass

class IfTrue(Instruction):
    _fields = ['src0', 'label']
    _defval = [None, None]

class IfFalse(Instruction):
    _fields = ['src0', 'label']
    _defval = [None, None]

class Jump(Instruction):
    _fields = ['label', 'target']
    _defval = [None, None]

class Label(Instruction):
    _fields = ['name']
    _defval = [None]

class Call(Instruction):
    _fields = ['target', 'args', 'retval']
    _defval = [None, [], None]

class Return(Instruction):
    _fields = ['src0']
    _defval = [None]

class Enter(Instruction):
    _fields = ['name']
    _defval = [None]

class Leave(Instruction): pass

class SymPush(Instruction):
    _fields = ['symtab']
    _defval = [None]
class SymPop(Instruction): pass

class Data(Instruction):
    _fields = ['name', 'type', 'data']
    _defval = [None, None, None]

class Extern(Instruction):
    _fields = ['name']
    _defval = [None]

