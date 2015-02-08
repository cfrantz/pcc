######################################################################
#
# AST and helper functions for C
#
#
#
######################################################################
from copy import copy
import itertools
from cStringIO import StringIO
import error

print_extra = False

def concat(*args):
    ret = []
    for a in args:
        if a is None:
            pass
        elif isinstance(a, (tuple, list)):
            ret.extend(concat(*a))
        else:
            ret.append(a)
    return ret


class AST(object):
    _fields = []
    _defval = []
    _extra = {}
    def __init__(self, **kwargs):
        operators = getattr(self, 'operators', None)
        for k, defval in zip(self._fields, self._defval):
            v = kwargs.pop(k, None)
            if v is None:
                v = copy(defval)
            if k == 'op' and operators:
                v, prec = operators[v]
                self.precedence = prec
            setattr(self, k, v)

        for k,v in self._extra.items():
            v = kwargs.pop(k, v)
            setattr(self, k, v)

        if kwargs:
            raise Exception('Unknown initializers for class %s: %r' % (
                self.__class__.__name__, kwargs)) 

    def _print(self, indent, sio):
        name = self.__class__.__name__
        sio.write(name+'(')
        indent += len(name)+1
        fields = self._fields
        if print_extra:
            fields = fields[:] + self._extra.keys()
        for i, k in enumerate(fields):
            if i:
                sio.write(',\n'+' '*indent)
            v = getattr(self, k)
            sio.write(k+'=')
            nindent = indent+len(k)+1
            if isinstance(v, AST):
                v._print(nindent, sio)
            elif isinstance(v, list):
                nindent += 1
                sio.write('[')
                for j, vv in enumerate(v):
                    if j:
                        sio.write(',\n'+' '*nindent)
                    if isinstance(vv, AST):
                        vv._print(nindent, sio)
                    else:
                        sio.write(repr(vv))
                sio.write(']')
            else:
                sio.write(repr(v))
        sio.write(')')

    def _become(self, cls):
        kw = {k: getattr(self, k) for k in self._fields}
        print("Become %s(%r)" % (cls, kw))
        return cls(**kw)

    def __repr__(self):
        sio = StringIO()
        self._print(0, sio)
        ret = sio.getvalue()
        sio.close()
        return ret
        
    @classmethod
    def init(cls, ex, rest):
        for r in rest:
            kw = dict(zip(cls._fields, concat(ex, r)))
            ex = cls(**kw)
        return ex

class Expr(AST):
    precedence = 100
    pass

class ConstantExpr(Expr):
    pass

class Integer(ConstantExpr):
    _fields = ['value', 'mod', 'rep']
    _defval = [0, '', '']

    @staticmethod
    def init(value, mod):
        rep = 'dec'
        value = value.lower().replace('_', '')
        if value.startswith('0x'):
            rep = 'hex'
        elif value.startswith('0') and value != '0':
            rep = 'oct'
        value = int(value, 0)
        return Integer(value=value, mod=mod, rep=rep)

class Float(ConstantExpr):
    _fields = ['value', 'mod', 'rep']
    _defval = [0, '', '']
    def init(value, mod):
        rep = 'dec'
        value = value.lower().replace('_', '')
        if value.startswith('0x'):
            rep = 'hex'
        value = float(value)
        return Integer(value=value, mod=mod, rep=rep)

class Char(ConstantExpr):
    _fields = ['value']
    _defval = ['']

class String(ConstantExpr):
    _fields = ['value', 'mod']
    _defval = ['', '']

class SizeOf(ConstantExpr):
    _fields = ['expr']
    _defval = ['']

class Array(Expr):
    _fields = ['expr']
    _defval = [0]

class Identifier(Expr):
    _fields = ['name']
    _defval = [None]

class Assign(Expr):
    precedence = 2
    _fields = ['left', 'right']
    _defval = [None, None]
    @classmethod
    def init(cls, left, op, right):
        if op != '=':
            op = op[:-1]
            right = BinOp(left=left, op=op, right=right)
        return Assign(left=left, right=right)

class Field(Expr):
    operators = {
        '.':  ('field', 14),
        '->': ('ptrfield', 14),
    }
    _fields = ['expr', 'op', 'field']
    _defval = [None, None, None]

class Subscript(Expr):
    precedence = 15
    _fields = ['expr', 'index']
    _defval = [None, None]

class UnaryOp(Expr):
    operators = {
        '+': ('pos', 14),
        '-': ('neg', 14),
        '~': ('inv', 14),
        '!': ('not', 14),
        '&': ('addrof', 14),
        '*': ('deref', 14),
    }
    _fields = ['op', 'operand']
    _defval = [None, None]

class PrefixOp(Expr):
    operators = {
        '++': ('preinc', 14),
        '--': ('predec', 14),
    }
    _fields = ['op', 'operand']
    _defval = [None, None]

class PostfixOp(Expr):
    operators = {
        '++': ('postinc',15),
        '--': ('postdec',15),
    }
    _fields = ['op', 'operand']
    _defval = [None, None]

    @classmethod
    def init(cls, left, rest):
        for r in rest:
            if isinstance(r, (Field, Subscript, Call)):
                r.expr = left
                left = r
            elif isinstance(r, PostfixOp):
                r.operand = left
                left = r
            else:
                raise TypeError('Unknown type in PostfixOp', r.__class__.__name__)
        return left

class BoolOp(Expr):
    operators = {
        '&&': ('logand', 5),
        '||': ('logor', 4),
    }
    _fields = ['left', 'op', 'right']
    _defval = [None, None, None]

class BinOp(Expr):
    operators = {
        '*': ('mul', 13),
        '/': ('div', 13),
        '%': ('mod', 13),
        '+': ('add', 12),
        '-': ('sub', 12),
        '<<': ('shl', 11),
        '>>': ('shr', 11),
        '&': ('and', 8),
        '|': ('or', 7),
        '^': ('xor', 6),
    }
    _fields = ['left', 'op', 'right']
    _defval = [None, None, None]

class Compare(Expr):
    operators = {
        '==': ('eq', 9),
        '!=': ('ne', 9),
        '<':  ('lt', 10),
        '<=': ('le', 10),
        '>':  ('gt', 10),
        '>=': ('ge', 10),
    }
    _fields = ['left', 'op', 'right']
    _defval = [None, None, None]

class ConditionalOp(Expr):
    precedence = 3
    _fields = ['expr', 'body', 'orelse']
    _defval = [None, None, None]

class Cast(Expr):
    precedence = 14
    _fields = ['value', 'type']
    _defval = [None, None]

    @classmethod
    def init(cls, ex, rest):
        for r in reversed(rest):
            kw = dict(zip(cls._fields, concat(ex, r)))
            ex = cls(**kw)
        return ex

class Call(Expr):
    precedence = 15
    _fields = ['expr', 'args']
    _defval = [None, None]

class ExprList(Expr):
    precedence = 2
    _fields = ['body']
    _defval = [None]

    @classmethod
    def init(cls, ex, rest):
        if rest:
            ex = ExprList(body=[ex]+rest)
        return ex

class Statement(AST):
    pass

class StatementList(AST):
    _fields = ['body']
    _defval = [[]]
    pass

class While(Statement):
    _fields = ['expr', 'stmt']
    _defval = [None, None]

class DoWhile(Statement):
    _fields = ['expr', 'stmt']
    _defval = [None, None]

class For(Statement):
    _fields = ['initialize', 'expr', 'ctrl', 'stmt']
    _defval = [None, None, None, None]

class Break(Statement):
    pass

class Continue(Statement):
    pass

class Label(Statement):
    _fields = ['name']
    _defval = [None]

class Goto(Statement):
    _fields = ['expr']
    _defval = [None]

class If(Statement):
    _fields = ['expr', 'stmt', 'orelse']
    _defval = [None, None, None]


class Switch(Statement):
    _fields = ['expr', 'body' ]
    _defval = [None, None]

class Case(Statement):
    _fields = ['value', 'body']
    _defval = [None, None]

class Default(Statement):
    _fields = ['body']
    _defval = [None]

class Return(Statement):
    _fields = ['expr']
    _defval = [None]

class Arguments(AST):
    _fields = ['args']
    _defval = [None]

class Initializer(AST):
    _fields = ['initializer', 'designator']
    _defval = [None, []]

    def set_designator(self, d):
        self.designator = d
        return self

class Declarator(Statement):
    _fields = ['name', 'type', 'array', 'args', 'returns', 'bitfield', 'initializer']
    _defval = [None, [], [], [], None, None, None]
    _extra = {'offset':None, 'szaligned': 0, 'reg':None, 'count':0, 'extra':None, 'impl':False}

    def set_initializer(self, i):
        self.initializer = i or None
        return self

    def set_bitfield(self, b):
        self.bitfield = b
        return self

    @staticmethod
    def decl(decl, spec):
        if isinstance(decl, list):
            return [Declarator.decl(d, spec) for d in decl]
        if not decl:
            decl = Declarator()

        if 'typedef' in spec:
            spec.remove('typedef')
            decl = decl._become(Typedef)

        if decl.returns:
            for s in spec:
                if s in ('extern', 'static'):
                    decl.type.insert(0,s)
                else:
                    Declarator.decl(decl.returns, [s])
        else:
            decl.type.extend(spec)
        return [decl]

    @staticmethod
    def mods(decl, modifiers):
        if isinstance(decl, list):
            return [Declarator.mods(d, modifiers) for d in decl]
        if not decl:
            decl = Declarator()

        modifiers = list(modifiers)
        for m in modifiers:
            if decl.returns:
                Declarator.mods(decl.returns, [m])
            elif isinstance(m, Array):
                decl.type.append('array')
                decl.array.append(m)
            elif isinstance(m, Arguments):
                decl.type.append('function')
                decl.args.extend(m.args)
                decl.returns = Declarator()
            elif isinstance(m, basestring):
                decl.type.append(m)
            elif isinstance(m, CompositeType):
                decl.type.append(m)
            else:
                error.fatal("Don't know how to handle declmods %r", m)
        return decl

class Typedef(Declarator):
    pass

class Enumerator(AST):
    _fields = ['name', 'value']
    _defval = [None, None]

class Enumeration(AST):
    _fields = ['name', 'body']
    _defval = [None, None]

class CompositeType(AST):
    _fields = ['name', 'body']
    _defval = [None, None]

    @classmethod
    def init(cls, type, name, body):
        if type == 'struct':
            cls = Struct
        elif type == 'union':
            cls = Union
        else:
            raise Exception('Expecting struct or union')
        if name:
            name = name.name
        return cls(name=name, body=body)

class Struct(CompositeType):
    pass

class Union(CompositeType):
    pass

class Attribute(AST):
    _fields = ['attr']
    _defval = [None]

class DeclSpec(AST):
    _fields = ['spec']
    _defval = [None]

class Function(AST):
    _fields = ['signature', 'body']
    _defval = [None, None, None]

    @classmethod
    def init(cls, signature, returns, body):
        Declarator.decl(signature, returns)
        return cls(signature=signature, body=body)

class TranslationUnit(AST):
    _fields = ['body']
    _defval = [None]
