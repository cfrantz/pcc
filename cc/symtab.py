from collections import OrderedDict
import error
import typeinfo

class Symtab(object):
    def __init__(self, name, type=None):
        self.name = name
        self.stack = []
        self.pos = 0
        self.set_align(type)
        self.tab = OrderedDict()

    def enter(self, symbol, value, extra=None):
        #print "Entering %s into %s (value=%s)" % (symbol, self.name, value)
        if symbol in self.tab:
            if self.tab[symbol] is not None:
                if value is None:
                    return
                error.fatal('%s %r already defined', self.name, symbol)

        self.tab[symbol] = value
        if value is None:
            return
            
        value.extra = extra
        if not value.name:
            value.name = symbol

        if self.type in ('stack', 'struct') and symbol[0] != '%':
            self.alloc(symbol, value)
        elif self.type == 'union':
            self.pos = max(self.pos, typeinfo.sizeof(value))

    def find(self, symbol):
        return self.tab.get(symbol)

    def alloc(self, symbol, value=None):
        if value is None:
            value = self.tab[symbol]
        # Don't allocate space if it's already been allocated
        if value.offset is not None:
            return
        align = self.alignof(value)
        pos = (self.pos+align-1) & ~(align-1)
        value.offset = pos
        self.pos = pos + typeinfo.sizeof(value)

    def set_align(self, type):
        self.type = type
        if type == 'stack':
            self.alignof = typeinfo.stackalign
        elif type == 'struct':
            self.alignof = typeinfo.alignof

    def sizeof(self):
        return self.pos

class Stack(object):
    def __init__(self, name):
        self.name = name
        self.stack = []

    def push(self, val):
        self.stack.insert(0, val)

    def pop(self):
        return self.stack.pop(0)

    def top(self):
        return self.stack[0]

    def find(self, symbol):
        for s in self.stack:
            val = s.find(symbol)
            if val:
                return s, val
        error.fatal('%s: could not resolve %r', self.name, symbol)
        return None, None

    def get(self, symbol):
        return self.find(symbol)[1]

    def enter(self, symbol, value, extra=None):
        self.stack[0].enter(symbol, value, extra)

struct = Symtab('struct')
union = Symtab('union')
enum = Symtab('enum')
typedef = Symtab('typedef')
ident = Stack('identifier')
