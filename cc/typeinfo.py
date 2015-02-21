from copy import deepcopy
import error
import cast as C
import symtab
import iemit

size = {
    'char': (1, 1),
    'short': (2, 2),
    'int': (4, 4),
    'long': (8, 4),
    'float': (4, 4),
    'double': (8, 8),
    'pointer': (4, 4),
    'function': (0, 1),
    'void': (0, 1),
}

def sizeof(decl):
    guess = size['int']
    a = 0
    asz = 1
    tsz = None
    for t in decl.type:
        if isinstance(t, C.Struct):
            tmp = symtab.struct.find(t.name)
            guess = tmp.sizeof()
            break
        if isinstance(t, C.Union):
            tmp = symtab.union.find(t.name)
            guess = tmp.sizeof()
            break
        if t == 'array':
            expr = decl.array[a].expr
            val = iemit.const_eval(expr)
            asz *= val
            a += 1
            continue
        tsz = size.get(t)
        if tsz:
            guess = tsz[0]
            break

    if guess:
        return guess*asz        

    error.fatal('Cannot determine size of %r', decl.name)
    return 0

def alignof(decl):
    sz = None
    align = None
    for t in decl.type:
        sz, align = size.get(t, (None, None))
        if sz is not None:
            return align

    error.warn('Cannot determine align of %r', decl.name)
    return 1

def stackalign(decl):
    return size['pointer'][1]

def copyof(decl):
    d = deepcopy(decl)
    d.name = None
    d.count = 0
    return d

def isptr(decl):
    for t in decl.type:
        if t in ('pointer', 'array'):
            return True
    return False

def isarray(decl):
    if 'array' in decl.type:
        # Arrays that are function parameters are not really arrays
        if decl.offset is not None and decl.offset < 0:
            return False
        return True
    return False

def issigned(decl):
    return 'unsigned' not in decl.type

def isstatic(decl):
    return 'static' in decl.type

def isextern(decl):
    return 'extern' in decl.type

def deref(decl):
    d = copyof(decl)
    d.name = None
    d.offset = None
    for i, v in enumerate(d.type):
        if v in ('pointer', 'array'):
            break
    else:
        error.fatal('Cannot dereference %r', decl.name)

    d.type.pop(i)
    if v == 'array':
        d.array.pop()
    return d

def addrof(decl):
    d = copyof(decl)
    d.name = None
    d.offset = None
    try:
        i = d.type.index('array')
        d.type[i] = 'pointer'
        d.array.pop()
    except ValueError:
        d.type.insert(0, 'pointer')
    return d

def typeof(decl):
    type = decl.type
    while type:
        for t in type:
            if t in size:
                return t
            if isinstance(t, (C.Struct, C.Union)):
                return t
            t = symtab.typedef.find(t)
            if t:
                type = t.type
                break
        else:
            break
    return 'int'
