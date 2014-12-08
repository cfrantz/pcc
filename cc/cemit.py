######################################################################
#
# cemit.py: Walk and emit a C-AST as C code
#
#
######################################################################
import cast as C

_tabstop = 4
_emitters = {}
def emitter(f):
    _, name = f.func_name.split('_', 1)
    _emitters[name] = f
    return f

def emit(node, indent=0):
    nn = node.__class__.__name__
    fn = _emitters[nn]
    return fn(node, indent)

operators = {
    'field': '.',
    'ptrfield': '->',
    'pos': '+',
    'neg': '-',
    'inv': '~',
    'not': '!',
    'addrof': '&',
    'deref': '*',
    'preinc': '++',
    'predec': '--',
    'postinc': '++',
    'postdec': '--',
    'logand': '&&',
    'logor': '||',
    'mul': '*',
    'div': '/',
    'mod': '%',
    'add': '+',
    'sub': '-',
    'shl': '<<',
    'shr': '>>',
    'and': '&',
    'or': '|',
    'xor': '^',
    'eq': '==',
    'ne': '!=',
    'lt': '<',
    'le': '<=',
    'gt': '>',
    'ge': '>=',
}

def emitp(node, side):
    side = getattr(node, side)
    val = emit(side)
    if side.precedence < node.precedence:
        val = '(%s)' % val
    return val

@emitter
def emit_Integer(node, indent):
    if node.rep == 'oct':
        val = '0%o' % node.value
    elif node.rep == 'hex':
        val = '0x%x' % node.value
    else:
        val = '%d' % node.value
    return val + node.mod

@emitter
def emit_Float(node, indent):
    if node.rep == 'hex':
        val = node.value.hex()
    else:
        val = str(node.value)
    return val.node.mod

@emitter
def emit_Char(node, indent):
    return "'%s'" % node.value

@emitter
def emit_String(node, indent):
    return '"%s"' % node.value

@emitter
def emit_Identifier(node, indent):
    return node.name

@emitter
def emit_Array(node, indent):
    if node.expr:
        return emit(node.expr)
    return ''

@emitter
def emit_Assign(node, indent):
    return '%s = %s' % (emitp(node, 'left'), emitp(node, 'right'))

def emit_binary_expr(node):
    return '%s %s %s' % (emitp(node, 'left'), operators[node.op], emitp(node, 'right'))

@emitter
def emit_BoolOp(node, indent):
    return emit_binary_expr(node)

@emitter
def emit_BinOp(node, indent):
    return emit_binary_expr(node)

@emitter
def emit_Compare(node, indent):
    return emit_binary_expr(node)

@emitter
def emit_PrefixOp(node, indent):
    return '%s%s' % (operators[node.op], emitp(node, 'operand'))

@emitter
def emit_UnaryOp(node, indent):
    return '%s%s' % (operators[node.op], emitp(node, 'operand'))

@emitter
def emit_PostfixOp(node, indent):
    return '%s%s' % (emitp(node, 'operand'), operators[node.op])

@emitter
def emit_ConditionalOp(node, indent):
    return '%s ? %s : %s' % (
            emitp(node, 'expr'), emitp(node, 'body'), emitp(node, 'orelse'))

@emitter
def emit_Cast(node, indent):
    return '(%s)%s' % (emit(node.type), emitp(node, 'value'))

@emitter
def emit_Call(node, indent):
    a = [emit(x) for x in node.args]
    return '%s(%s)' % (emitp(node, 'expr'), ', '.join(a))

def emit_body(body, indent):
    ret = []
    ind = ' ' * (_tabstop*indent)
    for b in body:
        val = emit(b, indent)
        if isinstance(b, (C.Expr, C.Declarator)):
            ret.append(ind+val+';')
        elif isinstance(b, C.Enumerator):
            ret.append(ind+val+',')
        else:
            ret.append(ind+val)
    return ret

@emitter
def emit_StatementList(node, indent):
    ret = ['{']
    ret.extend(emit_body(node.body, indent))
    ind = ' ' * (_tabstop*(indent-1))
    ret.append(ind + '}')
    return '\n'.join(ret)

@emitter
def emit_While(node, indent):
    i = indent+1
    return 'while(%s) %s' % (emit(node.expr, i), emit(node.stmt, i))

@emitter
def emit_DoWhile(node, indent):
    i = indent+1
    return 'do %s while(%s);' % (emit(node.stmt, i), emit(node.expr, i))

@emitter
def emit_For(node, indent):
    i = indent+1
    return 'for(%s; %s; %s) %s' % (
        emit(node.initialize, i), emit(node.expr, i), emit(node.ctrl, i),
        emit(node.stmt, i))

@emitter
def emit_If(node, indent):
    i = indent+1
    ret = 'if (%s) %s' % (emit(node.expr, i), emit(node.stmt, i))
    if node.orelse:
        ret += ' else %s' % emit(node.orelse, i)
    return ret

@emitter
def emit_Break(node, indent):
    return 'break;'

@emitter
def emit_Continue(node, indent):
    return 'continue;'

@emitter
def emit_Label(node, indent):
    return '%s:' % node.name

@emitter
def emit_Goto(node, indent):
    i = indent+1
    return 'goto %s;' % emit(node.expr, i)

@emitter
def emit_Switch(node, indent):
    i = indent+1
    return 'switch(%s) %s' % (emit(node.expr, i), emit(node.stmt, i))

@emitter
def emit_Case(node, indent):
    return 'case %s:' % emit(node.expr)

@emitter
def emit_Default(node, indent):
    return 'default:'

@emitter
def emit_Return(node, indent):
    i = indent+1
    return 'return %s;' % emit(node.expr, i)

def emit_declarator(node):
    func = False
    pointer = False
    arr = 0
    left = []
    right = []
    if node.name:
        left.append(node.name)

    for t in node.type:
        if t == 'pointer':
            left.insert(0, '*')
            pointer = True
        elif t == 'function':
            func = True
            if pointer:
                left.insert(0, '(')
                right.append(')')
        elif t == 'array':
            left.append('[%s]' % emit(node.array[arr], 0))
            arr += 1
        else:
            if len(left):
                left.insert(0, ' ')
            if isinstance(t, C.AST):
                left.insert(0, emit(t))
            else:
                left.insert(0, t)

    if func:
        rleft, rright = emit_declarator(node.returns)
        left.insert(0, ' ')
        left.insert(0, rleft)
        args = '(' + ', '.join([emit(x) for x in node.args]) + ')'
        right.append(args)
        right.append(rright)

    if node.initializer:
        right.append(' = ')
        if isinstance(node.initializer, list):
            right.append('{')
            for i in node.initializer:
                right.append(emit(i))
                right.append(', ')
            right.append('}')
        else:
            right.append(emit(node.initializer))

    return ''.join(left), ''.join(right)

@emitter
def emit_Declarator(node, indent):
    left, right = emit_declarator(node)
    return left+right

@emitter
def emit_Typedef(node, indent):
    left, right = emit_declarator(node)
    return 'typedef ' + left+right

@emitter
def emit_Initializer(node, indent):
    i = ''
    for d in node.designator:
        if isinstance(d, C.Identifier):
            i += '.%s' % emit(d)
        else:
            i += '[%s]' % emit(d)

    if i:
        i = '%s = %s' % (i, emit(node.initializer))
    else:
        i = emit(node.initializer)
    return i
    
@emitter
def emit_Enumerator(node, indent):
    name = node.name
    if node.value:
        name += ' = %s' % emit(node.value, indent+1)
    return name

def emit_CompositeType(type, node, indent):
    if node.body:
        ret = ['%s %s {' % (type, node.name) ]
        ret.extend(emit_body(node.body, indent+1))
        ret.append('}')
        ret = '\n'.join(ret)
    else:
        ret = '%s %s' % (type, node.name)
    return ret

@emitter
def emit_Enumeration(node, indent):
    return emit_CompositeType('enum', node, indent)

@emitter
def emit_Struct(node, indent):
    return emit_CompositeType('struct', node, indent)

@emitter
def emit_Union(node, indent):
    return emit_CompositeType('union', node, indent)

@emitter
def emit_Function(node, indent):
    decl = emit(node.signature)
    if node.body:
        ret = '%s\n%s' % (decl, emit(node.body, indent+1))
    else:
        ret = decl + ';'
    return ret

@emitter
def emit_TranslationUnit(node, indent):
    ret = emit_body(node.body, indent)
    return '\n'.join(ret)
