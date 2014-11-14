######################################################################
#
# cemit.py: Walk and emit a C-AST as C code
#
#
######################################################################
import cast as C

_emitters = {}
def emitter(f):
    _, name = f.func_name.split('_', 1)
    _emitters[name] = f
    return f

def emit(node):
    nn = node.__class__.__name__
    fn = _emitters[nn]
    return fn(node)

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
def emit_Integer(node):
    if node.rep == 'oct':
        val = '0%o' % node.value
    elif node.rep == 'hex':
        val = '0x%x' % node.value
    else:
        val = '%d' % node.value
    return val + node.mod

@emitter
def emit_Float(node):
    if node.rep == 'hex':
        val = node.value.hex()
    else:
        val = str(node.value)
    return val.node.mod

@emitter
def emit_Char(node):
    return "'%s'" % node.value

@emitter
def emit_String(node):
    return '"%s"' % node.value

@emitter
def emit_Identifier(node):
    return node.name

@emitter
def emit_Array(node):
    return emit(node.expr)

@emitter
def emit_Assign(node):
    return '%s = %s' % (emitp(node, 'left'), emitp(node, 'right'))

def emit_binary_expr(node):
    return '%s %s %s' % (emitp(node, 'left'), operators[node.op], emitp(node, 'right'))

@emitter
def emit_BoolOp(node):
    return emit_binary_expr(node)

@emitter
def emit_BinOp(node):
    return emit_binary_expr(node)

@emitter
def emit_Compare(node):
    return emit_binary_expr(node)

@emitter
def emit_PrefixOp(node):
    return '%s%s' % (operators[node.op], emitp(node, 'operand'))

@emitter
def emit_PostfixOp(node):
    return '%s%s' % (emitp(node, 'operand'), operators[node.op])

@emitter
def emit_ConditionalOp(node):
    return '%s ? %s : %s' % (
            emitp(node, 'expr'), emitp(node, 'body'), emitp(node, 'orelse'))

@emitter
def emit_Cast(node):
    return '(%s)%s' % (emit(node.type), emitp(node, 'value'))

@emitter
def emit_Call(node):
    a = [emit(x) for x in node.args]
    return '%s(%s)' % (emitp(node, 'expr'), ', '.join(a))

def emit_body(body):
    ret = []
    for b in body:
        val = emit(b)
        if isinstance(b, (C.Expr, C.Declarator)):
            ret.append(val+';')
        elif isinstance(b, C.Enumerator):
            ret.append(val+',')
        else:
            ret.append(val)
    return ret

@emitter
def emit_StatementList(node):
    ret = ['{']
    ret.extend(emit_body(node.body))
    ret.append('}')
    return '\n'.join(ret)

@emitter
def emit_While(node):
    return 'while(%s) %s' % (emit(node.expr), emit(node.stmt))

@emitter
def emit_DoWhile(node):
    return 'do %s while(%s);' % (emit(node.stmt), emit(node.expr))

@emitter
def emit_For(node):
    return 'for(%s; %s; %s) %s' % (
        emit(node.initialize), emit(node.expr), emit(node.ctrl),
        emit(node.stmt))

@emitter
def emit_If(node):
    ret = 'if (%s) %s' % (emit(node.expr), emit(node.stmt))
    if node.orelse:
        ret += ' else %s' % emit(node.orelse)
    return ret

@emitter
def emit_Break(node):
    return 'break;'

@emitter
def emit_Continue(node):
    return 'continue;'

@emitter
def emit_Label(node):
    return '%s:' % node.name

@emitter
def emit_Goto(node):
    return 'goto %s;' % emit(node.expr)

@emitter
def emit_Switch(node):
    return 'switch(%s) %s' % (emit(node.expr), emit(node.stmt))

@emitter
def emit_Case(node):
    return 'case %s:' % emit(node.expr)

@emitter
def emit_Default(node):
    return 'default:'

@emitter
def emit_Return(node):
    return 'return %s;' % emit(node.expr)

def emit_declarator(node):
    func = False
    pointer = False
    arr = 0
    name = node.name or ''
    left = [name]
    right = []
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
            left.append('[%s]' % emit(node.array[arr]))
            arr += 1
        else:
            if isinstance(t, C.AST):
                left.insert(0, emit(t))
            else:
                left.insert(0, ' ')
                left.insert(0, t)

    if func:
        rleft, rright = emit_declarator(node.returns)
        left.insert(0, rleft)
        args = '(' + ', '.join([emit(x) for x in node.args]) + ')'
        right.append(args)
        right.append(rright)

    if node.initializer:
        right.append(' = ')
        right.append(emit(node.initializer))

    return ''.join(left), ''.join(right)

@emitter
def emit_Declarator(node):
    left, right = emit_declarator(node)
    return left+right
    
@emitter
def emit_Enumerator(node):
    name = node.name
    if node.value:
        name += ' = %s' % emit(node.value)
    return name

def emit_CompositeType(type, node):
    if node.body:
        ret = ['%s %s {' % (type, node.name) ]
        ret.extend(emit_body(node.body))
        ret.append('}')
        ret = '\n'.join(ret)
    else:
        ret = '%s %s' % (type, node.name)
    return ret


@emitter
def emit_Enumeration(node):
    return emit_CompositeType('enum', node)

@emitter
def emit_Struct(node):
    return emit_CompositeType('struct', node)

@emitter
def emit_Union(node):
    return emit_CompositeType('union', node)

@emitter
def emit_Function(node):
    decl = '%s %s' % (emit(node.returns), emit(node.signature))
    if node.body:
        ret = '%s\n%s' % (decl, emit(node.body))
    else:
        ret = decl + ';'
    return ret

@emitter
def emit_TranslationUnit(node):
    ret = emit_body(node.body)
    return '\n'.join(ret)
