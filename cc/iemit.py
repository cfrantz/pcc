######################################################################
#
# iemit.py: Walk a C AST and emit Intermediate Assembler
#
# Abbrieviations used in this module:
#
# lhs, rhs: left-hand-side, right-hand-side
# lrr, rrr: left-result-register, right-result-register
# er, br: expression-result, body-result
# ea: effective-address
# 
######################################################################
import cast as C
from ia import *
import symtab
import typeinfo
import error

brk_stack = symtab.Stack('break')
cont_stack = symtab.Stack('continue')
ret_stack = symtab.Stack('return')
globaltab = symtab.Symtab('globals', 'global')

######################################################################
# Decorator for emitter functions; emit dispatcher
######################################################################
_emitters = {}
def emitter(f):
    _, name = f.func_name.split('_', 1)
    _emitters[name] = f
    return f

def emit(node, ea=False):
    nn = node.__class__.__name__
    fn = _emitters[nn]
    return fn(node, ea)

######################################################################
# Anonymous and temporaries
######################################################################
_anon = 1
def anon(s):
    global _anon
    x = _anon
    _anon += 1
    return '%s_%d' % (s, x)

_tmpreg = 1
def tmpreg():
    global _tmpreg
    x = _tmpreg
    _tmpreg += 1
    return "%%%d" % x

def tmpreg_reset():
    _tmpreg = 1

######################################################################
# Convienience functions
######################################################################
def _typehelper(sym, fn):
    _, tp = symtab.ident.find(sym)
    return fn(tp)

def sizeof(sym):
    return _typehelper(sym, typeinfo.sizeof)

def issigned(sym):
    return _typehelper(sym, typeinfo.issigned)

def isptr(sym):
    return _typehelper(sym, typeinfo.isptr)

def isarray(sym):
    return _typehelper(sym, typeinfo.isarray)

def deref(sym):
    return _typehelper(sym, typeinfo.deref)

def addrof(sym):
    return _typehelper(sym, typeinfo.addrof)

def typeof(sym):
    return _typehelper(sym, typeinfo.typeof)

def copyof(sym):
    return _typehelper(sym, typeinfo.copyof)

######################################################################
# Instruction List
######################################################################
class IList(list):
    def __init__(self, *args, **kw):
        result = kw.pop('result', None)
        list.__init__(self, *args, **kw)
        self.result = result

    def _checkregs(self, inst):
        for k in inst._fields:
            if k not in ('target', 'src0', 'src1', 'addr', 'args', 'retval'):
                continue
            v = getattr(inst, k, None)
            if v is None:
                continue
            if k == 'args':
                for vv in v:
                    _, tp = symtab.ident.find(vv)
                    if tp:
                        tp.last_instr = inst
                        tp.count += 1
            else:
                _, tp = symtab.ident.find(v)
                if tp:
                    tp.last_instr = inst
                    tp.count += 1

    def sympush(self, tab):
        self.append(SymPush(symtab=tab))
        symtab.ident.push(tab)

    def sympop(self):
        self.append(SymPop())
        return symtab.ident.pop()

    def append(self, *instructions):
        for i in instructions:
            if isinstance(i, IList):
                self.extend(i)
            else:
                self._checkregs(i)
                list.append(self, i)
        return self

def isconst(ilist):
    if len(ilist)==1 and isinstance(ilist[0], Constant):
        return True
    return False

def const_eval(expr):
    node = emit(expr)
    if isconst(node):
        return node[0].val
    error.fatal("Expecting constant expression: %r", expr)

######################################################################
# Pseudo node for capturing state and passing it on to other emitters
######################################################################
class PassThrough(C.AST):
    _fields = ['code']
    def __init__(self, code):
        self.code = code

@emitter
def emit_PassThrough(node, ea):
    return node.code

######################################################################
# Emitters for AST nodes
######################################################################

@emitter
def emit_Integer(node, ea):
    ret = IList(result=tmpreg())
    tp = []
    signed = True
    if 'U' in node.mod:
        tp.append('unsigned')
        signed = False
    if 'L' in node.mod:
        tp.append('long')
    else:
        tp.append('int')
    tp = C.Declarator(type=tp)
    sz = typeinfo.sizeof(tp)
    symtab.ident.enter(ret.result, tp)
    ret.append(Constant(target=ret.result, val=node.value, signed=signed, size=sz))
    return ret

@emitter
def emit_Float(node, ea):
    ret = IList(result=tmpreg())
    tp = []
    if 'F' in node.mod:
        tp.append('float')
    else:
        tp.append('double')
    tp = C.Declarator(type=tp)
    sz = typeinfo.sizeof(tp)
    symtab.ident.enter(ret.result, tp)
    ret.append(Constant(target=ret.result, val=node.value, size=sz))
    return ret

@emitter
def emit_Char(node, ea):
    ret = IList(result=tmpreg())
    tp = C.Declarator(type=['char'])
    symtab.ident.enter(ret.result, tp)
    val = ord(node.value.decode('unicode_escape'))
    ret.append(Constant(target=ret.result, val=val, size=1))
    return ret

@emitter
def emit_String(node, ea):
    ret = IList(result=tmpreg())
    name = anon('str')
    val= node.value.decode('unicode_escape')
    ret.append(Data(name=name, type='str', data=val))
    tp = C.Declarator(type=['pointer', 'const', 'char'])
    sz = typeinfo.sizeof(tp)
    globaltab.enter(name, typeinfo.copyof(tp))
    symtab.ident.enter(ret.result, tp)
    ret.append(EffectiveAddr(target=ret.result, symbol=name))
    return ret

@emitter
def emit_SizeOf(node, ea):
    ret = IList(result=tmpreg())
    tp = C.Declarator(type=['int'])
    symtab.ident.enter(ret.result, tp)
    val = emit(node.expr, True)
    #print "SizeOf", node.expr, val, val.result
    ret.append(Constant(target=ret.result, val=typeinfo.sizeof(deref(val.result))))
    return ret

@emitter
def emit_Identifier(node, ea):
    ret = IList()
    addr = tmpreg()
    _, tp = symtab.ident.find(node.name)
    symtab.ident.enter(addr, typeinfo.addrof(tp))
    ret.append(EffectiveAddr(target=addr, symbol=node.name))
    if ea or typeinfo.isarray(tp):
        ret.result = addr
    else:
        value = tmpreg()
        tp = copyof(node.name)
        symtab.ident.enter(value, typeinfo.copyof(tp))
        ret.append(Load(target=value, addr=addr,
            size=typeinfo.sizeof(tp), signed=typeinfo.issigned(tp)))
        ret.result = value
    return ret

@emitter
def emit_Array(node, ea):
    if node.expr:
        return emit(node.expr)
    return ''

@emitter
def emit_Assign(node, ea):
    rhs = emit(node.right)
    lhs = emit(node.left, ea=True)
    rhs.extend(lhs)
    rhs.append(Store(addr=lhs.result,src0=rhs.result,
                     size=typeinfo.sizeof(deref(lhs.result))))
    return rhs

@emitter
def emit_BoolOp(node, ea):
    ret = IList(result=tmpreg())
    symtab.ident.enter(ret.result, C.Declarator(type=['int']))
    shortcir = anon('lbl')
    lhs = emit(node.left)
    ret.extend(lhs)
    ret.append(Move(target=ret.result, src0=lhs.result))
    if node.op == 'logand': 
        ret.append(IfFalse(src0=lhs.result, label=shortcir))
    else:
        ret.append(IfTrue(src0=lhs.result, label=shortcir))

    rhs = emit(node.right)
    ret.extend(rhs)
    ret.append(Move(target=ret.result, src0=rhs.result))
    ret.append(Label(name=shortcir))
    return ret

def emit_binop(node, ea):
    ret = IList(result=tmpreg())
    const = False
    lhs = emit(node.left)
    rhs = emit(node.right)

    if isconst(lhs) and isconst(rhs):
        const = True

    if node.op == 'sub' and isptr(lhs.result) and isptr(rhs.result):
        ret.append(lhs, rhs)
        symtab.ident.enter(ret.result, C.Declarator(type=['long']))
        sz = typeinfo.sizeof(deref(lhs.result))
        tmp = tmpreg()
        sztmp = tmpreg()
        ret.append(
            Sub(target=tmp, src0=lhs.result, src1=rhs.result),
            Constant(target=sztmp, val=sz),
            Div(target=ret.result, src0=tmp, src1=sztmp))
        return ret
    elif node.op in ('add', 'sub') and isptr(lhs.result):
        _, tp = symtab.ident.find(lhs.result)
        symtab.ident.enter(ret.result, copyof(lhs.result))
        op = Add if node.op == 'add' else Sub
        dr = deref(lhs.result)
        sz = typeinfo.sizeof(deref(lhs.result))
        rhs = emit(C.BinOp(left=node.right, op='*', right=C.Integer(value=sz)))
        ret.append(
            lhs,
            rhs,
            op(target=ret.result, src0=lhs.result, src1=rhs.result))
        return ret
    elif node.op == 'add' and isptr(rhs.result):
        symtab.ident.enter(ret.result, copyof(rhs.result))
        op = Add if node.op == 'add' else Sub
        dr = deref(rhs.result)
        sz = typeinfo.sizeof(deref(rhs.result))
        lhs = emit(C.BinOp(left=node.left, op='*', right=C.Integer(value=sz)))
        ret.append(
            lhs,
            rhs,
            op(target=ret.result, src0=lhs.result, src1=rhs.result))
        return ret

    if not const:
        ret.extend(lhs)
        ret.extend(rhs)

    if node.op in ('eq', 'ne', 'lt', 'le', 'gt', 'ge'):
        symtab.ident.enter(ret.result, C.Declarator(type=['int']))
    elif sizeof(rhs.result) > sizeof(lhs.result):
        symtab.ident.enter(ret.result, copyof(rhs.result))
    else:
        symtab.ident.enter(ret.result, copyof(lhs.result))

    if node.op == 'add':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val + rhs[0].val)))
        else:
            ret.append(Add(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'sub':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val - rhs[0].val)))
        else:
            ret.append(Sub(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'mul':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val * rhs[0].val)))
        else:
            ret.append(Mul(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'div':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val / rhs[0].val)))
        else:
            ret.append(Div(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'mod':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val % rhs[0].val)))
        else:
            ret.append(Mod(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'and':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val & rhs[0].val)))
        else:
            ret.append(And(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'or':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val | rhs[0].val)))
        else:
            ret.append(Or(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'xor':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val ^ rhs[0].val)))
        else:
            ret.append(Xor(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'shl':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val << rhs[0].val)))
        else:
            ret.append(Shl(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'shr':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val >> rhs[0].val)))
        else:
            ret.append(Shr(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'eq':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val == rhs[0].val)))
        else:
            ret.append(Eq(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'ne':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val != rhs[0].val)))
        else:
            ret.append(Ne(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'gt':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val > rhs[0].val)))
        else:
            ret.append(Gt(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'ge':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val >= rhs[0].val)))
        else:
            ret.append(Ge(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'lt':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val < rhs[0].val)))
        else:
            ret.append(Lt(target=ret.result, src0=lhs.result, src1=rhs.result))
    elif node.op == 'le':
        if const:
            ret.append(Constant(target=ret.result, val=(lhs[0].val <= rhs[0].val)))
        else:
            ret.append(Le(target=ret.result, src0=lhs.result, src1=rhs.result))

    return ret

@emitter
def emit_BinOp(node, ea):
    return emit_binop(node, ea)

@emitter
def emit_Compare(node, ea):
    return emit_binop(node, ea)

@emitter
def emit_PrefixOp(node, ea):
    one = C.Integer(value=1)
    if node.op == 'preinc':
        op = C.BinOp(left=node.operand, op='+', right=one)
    else:
        op = C.BinOp(left=node.operand, op='-', right=one)
    op = C.Assign(left=node.operand, right=op)
    return emit(op)

@emitter
def emit_UnaryOp(node, ea):
    const = False
    if node.op == 'addrof':
        return emit(node.operand, ea=True)

    code = emit(node.operand)
    if node.op == 'pos':
        return code

    ret = IList(result=tmpreg())
    if isconst(code):
        const = True
    else:
        ret.extend(code)

    if node.op == 'neg':
        symtab.ident.enter(ret.result, copyof(code.result))
        if const:
            ret.append(Constant(target=ret.result, val=-code[0].val))
        else:
            ret.append(Negate(target=ret.result, src0=code.result))
    elif node.op == 'inv':
        symtab.ident.enter(ret.result, copyof(code.result))
        if const:
            ret.append(Constant(target=ret.result, val=~code[0].val))
        else:
            ret.append(Complement(target=ret.result, src0=code.result))
    elif node.op == 'not':
        symtab.ident.enter(ret.result, C.Declarator(type=['int']))
        if const:
            ret.append(Constant(target=ret.result, val=not code[0].val))
        else:
            ret.append(Not(target=ret.result, src0=code.result))
    elif node.op == 'addrof':
        # FIXME can't get here
        pass
    elif node.op == 'deref':
        if not ea:
            tp = deref(code.result)
            symtab.ident.enter(ret.result, tp)
            ret.append(Load(target=ret.result, addr=code.result,
                size=typeinfo.sizeof(tp), signed=typeinfo.issigned(tp)))
        else:
            symtab.ident.enter(ret.result, copyof(code.result))
            ret.result = code.result
    return ret

@emitter
def emit_PostfixOp(node, ea):
    one = C.Integer(value=1)
    pre = PassThrough(emit(node.operand))

    if node.op == 'postinc':
        op = C.BinOp(left=pre, op='+', right=one)
    else:
        op = C.BinOp(left=pre, op='-', right=one)
    op = C.Assign(left=node.operand, right=op)
    code = emit(op)
    code.result = pre.code.result
    return code

@emitter
def emit_Field(node, ea):
    ret = IList(result=tmpreg())
    if node.op == 'field':
        code = emit(node.expr, ea=True)
    else:
        code = emit(node.expr)
    _, x = symtab.ident.find(code.result)
    tp = typeinfo.typeof(deref(code.result))
    if isinstance(tp, C.Struct):
        tab = symtab.struct.find(tp.name)
    elif isinstance(tp, C.Union):
        tab = symtab.union.find(tp.name)
    else:
        error.fatal('Asked for %r in something not struct or union (%r)', node.field, tp)

    field = tab.find(node.field)
    if not field:
        error.fatal('Cannot find field %r in %r', node.field, tp.name)

    tmp = tmpreg()
    symtab.ident.enter(tmp, typeinfo.addrof(field))
    if isconst(code):
        ret.append(Constant(target=tmp, val=(code[0].val + field.offset)))
    else:
        ret.extend(code)
        offset = emit(C.Integer(value=field.offset))
        ret.append(
            offset,
            Add(target=tmp, src0=code.result, src1=offset.result))

    if ea:
        ret.result = tmp
    else:
        tp = copyof(field)
        symtab.ident.enter(ret.result, tp)
        ret.append(Load(target=ret.result, addr=tmp,
            size=typeinfo.sizeof(tp), signed=typeinfo.issigned(tp)))
    return ret

@emitter
def emit_Subscript(node, ea):
    code = emit(C.BinOp(left=node.expr, op='+', right=node.index))
    if ea:
        return code
    else:
        result = tmpreg()
        tp = deref(code.result)
        symtab.ident.enter(result, tp)
        code.append(Load(target=result, addr=code.result,
            size=typeinfo.sizeof(tp), signed=typeinfo.issigned(tp)))
        code.result = result
        return code

@emitter
def emit_ConditionalOp(node, ea):
    expr = emit(node.expr)
    if isconst(expr):
        if expr[0].val:
            return emit(node.body)
        else:
            return emit(node.orelse)

    ret = IList(result=tmpreg())
    ret.extend(expr)
    condelse = anon('lbl')
    condend = anon('lbl')

    ret.append(IfFalse(src0=expr.result, label=condelse))
    body = emit(node.body)
    # FIXME: make sure types of body and orelse are compatible
    symtab.ident.enter(ret.result, copyof(body.result))
    ret.extend(body)
    ret.append(Move(target=ret.result, src0=body.result))
    ret.append(Jump(label=condend))
    orelse = emit(node.orelse)
    ret.append(Label(name=condelse))
    ret.extend(orelse)
    ret.append(Move(target=ret.result, src0=orelse.result))
    ret.append(Label(name=condend))
    return ret

@emitter
def emit_Cast(node, ea):
    ret = IList(result=tmpreg())
    expr = emit(node.value)
    # FIXME: determine if expr can be casted to type
    #print "Cast", expr, node.type
    symtab.ident.enter(ret.result, node.type)
    if isconst(expr):
        ret.append(Constant(target=ret.result, val=expr[0].val,
            signed=typeinfo.issigned(node.type),
            size=typeinfo.sizeof(node.type)))
    else:
        ret.extend(expr)
        ret.append(Move(target=ret.result, src0=expr.result))
    return ret

@emitter
def emit_Call(node, ea):
    ret = IList(result=tmpreg())
    args = []

    for a in node.args:
        code = emit(a)
        ret.extend(code)
        args.append(code.result)

    if isinstance(node.expr, C.Identifier):
        target = node.expr.name
    else:
        expr = emit(node.expr)
        target = expr.result
        ret.append(expr)

    _, tp = symtab.ident.find(target)
    symtab.ident.enter(ret.result, typeinfo.copyof(tp.returns))
    ret.append(Call(target=target, args=args, retval=ret.result))
    return ret

@emitter
def emit_ExprList(node, ea):
    ret = IList(result=tmpreg())
    for b in node.body:
        val = emit(b)
        ret.extend(val)
    symtab.ident.enter(ret.result, copyof(val.result))
    ret.append(Move(target=ret.result, src0=val.result))
    return ret

@emitter
def emit_StatementList(node, ea):
    ret = IList()
    for b in node.body:
        ret.extend(emit(b))
    return ret

@emitter
def emit_While(node, ea):
    ret = IList()
    begin = anon('lbl')
    end = anon('lbl')

    ret.append(Label(name=begin))
    expr = emit(node.expr)
    ret.extend(expr)
    ret.append(IfFalse(src0=expr.result, label=end))
    cont_stack.push(begin)
    brk_stack.push(end)
    ret.extend(emit(node.stmt))
    cont_stack.pop()
    brk_stack.pop()
    ret.append(Jump(label=begin))
    ret.append(Label(name=end))
    return ret

@emitter
def emit_DoWhile(node, ea):
    ret = []
    begin = anon('lbl')
    end = anon('lbl')
    cont_stack.push(begin)
    brk_stack.push(end)

    ret.append(Label(name=begin))
    ret.extend(emit(node.stmt))
    cont_stack.pop()
    brk_stack.pop()
    expr = emit(node.expr)
    ret.extend(expr)
    ret.append(IfTrue(src0=expr.result, label=begin))
    ret.append(Label(name=end))
    return ret

@emitter
def emit_For(node, ea):
    ret = []
    test = anon('lbl')
    mutate = anon('lbl')
    exit = anon('lbl')

    ret.extend(emit(node.initialize))
    ret.append(Label(name=test))
    if node.expr:
        expr = emit(node.expr)
        ret.extend(expr)
        ret.append(IfFalse(src0=expr.result, label=exit))

    cont_stack.push(mutate)
    brk_stack.push(exit)
    ret.extend(emit(node.stmt))
    cont_stack.pop()
    brk_stack.pop()

    ret.extend(emit(node.ctrl))
    ret.append(Jump(label=test))
    ret.append(Label(name=exit))
    return ret

@emitter
def emit_If(node, ea):
    ret = IList()
    orelse = anon('lbl')

    expr = emit(node.expr)
    ret.extend(expr)
    ret.append(IfFalse(src0=expr.result, label=orelse))
    ret.extend(emit(node.stmt))
    if node.orelse:
        endif = anon('lbl')
        ret.append(
            Jump(label=endif),
            Label(name=orelse),
            emit(node.orelse),
            Label(name=endif))
    else:
        ret.append(Label(name=orelse))
    return ret

@emitter
def emit_Break(node, ea):
    ret = IList()
    return ret.append(Jump(label=brk_stack.top()))


@emitter
def emit_Continue(node, ea):
    ret = IList()
    return ret.append(Jump(label=cont_stack.top()))

@emitter
def emit_Label(node, ea):
    ret = IList()
    return ret.append(Label(name=node.name))

@emitter
def emit_Goto(node, ea):
    ret = IList()
    if isinstance(node, C.Identifier):
        ret.append(Jump(label=node.name))
    else:
        code = emit(node.expr)
        ret.extend(code)
        ret.append(Jump(target=code.result))
    return ret

@emitter
def emit_Switch(node, ea):
    return IList()

@emitter
def emit_Case(node, ea):
    return IList()

@emitter
def emit_Default(node, ea):
    return IList()

@emitter
def emit_Return(node, ea):
    ret = IList()
    result = None
    if node.expr:
        expr = emit(node.expr)
        result = expr.result
        ret.extend(expr)
    ret.append(
            Return(src0=result),
            Jump(label=ret_stack.top()))
    return ret

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
def emit_Declarator(node, ea):
    ret = IList()
    if node.name:
        symtab.ident.enter(node.name, node)
    for t in node.type:
        if isinstance(t, C.AST):
            emit(t)
    if typeinfo.isextern(node):
        ret.append(Extern(name=node.name))
    return ret

@emitter
def emit_Attribute(node, ea):
    return IList()

@emitter
def emit_Typedef(node, ea):
    symtab.typedef.enter(node.name, node)
    return IList()

@emitter
def emit_Initializer(node, ea):
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
def emit_Enumerator(node, ea):
    name = node.name
    enum = symtab.enum.top()
    if node.value:
        value = emit(node.value)
    else:
        for k,v in enum.items():
            value = v
        value = value+1
    enum.enter(name, value)
    return IList()

@emitter
def emit_Enumeration(node, ea):
    if not node.name:
        node.name = anon('enum')
    symtab.enum.enter(node.name, None)
    if node.body:
        symtab.ident.push(symtab.Symtab(node.name, 'enum'))
        for n in node.body:
            emit(n)
        symtab.enum.enter(node.name, symtab.ident.pop())
    return IList()

@emitter
def emit_Struct(node, ea):
    if not node.name:
        node.name = anon('struct')
    symtab.struct.enter(node.name, None)
    if node.body:
        symtab.ident.push(symtab.Symtab(node.name, 'struct'))
        for n in node.body:
            emit(n)
        symtab.struct.enter(node.name, symtab.ident.pop())
    return IList()

@emitter
def emit_Union(node, ea):
    if not node.name:
        node.name = anon('union')
    symtab.union.enter(node.name, None)
    if node.body:
        symtab.ident.push(symtab.Symtab(node.name, 'union'))
        for n in node.body:
            emit(n)
        symtab.union.enter(node.name, symtab.ident.pop())
    return IList()

@emitter
def emit_Function(node, ea):
    ret = IList()
    tmpreg_reset()
    sig = node.signature
    symtab.ident.enter(sig.name, sig)
    if node.body:
        sig.impl = True
        fnexit = anon('lbl_%s_exit' % sig.name)
        ret_stack.push(fnexit)
        ret.sympush(symtab.Symtab('locals', 'stack'))

        # Add the parameters to the local symbol table, and set their
        # offsets as negative values (meaning allocated before function
        # entry)
        align = typeinfo.stackalign(None)
        # Allocate 1 word for the return value
        pos = 1
        for a in sig.args:
            pos = (pos+align-1) & ~(align-1)
            a.offset = -pos
            symtab.ident.enter(a.name, a)
            pos += typeinfo.sizeof(a)

        ret.append(Enter(name=sig.name))
        ret.extend(emit(node.body))
        # if the last thing is "return", we dont need the jump
        if isinstance(ret[-1], Jump) and ret[-1].label == fnexit:
            ret.pop(-1)
        ret.append(
                Label(name=fnexit),
                Leave())
        ret.sympop()
        ret_stack.pop()

    return ret

@emitter
def emit_TranslationUnit(node, ea):
    ret = IList()
    ret.sympush(globaltab)
    for b in node.body:
        ret.extend(emit(b))
    ret.sympop()
    return ret
