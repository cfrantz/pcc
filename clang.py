#!/usr/bin/env python
from bubbles.xsd.schema import SchemaLoader, Builder, SchemaObject

def xsdimport(url):
    SchemaObject.__attrchar__ = ''
    ns = SchemaLoader.load(url)
    b = Builder()
    types = SchemaLoader.schemas[ns]['types']
    glb = globals()
    for k in types.keys():
        glb[k] = b.factory(k)

xsdimport('file:c_ast.xsd')

def postfix_expr(lhs, rest):
    for expr in rest:
        if isinstance(expr, UnaryOp):
            expr.expr = lhs
            lhs = expr
        elif isinstance(expr, BinaryOp):
            expr.lhs = lhs
            lhs = expr
        elif isinstance(expr, Call):
            expr.function = lhs
            lhs = expr
        else:
            raise Exception("Type Error: expected UnaryOp or BinaryOp", expr)
    return lhs

def cast_expr(casts, expr):
    casts.reverse()
    for c in casts:
        c.rhs = expr
        expr = c
    return expr

UnaryOps = {
        '-': 'negate',
        '~': 'invert',
        '!': 'lognot',
        '&': 'addressof',
        '*': 'dereference',
}
            
def unary_op(op, expr):
    if op != '+':
        expr = UnaryOp(op=UnaryOps[op], expr=expr)
    return expr

BinOps = {
    '*': 'mul', '/': 'div', '%': 'mod',
    '+': 'add', '-': 'sub',
    '<<': 'shl', '>>': 'shr',
    '&': 'and', '|': 'or', '^': 'xor',
    '&&': 'logand', '||': 'logor',
    '==': 'eq', '!=': 'ne', '<': 'lt', '<=': 'le', '>': 'gt', '>=': 'ge',
}

def binexpr(lhs, rest):
    for (op, rhs) in rest:
        lhs = BinaryOp(op=BinOps[op], lhs=lhs, rhs=rhs)
    return lhs

def condexpr(expr, rest):
    for a, b in rest:
        expr = Conditional(expr=expr, condtrue=a, condfalse=b)
    return expr

def assnexpr(lhs, op, rhs):
    if op != '=':
        aop = op[:-1]
        rhs = binexpr(lhs, [(aop, rhs)])
    return BinaryOp(op='assign', lhs=lhs, rhs=rhs)

def exprlist(expr, rest):
    if rest:
        expr = ExpressionList(expr=[expr]+rest)
    return expr
    

def initdecl(decl, initializer):
    decl.initializer = initializer
    return decl

def declmods(decl, modifiers):
    if not decl:
        decl = Declarator()
    for m in modifiers:
        if isinstance(m, Array):
            decl.type.append('array')
            decl.asize.append(m.size)
        elif isinstance(m, FuncSig):
            decl.type.append('function')
            decl.signature.append(m)
        elif isinstance(m, basestring):
            decl.type.append(m)
    return decl

def declsmods(decls, modifiers):
    for d in decls:
        if isinstance(d, Declarator):
            declmods(d, modifiers)
    return decls


def declaration(spec, decls):
    if not decls or decls == ['']:
        decls = Declarator()

    if not isinstance(decls, list):
        decls = [decls]

    comp = []
    for i, s in enumerate(spec):
        if isinstance(s, CompositeType):
            comp.append(s)
            spec[i] = '%s:%s' % (s.type, s.name)

    for d in decls:
        d.type.extend(spec)
    return comp+decls

def externals(decls):
    ret = []
    for d in decls:
        if isinstance(d, CompositeType):
            ret.append(ExternalDeclaration(composite=d))
        elif isinstance(d, Declarator):
            if d.name:
                ret.append(ExternalDeclaration(declaration=d))
        else:
            raise Exception('Type Error.  Expecting CompositeType or Declarator')

    return ret


def concat(lists):
    ret = []
    for x in lists:
        if isinstance(x, list):
            ret.extend(x)
        else:
            ret.append(x)
    return ret

# vim: ts=4 sts=4 sw=4 expandtab:
