#!/usr/bin/env python

from macropy.peg import macros, peg
import clang as C

with peg:
    TranslationUnit = (Spacing, ExternalDeclaration.rep1 is e) >> C.TranslationUnit(node=C.concat(e))

    ExternalDeclaration = (
        (FunctionDefinition is f) >> C.ExternalDeclaration(function=f)
        | (Declaration is d) >> C.externals(d)
        )

    FunctionDefinition = (
        DeclarationSpecifiers is r, Declarator is sig, DeclarationList.opt, CompoundStatement is body
    ) >> C.Function(rtype=C.Declarator(type=r), signature=sig, body=body)

    DeclarationList = Declaration.rep1

    # Declarators
    Declaration = (
            DeclarationSpecifiers is spec, InitDeclaratorList.opt is decls, SEMI
        ) >> C.declaration(spec, decls)
        
    DeclarationSpecifiers = (
        (( StorageClassSpecifier
           | TypeQualifier
           | FunctionSpecifier
           ).rep,
           TypedefName,
           ( StorageClassSpecifier
           | TypeQualifier
           | FunctionSpecifier
           ).rep
          )
        | ( StorageClassSpecifier
          | TypeSpecifier
          | TypeQualifier
          | FunctionSpecifier
          ).rep1
        )

    InitDeclaratorList = (
        InitDeclarator is first, ((COMMA, InitDeclarator is r)>>r).rep is rest) >> [first]+rest

    InitDeclarator = (Declarator is d, ((EQU, Initializer is i) >> i).opt is i) >> C.initdecl(d, (i or None))

    StorageClassSpecifier = (
        TYPEDEF
        | EXTERN
        | STATIC
        | AUTO
        | REGISTER
        | (ATTRIBUTE, LPAR, LPAR, '[^)]*'.r is a, RPAR, RPAR) >> ('attribute:%s' % a)
        )

    TypeSpecifier = (
        VOID
        | CHAR
        | SHORT
        | INT
        | LONG
        | FLOAT
        | DOUBLE
        | SIGNED
        | UNSIGNED
        | BOOL
        | COMPLEX
        | StructOrUnionSpecifier
        | EnumSpecifier
        )

    StructOrUnionSpecifier = (
          (StructOrUnion is t, Identifier.opt is name, LWING, StructDeclaration.rep1 is decl, RWING) >> C.CompositeType(type=t, name=name, body=C.concat(decl))
          | (StructOrUnion is t, Identifier is name) >> C.CompositeType(type=t, name=name)
    )

    StructOrUnion = STRUCT | UNION

    StructDeclaration = (SpecifierQualifierList is q, StructDeclaratorList is d, SEMI) >> C.declsmods(d, q)

    SpecifierQualifierList = (
        (TypeQualifier.rep, TypedefName, TypeQualifier.rep) // (lambda x: x[0]+['typedef:%s'%x[1]]+x[2])
        | ( TypeSpecifier | TypeQualifier).rep1
    )

    StructDeclaratorList = (StructDeclarator is first, ((COMMA, StructDeclarator is d)>>d).rep is rest) >> [first]+rest

    StructDeclarator = (
        (Declarator.opt is d, COLON, ConstantExpression is sz) >> C.BitField(name=d, size=sz)
        | Declarator
    )

    EnumSpecifier = (
        ENUM, (
            (Identifier.opt is i, LWING, EnumeratorList is e, COMMA.opt, RWING) >> C.Enumeration(name=i, values=e)
            | (Identifier is i) >> ('e(%s)' % i)
        ) 
    ) // (lambda x: x[1])

    EnumeratorList = (Enumerator is first, ((COMMA, Enumerator is e)>>e).rep is rest) >> [first]+rest

    Enumerator = (EnumerationConstant is name, ((EQU, ConstantExpression is c)>>c).opt is value) >> C.Enumerator(name=name, value=value)

    TypeQualifier = (
        CONST
        | RESTRICT
        | VOLATILE
        | (DECLSPEC, LPAR, Identifier is i, RPAR) >> ('declspec:%s' % i)
        )

    FunctionSpecifier = INLINE

    Declarator = (Pointer.opt is p, DirectDeclarator is d) >> C.declmods(d, p)

    DirectDeclarator = (
        ( Identifier >> C.Declarator(name=Identifier)
        | (LPAR, Declarator is d, RPAR) >> d
        ) is d,
        ( (LBRK, TypeQualifier.rep, AssignmentExpression.opt is sz, RBRK) >>  C.Array(size=sz)
        | (LBRK, STATIC, TypeQualifier.rep, AssignmentExpression is sz, RBRK) >> C.Array(size=sz)
        | (LBRK, TypeQualifier.rep1, STATIC, AssignmentExpression is sz, RBRK) >> C.Array(size=sz)
        | (LBRK, TypeQualifier.rep, STAR, RBRK) >> C.Array()
        | (LPAR, ParameterTypeList is p, RPAR) >> C.FuncSig(sig=p)
        | (LPAR, IdentifierList.opt is p, RPAR) >> C.FuncSig(sig=(p or []))
        ).rep is modifiers
    ) >> C.declmods(d, modifiers)

    Pointer = (((STAR, TypeQualifier.rep is q) >> ['pointer']+q).rep1 is p) >> reversed([v for i in p for v in i])

    ParameterTypeList = (ParameterList is p, ((COMMA, ELLIPSIS)>>[C.declmods(None, ['va_list'])]).opt is a) >> p + (a or [])

    ParameterList = (
        ParameterDeclaration is first, ((COMMA, ParameterDeclaration is d) >> d[0]).rep is rest
    ) >> first+rest

    ParameterDeclaration = (
        DeclarationSpecifiers is spec, ( Declarator | AbstractDeclarator).opt is decl
    ) >> C.declaration(spec, decl)

    IdentifierList = (Identifier is first, ((COMMA, Identifier is i)>>i).rep is rest) >> [first]+rest

    TypeName = (SpecifierQualifierList, AbstractDeclarator.opt)

    AbstractDeclarator = (
        (Pointer.opt is p, DirectAbstractDeclarator is d) >> C.declmods(d, p)
        | (Pointer is p) >> C.declmods(None, p)
        )

    DirectAbstractDeclarator = (
        ( (LPAR, AbstractDeclarator is d, RPAR)  >> d
        | (LBRK, AssignmentExpression is sz, RBRK) >> C.declmods(None, [C.Array(size=sz)])
        | (LBRK, STAR.opt, RBRK) >> C.declmods(None, [C.Array(size=0)])
        | (LPAR, ParameterTypeList.opt is sig, RPAR) >> C.declmods(None, [C.FuncSig(sig=sig)])
        ) is d,
        ( (LBRK, ((AssignmentExpression is sz) >> C.Array(size=sz) | STAR >> C.Array(size=0)).opt is array, RBRK) >> array
        | (LPAR, ParameterTypeList.opt is sig, RPAR) >> C.FuncSig(sig=sig)
        ).rep is modifiers
    ) >> C.declmods(d, modifiers)

    TypedefName = Identifier

    Initializer = (
        AssignmentExpression
        | (LWING, InitializerList, COMMA.opt, RWING)
        )

    InitializerList = (
        Designation.opt, Initializer, (COMMA, Designation.opt, Initializer).rep
        )

    Designation = (Designator.rep1, EQU)

    Designator = (
        (LBRK, ConstantExpression, RBRK)
        | (DOT, Identifier)
        )

    # Statements
    Statement = (
        LabeledStatement
        | CompoundStatement
        | ExpressionStatement
        | SelectionStatement
        | IterationStatement
        | JumpStatement
        )

    # CJF: I don't think this is correct
    #LabeledStatement = (
    #    (Identifier, COLON, Statement) >> [C.Label(label=Identifier), Statement]
    #    | (CASE, ConstantExpression is val, COLON, Statement) >> [C.Case(expr=val), Statement]
    #    | (DEFAULT, COLON, Statement) >> [C.Default(), Statement]
    #    )

    LabeledStatement = (
        (Identifier, COLON) >> C.Label(label=Identifier)
        | (CASE, ConstantExpression is val, COLON) >> C.Case(expr=val)
        | (DEFAULT, COLON) >> C.Default()
        )

    CompoundStatement = (
        (LWING, ( Declaration | Statement ).rep is stmt, RWING) >> C.StatementList(stmt=C.concat(stmt))
    )

    ExpressionStatement = (Expression.opt, SEMI) >> Expression

    SelectionStatement = (
        (IF, LPAR, Expression is expr, RPAR, Statement is then, (ELSE, Statement is else_).opt) >> C.If(expr=expr, condtrue=then, condfalse=(else_ or None))
        | (SWITCH, LPAR, Expression is expr, RPAR, Statement is body) >> C.Switch(expr=expr, body=body)
        )

    IterationStatement = (
        (WHILE, LPAR, Expression, RPAR, Statement) >> C.While(expr=Expression, body=Statement)
        | (DO, Statement, WHILE, LPAR, Expression, RPAR, SEMI) >> C.DoWhile(expr=Expression, body=Statement)
        | (FOR, LPAR, Expression.opt is init, SEMI, Expression.opt is cond, SEMI, Expression.opt is cont, RPAR, Statement is body) >> C.For(init=init, condition=cond, control=cont, body=body)
#        | (FOR, LPAR, Declaration is init, Expression.opt is test, SEMI, Expression.opt is modify, RPAR, Statement is body) >> C.For(init=init, test=test, modify=modify, body=body)
        )

    JumpStatement = (
        (GOTO, Identifier, SEMI) >> C.Goto(target=Identifier)
        | (CONTINUE, SEMI) >> C.Continue()
        | (BREAK, SEMI) >> C.Break()
        | (RETURN, Expression.opt is e, SEMI) >> C.Return(expr=(e or None))
        )

    # Expressions
    PrimaryExpression = (
        (Identifier is i) >> C.Identifier(name=i)
        | Constant 
        | StringLiteral 
        | (LPAR, Expression, RPAR) >> Expression
    )

    PostfixExpression = ( 
        (PrimaryExpression
#          | LPAR,TypeName,RPAR,LWING,InitializerList,COMMA.opt, RWING
          ) is lhs, (
          (LBRK, Expression, RBRK) >> C.BinaryOp(op='index', rhs=Expression)
          | (LPAR, ArgumentExpressionList.opt is args, RPAR) >> C.Call(arguments=args)
          | (DOT, Identifier) >> C.BinaryOp(op='field', rhs=Identifier)
          | (PTR, Identifier) >> C.BinaryOp(op='ptrfield', rhs=Identifier)
          | INC >> C.UnaryOp(op='postinc')
          | DEC >> C.UnaryOp(op='postdec')
          ).rep is rest) >> C.postfix_expr(lhs, rest)

    ArgumentExpressionList = (AssignmentExpression is first, ((COMMA, AssignmentExpression is r)>>r ).rep is rest) >> [first]+rest

    UnaryExpression = (
            PostfixExpression 
            | (INC, UnaryExpression) >> C.PreInc(expr=UnaryExpression)
            | (DEC, UnaryExpression) >> C.PreDec(expr=UnaryExpression)
            | (UnaryOperator, CastExpression) >> C.unary_op(UnaryOperator, CastExpression)
            | (SIZEOF, (UnaryExpression | (LPAR, TypeName, RPAR)))
        )

    UnaryOperator = AND | STAR | PLUS | MINUS | TILDA | BANG

    CastExpression = (((LPAR, TypeName, RPAR) >> Cast(type=Typename)).rep is casts, UnaryExpression is expr) >> C.cast_expr(casts, expr)

    MultiplicativeExpression = (CastExpression is expr, ((STAR | DIV | MOD), CastExpression).rep is rest) >> C.binexpr(expr, rest)

    AdditiveExpression = (MultiplicativeExpression is expr, ((PLUS | MINUS), MultiplicativeExpression).rep is rest) >> C.binexpr(expr, rest)

    ShiftExpression = (AdditiveExpression is expr, ((LEFT | RIGHT), AdditiveExpression).rep is rest) >> C.binexpr(expr, rest)

    RelationalExpression = (ShiftExpression is expr, ((LE | GE | LT | GT), ShiftExpression).rep is rest) >> C.binexpr(expr, rest)

    EqualityExpression = (RelationalExpression is expr, ((EQUEQU | BANGEQU), RelationalExpression).rep is rest) >> C.binexpr(expr, rest)

    ANDExpression = (EqualityExpression is expr, (AND, EqualityExpression).rep is rest) >> C.binexpr(expr, rest)

    ExclusiveORExpression = (ANDExpression is expr, (HAT, ANDExpression).rep is rest) >> C.binexpr(expr, rest)

    InclusiveORExpression = (ExclusiveORExpression is expr, (OR, ExclusiveORExpression).rep is rest) >> C.binexpr(expr, rest)

    LogicalANDExpression = (InclusiveORExpression is expr, (ANDAND, InclusiveORExpression).rep is rest) >> C.binexpr(expr, rest)

    LogicalORExpression = (LogicalANDExpression is expr, (OROR, LogicalANDExpression).rep is rest) >> C.binexpr(expr, rest)

    ConditionalExpression = (LogicalORExpression is expr, ((QUERY, Expression is a, COLON, LogicalORExpression is b) >> (a, b)).rep is rest) >> C.condexpr(expr, rest)

    AssignmentExpression = (UnaryExpression is lhs, AssignmentOperator is op, AssignmentExpression is rhs) >> C.assnexpr(lhs, op, rhs) | ConditionalExpression

    AssignmentOperator = EQU | STAREQU | DIVEQU | MODEQU | PLUSEQU | MINUSEQU | LEFTEQU | RIGHTEQU | ANDEQU | HATEQU | OREQU

    Expression = (AssignmentExpression is a, ((COMMA, AssignmentExpression is expr) >> expr).rep is b) >> C.exprlist(a, b)

    ConstantExpression = ConditionalExpression

    # Whitespace
    Spacing = (WhiteSpace | LongComment | LineComment).rep
    WhiteSpace = u'[ \n\r\t\u000b\u000c]'.r
    LongComment = '/*', (r'[^*]|\*[^/]'.r ).rep , '*/'
    LineComment = '//', '[^\n]'.r

    # Keywords
    AUTO      = ("auto",       -IdChar, Spacing) // (lambda x: x[0])
    BREAK     = ("break",      -IdChar, Spacing) // (lambda x: x[0])
    CASE      = ("case",       -IdChar, Spacing) // (lambda x: x[0])
    CHAR      = ("char",       -IdChar, Spacing) // (lambda x: x[0])
    CONST     = ("const",      -IdChar, Spacing) // (lambda x: x[0])
    CONTINUE  = ("continue",   -IdChar, Spacing) // (lambda x: x[0])
    DEFAULT   = ("default",    -IdChar, Spacing) // (lambda x: x[0])
    DOUBLE    = ("double",     -IdChar, Spacing) // (lambda x: x[0])
    DO        = ("do",         -IdChar, Spacing) // (lambda x: x[0])
    ELSE      = ("else",       -IdChar, Spacing) // (lambda x: x[0])
    ENUM      = ("enum",       -IdChar, Spacing) // (lambda x: x[0])
    EXTERN    = ("extern",     -IdChar, Spacing) // (lambda x: x[0])
    FLOAT     = ("float",      -IdChar, Spacing) // (lambda x: x[0])
    FOR       = ("for",        -IdChar, Spacing) // (lambda x: x[0])
    GOTO      = ("goto",       -IdChar, Spacing) // (lambda x: x[0])
    IF        = ("if",         -IdChar, Spacing) // (lambda x: x[0])
    INT       = ("int",        -IdChar, Spacing) // (lambda x: x[0])
    INLINE    = ("inline",     -IdChar, Spacing) // (lambda x: x[0])
    LONG      = ("long",       -IdChar, Spacing) // (lambda x: x[0])
    REGISTER  = ("register",   -IdChar, Spacing) // (lambda x: x[0])
    RESTRICT  = ("restrict",   -IdChar, Spacing) // (lambda x: x[0])
    RETURN    = ("return",     -IdChar, Spacing) // (lambda x: x[0])
    SHORT     = ("short",      -IdChar, Spacing) // (lambda x: x[0])
    SIGNED    = ("signed",     -IdChar, Spacing) // (lambda x: x[0])
    SIZEOF    = ("sizeof",     -IdChar, Spacing) // (lambda x: x[0])
    STATIC    = ("static",     -IdChar, Spacing) // (lambda x: x[0])
    STRUCT    = ("struct",     -IdChar, Spacing) // (lambda x: x[0])
    SWITCH    = ("switch",     -IdChar, Spacing) // (lambda x: x[0])
    TYPEDEF   = ("typedef",    -IdChar, Spacing) // (lambda x: x[0])
    UNION     = ("union",      -IdChar, Spacing) // (lambda x: x[0])
    UNSIGNED  = ("unsigned",   -IdChar, Spacing) // (lambda x: x[0])
    VOID      = ("void",       -IdChar, Spacing) // (lambda x: x[0])
    VOLATILE  = ("volatile",   -IdChar, Spacing) // (lambda x: x[0])
    WHILE     = ("while",      -IdChar, Spacing) // (lambda x: x[0])
    BOOL      = ("_Bool",      -IdChar, Spacing) // (lambda x: x[0])
    COMPLEX   = ("_Complex",   -IdChar, Spacing) // (lambda x: x[0])
    STDCALL   = ("_stdcall",   -IdChar, Spacing) // (lambda x: x[0])
    DECLSPEC  = ("__declspec", -IdChar, Spacing) // (lambda x: x[0])
    ATTRIBUTE = ("__attribute__",-IdChar, Spacing) // (lambda x: x[0])

    Keyword = ( "auto"
          | "break"
          | "case"
          | "char"
          | "const"
          | "continue"
          | "default"
          | "double"
          | "do"
          | "else"
          | "enum"
          | "extern"
          | "float"
          | "for"
          | "goto"
          | "if"
          | "int"
          | "inline"
          | "long"
          | "register"
          | "restrict"
          | "return"
          | "short"
          | "signed"
          | "sizeof"
          | "static"
          | "struct"
          | "switch"
          | "typedef"
          | "union"
          | "unsigned"
          | "void"
          | "volatile"
          | "while"
          | "_Bool"
          | "_Complex"
          | "_Imaginary"
          | "_stdcall"
          | "__declspec"
          | "__attribute__"), -IdChar 
        
    Identifier = (-Keyword, (IdNondigit, IdChar.rep.join).join is i, Spacing) >> i
    IdNondigit = '[A-Za-z_]'.r | UniversalChar
    IdChar = '[0-9A-Za-z_]'.r | UniversalChar
    UniversalChar = r'\\u[0-9A-Fa-f]{4}'.r | r'\\U[0-9A-Fa-f]{8}'.r

    Constant = (FloatConstant | IntegerConstant | EnumerationConstant | CharacterConstant)

    IntegerConstant = (((DecimalConstant | HexConstant | OctalConstant) is val), IntegerSuffix.opt, Spacing) >> C.Integer(value=int(val, 0))
    DecimalConstant = ('[1-9][0-9_]*'.r is val) >> val.replace('_', '')
    OctalConstant = ('0[0-7_]*'.r is val) >> val.replace('_', '')
    HexConstant = ('0[xX][0-9A-Fa-f_]+'.r is val) >> val.replace('_', '')
    IntegerSuffix = (('[uU]'.r, Lsuffix.opt).join | (Lsuffix, 'uU'.r.opt).join) // str.upper
    Lsuffix = ( 'll' | 'LL' | 'l' | 'L')

    FloatConstant = (((DecimalFloatConstant | HexFloatConstant) is val), FloatSuffix.opt, Spacing) >> C.Double(value=float(val))
    DecimalFloatConstant = (Fraction, Exponent.opt).join | ('[0-9]+'.r, Exponent).join
    Fraction = r'[0-9]*\.[0-9]+'.r | r'[0-9]+\.'.r
    HexFloatConstant = r'0[xX](([0-9A-Fa-f]?\.[0-9A-Fa-f]+)|([0-9A-Fa-f]\.)([pP][+-]?[0-9]+)?)|([0-9A-Fa-f]+[pP][+-]?[0-9]+)'.r 
    Exponent = '[eE][+-]?[0-9]+'.r
    FloatSuffix = '[flFL]'.r // str.upper

    EnumerationConstant = Identifier
    CharacterConstant = ('L'.opt, "'", Char.rep.join is val, "'") >> C.Char(value=val)
    Char = Escape | "[^'\n\\\\]".r
    Escape  = "\\\\['\"?\\abfnrtv]".r
    OctalEscape = r'\[0-7][0-7]?[0-7]?'.r
    HexEscape = r'\x[0-9A-Fa-f]+'

    StringLiteral = ('L'.opt, (('"', StringChar.rep.join is val, '"', Spacing) >> val).rep1.join is strval) >> C.String(value=strval)
    StringChar = Escape | '[^"\n\\\\]'.r

    LBRK       =  ("[",        Spacing ) // (lambda x: x[0])
    RBRK       =  ("]",        Spacing ) // (lambda x: x[0])
    LPAR       =  ("(",        Spacing ) // (lambda x: x[0])
    RPAR       =  (")",        Spacing ) // (lambda x: x[0])
    LWING      =  ("{",        Spacing ) // (lambda x: x[0])
    RWING      =  ("}",        Spacing ) // (lambda x: x[0])
    DOT        =  (".",        Spacing ) // (lambda x: x[0])
    PTR        =  ("->",       Spacing ) // (lambda x: x[0])
    INC        =  ("++",       Spacing ) // (lambda x: x[0])
    DEC        =  ("--",       Spacing ) // (lambda x: x[0])
    AND        =  ("&", -"&",  Spacing ) // (lambda x: x[0])
    STAR       =  ("*",        Spacing ) // (lambda x: x[0])
    PLUS       =  ("+",        Spacing ) // (lambda x: x[0])
    MINUS      =  ("-",        Spacing ) // (lambda x: x[0])
    TILDA      =  ("~",        Spacing ) // (lambda x: x[0])
    BANG       =  ("!",        Spacing ) // (lambda x: x[0])
    DIV        =  ("/",        Spacing ) // (lambda x: x[0])
    MOD        =  ("%",        Spacing ) // (lambda x: x[0])
    LEFT       =  ("<<",       Spacing ) // (lambda x: x[0])
    RIGHT      =  (">>",       Spacing ) // (lambda x: x[0])
    LT         =  ("<",        Spacing ) // (lambda x: x[0])
    GT         =  (">",        Spacing ) // (lambda x: x[0])
    LE         =  ("<=",       Spacing ) // (lambda x: x[0])
    GE         =  (">=",       Spacing ) // (lambda x: x[0])
    EQUEQU     =  ("==",       Spacing ) // (lambda x: x[0])
    BANGEQU    =  ("!=",       Spacing ) // (lambda x: x[0])
    HAT        =  ("^",        Spacing ) // (lambda x: x[0])
    OR         =  ("|",        Spacing ) // (lambda x: x[0])
    ANDAND     =  ("&&",       Spacing ) // (lambda x: x[0])
    OROR       =  ("||",       Spacing ) // (lambda x: x[0])
    QUERY      =  ("?",        Spacing ) // (lambda x: x[0])
    COLON      =  (":",        Spacing ) // (lambda x: x[0])
    SEMI       =  (";",        Spacing ) // (lambda x: x[0])
    ELLIPSIS   =  ("...",      Spacing ) // (lambda x: x[0])
    EQU        =  ("=",        Spacing ) // (lambda x: x[0])
    STAREQU    =  ("*=",       Spacing ) // (lambda x: x[0])
    DIVEQU     =  ("/=",       Spacing ) // (lambda x: x[0])
    MODEQU     =  ("%=",       Spacing ) // (lambda x: x[0])
    PLUSEQU    =  ("+=",       Spacing ) // (lambda x: x[0])
    MINUSEQU   =  ("-=",       Spacing ) // (lambda x: x[0])
    LEFTEQU    =  ("<<=",      Spacing ) // (lambda x: x[0])
    RIGHTEQU   =  (">>=",      Spacing ) // (lambda x: x[0])
    ANDEQU     =  ("&=",       Spacing ) // (lambda x: x[0])
    HATEQU     =  ("^=",       Spacing ) // (lambda x: x[0])
    OREQU      =  ("|=",       Spacing ) // (lambda x: x[0])
    COMMA      =  (",",        Spacing ) // (lambda x: x[0])




# vim: ts=4 sts=4 sw=4 expandtab:
