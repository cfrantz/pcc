#!/usr/bin/env python

from macropy.peg import macros, peg
import cast as C

with peg:
    TranslationUnit = (Spacing, ExternalDeclaration.rep1 is e) >> C.TranslationUnit(body=C.concat(e))

    ExternalDeclaration = FunctionDefinition | Declaration

    FunctionDefinition = (
        DeclarationSpecifiers is r, Declarator is sig, DeclarationList.opt, CompoundStatement is body
    ) >> C.Function.init(sig, r, body)

    DeclarationList = Declaration.rep1

    # Declarators
    Declaration = (
            DeclarationSpecifiers is spec, InitDeclaratorList.opt is decls, SEMI
        ) >> C.Declarator.decl(decls, spec)
        
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

    InitDeclarator = (Declarator is d, ((EQU, Initializer is i) >> i).opt is i) >> d.set_initializer(i)

    StorageClassSpecifier = (
        TYPEDEF
        | EXTERN
        | STATIC
        | AUTO
        | REGISTER
        | (ATTRIBUTE, LPAR, LPAR, '[^)]*'.r is a, RPAR, RPAR) >> C.Attribute(attr=a)
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
          (StructOrUnion is t, Identifier.opt is name, LWING, StructDeclaration.rep1 is decl, RWING) >> C.CompositeType.init(t, name, C.concat(decl))
          | (StructOrUnion is t, Identifier is name) >> C.CompositeType.init(t, name, [])
    )

    StructOrUnion = STRUCT | UNION

    StructDeclaration = (SpecifierQualifierList is q, StructDeclaratorList is d, SEMI) >> C.Declarator.mods(d, q)

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
            (Identifier.opt is i, LWING, EnumeratorList is e, COMMA.opt, RWING) >> C.Enumeration(name=i.name, values=e)
            | (Identifier is i) >> C.Enumeration(name=i.name)
        ) 
    ) // (lambda x: x[1])

    EnumeratorList = (Enumerator is first, ((COMMA, Enumerator is e)>>e).rep is rest) >> [first]+rest

    Enumerator = (EnumerationConstant is name, ((EQU, ConstantExpression is c)>>c).opt is value) >> C.Enumerator(name=name, value=value)

    TypeQualifier = (
        CONST
        | RESTRICT
        | VOLATILE
        | (DECLSPEC, LPAR, Identifier is i, RPAR) >> C.DeclSpec(spec=i)
        )

    FunctionSpecifier = INLINE

    Declarator = (Pointer.opt is p, DirectDeclarator is d) >> C.Declarator.mods(d, p)

    DirectDeclarator = (
        ( Identifier >> C.Declarator(name=Identifier.name)
        | (LPAR, Declarator is d, RPAR) >> d
        ) is d,
        ( (LBRK, TypeQualifier.rep, AssignmentExpression.opt is sz, RBRK) >>  C.Array(expr=sz)
        | (LBRK, STATIC, TypeQualifier.rep, AssignmentExpression is sz, RBRK) >> C.Array(expr=sz)
        | (LBRK, TypeQualifier.rep1, STATIC, AssignmentExpression is sz, RBRK) >> C.Array(expr=sz)
        | (LBRK, TypeQualifier.rep, STAR, RBRK) >> C.Array()
        | (LPAR, ParameterTypeList is a, RPAR) >> C.Arguments(args=a)
        | (LPAR, IdentifierList.opt is a, RPAR) >> C.Arguments(args=(a or []))
        ).rep is modifiers
    ) >> C.Declarator.mods(d, modifiers)

    Pointer = (((STAR, TypeQualifier.rep is q) >> ['pointer']+q).rep1 is p) >> reversed([v for i in p for v in i])

    ParameterTypeList = (ParameterList is p, ((COMMA, ELLIPSIS)>>[C.Declarator.mods(None, ['...'])]).opt is a) >> p + (a or [])

    ParameterList = (
        ParameterDeclaration is first, ((COMMA, ParameterDeclaration is d) >> d[0]).rep is rest
    ) >> first+rest

    ParameterDeclaration = (
        DeclarationSpecifiers is spec, ( Declarator | AbstractDeclarator).opt is decl
    ) >> C.Declarator.decl(decl, spec)

    IdentifierList = (Identifier is first, ((COMMA, Identifier is i)>>i).rep is rest) >> [first]+rest

    TypeName = (SpecifierQualifierList, AbstractDeclarator.opt)

    AbstractDeclarator = (
        (Pointer.opt is p, DirectAbstractDeclarator is d) >> C.Declarator.mods(d, p)
        | (Pointer is p) >> C.Declarator.mods(None, p)
        )

    DirectAbstractDeclarator = (
        ( (LPAR, AbstractDeclarator is d, RPAR)  >> d
        | (LBRK, AssignmentExpression is sz, RBRK) >> C.Declarator.mods(None, [C.Array(expr=sz)])
        | (LBRK, STAR.opt, RBRK) >> C.Declarator.mods(None, [C.Array(expr=0)])
        | (LPAR, ParameterTypeList.opt is a, RPAR) >> C.Declarator.mods(None, [C.Arguments(args=a)])
        ) is d,
        ( (LBRK, ((AssignmentExpression is sz) >> C.Array(expr=sz) | STAR >> C.Array(expr=0)).opt is array, RBRK) >> array
        | (LPAR, ParameterTypeList.opt is a, RPAR) >> C.Arguments(args=a)
        ).rep is modifiers
    ) >> C.Declarator.mods(d, modifiers)

    TypedefName = Identifier

    Initializer = (
        (AssignmentExpression is a) >> C.Initializer(initializer=a)
        | (LWING, InitializerList is i, COMMA.opt, RWING) >> i
        )

    InitializerList = (
        ((Designation.opt is d, Initializer is i) >>
            i.set_designator(d)) is first,
        ((COMMA, Designation.opt is d, Initializer is i) >>
            i.set_designator(d)).rep is rest
        ) >> [first]+rest

    Designation = (Designator.rep1, EQU) >> Designator

    Designator = (
        (LBRK, ConstantExpression is c, RBRK) >> c
        | (DOT, Identifier is i) >> i
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
        (LWING, ( Declaration | Statement ).rep is body, RWING) >> C.StatementList(body=C.concat(body))
    )

    ExpressionStatement = (Expression.opt, SEMI) >> Expression

    SelectionStatement = (
        (IF, LPAR, Expression is expr, RPAR, Statement is then, (ELSE, Statement is orelse).opt) >> C.If(expr=expr, stmt=then, orelse=orelse)
        | (SWITCH, LPAR, Expression is expr, RPAR, Statement is body) >> C.Switch(expr=expr, stmt=body)
        )

    IterationStatement = (
        (WHILE, LPAR, Expression, RPAR, Statement) >> C.While(expr=Expression, stmt=Statement)
        | (DO, Statement, WHILE, LPAR, Expression, RPAR, SEMI) >> C.DoWhile(expr=Expression, stmt=Statement)
        | (FOR, LPAR, Expression.opt is init, SEMI, Expression.opt is expr, SEMI, Expression.opt is ctrl, RPAR, Statement is body) >> C.For(initialize=init, expr=expr, ctrl=ctrl, stmt=body)
        | (FOR, LPAR, Declaration is init, Expression.opt is expr, SEMI, Expression.opt is ctrl, RPAR, Statement is body) >> C.For(initialize=init, expr=expr, ctrl=ctrl, stmt=body)
        )

    JumpStatement = (
        (GOTO, Identifier, SEMI) >> C.Goto(expr=Identifier)
        | (CONTINUE, SEMI) >> C.Continue()
        | (BREAK, SEMI) >> C.Break()
        | (RETURN, Expression.opt is e, SEMI) >> C.Return(expr=(e or None))
        )

    # Expressions
    PrimaryExpression = (
        Identifier
        | Constant 
        | StringLiteral 
        | (LPAR, Expression, RPAR) >> Expression
    )

    PostfixExpression = ( 
        (PrimaryExpression
#          | LPAR,TypeName,RPAR,LWING,InitializerList,COMMA.opt, RWING
          ) is lhs, (
          (LBRK, Expression, RBRK) >> C.Subscript(index=Expression)
          | (LPAR, ArgumentExpressionList.opt is args, RPAR) >> C.Call(args=args)
          | (DOT, Identifier) >> C.Field(op=DOT, field=Identifier.name)
          | (PTR, Identifier) >> C.Field(op=PTR, field=Identifier.name)
          | INC >> C.PostfixOp(op=INC)
          | DEC >> C.PostfixOp(op=DEC)
          ).rep is rest) >> C.PostfixOp.init(lhs, rest)

    ArgumentExpressionList = (AssignmentExpression is first, ((COMMA, AssignmentExpression is r)>>r ).rep is rest) >> [first]+rest

    UnaryExpression = (
            PostfixExpression 
            | (INC, UnaryExpression) >> C.PrefixOp(op=INC, operand=UnaryExpression)
            | (DEC, UnaryExpression) >> C.PrefixOp(op=DEC, operand=UnaryExpression)
            | (UnaryOperator, CastExpression) >> C.UnaryOp.init(UnaryOperator, [CastExpression])
            | (SIZEOF, (UnaryExpression | (LPAR, TypeName, RPAR)))
        )

    UnaryOperator = AND | STAR | PLUS | MINUS | TILDA | BANG

    CastExpression = (((LPAR, TypeName, RPAR) >> Cast(type=Typename)).rep is casts, UnaryExpression is expr) >> C.Cast.init(expr, casts)

    MultiplicativeExpression = (CastExpression is expr, ((STAR | DIV | MOD), CastExpression).rep is rest) >> C.BinOp.init(expr, rest)

    AdditiveExpression = (MultiplicativeExpression is expr, ((PLUS | MINUS), MultiplicativeExpression).rep is rest) >> C.BinOp.init(expr, rest)

    ShiftExpression = (AdditiveExpression is expr, ((LEFT | RIGHT), AdditiveExpression).rep is rest) >> C.BinOp.init(expr, rest)

    RelationalExpression = (ShiftExpression is expr, ((LE | GE | LT | GT), ShiftExpression).rep is rest) >> C.Compare.init(expr, rest)

    EqualityExpression = (RelationalExpression is expr, ((EQUEQU | BANGEQU), RelationalExpression).rep is rest) >> C.Compare.init(expr, rest)

    ANDExpression = (EqualityExpression is expr, (AND, EqualityExpression).rep is rest) >> C.BinOp.init(expr, rest)

    ExclusiveORExpression = (ANDExpression is expr, (HAT, ANDExpression).rep is rest) >> C.BinOp.init(expr, rest)

    InclusiveORExpression = (ExclusiveORExpression is expr, (OR, ExclusiveORExpression).rep is rest) >> C.BinOp.init(expr, rest)

    LogicalANDExpression = (InclusiveORExpression is expr, (ANDAND, InclusiveORExpression).rep is rest) >> C.BoolOp.init(expr, rest)

    LogicalORExpression = (LogicalANDExpression is expr, (OROR, LogicalANDExpression).rep is rest) >> C.BoolOp.init(expr, rest)

    ConditionalExpression = (LogicalORExpression is expr, ((QUERY, Expression is a, COLON, LogicalORExpression is b) >> (a, b)).rep is rest) >> C.ConditionalOp.init(expr, rest)

    AssignmentExpression = (UnaryExpression is lhs, AssignmentOperator is op, AssignmentExpression is rhs) >> C.Assign.init(lhs, op, rhs) | ConditionalExpression

    AssignmentOperator = EQU | STAREQU | DIVEQU | MODEQU | PLUSEQU | MINUSEQU | LEFTEQU | RIGHTEQU | ANDEQU | HATEQU | OREQU

    Expression = (AssignmentExpression is a, ((COMMA, AssignmentExpression is expr) >> expr).rep is b) >> C.ExprList.init(a, b)

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
        
    Identifier = (-Keyword, (IdNondigit, IdChar.rep.join).join is i, Spacing) >> C.Identifier(name=i)
    IdNondigit = '[A-Za-z_]'.r | UniversalChar
    IdChar = '[0-9A-Za-z_]'.r | UniversalChar
    UniversalChar = r'\\u[0-9A-Fa-f]{4}'.r | r'\\U[0-9A-Fa-f]{8}'.r

    Constant = (FloatConstant | IntegerConstant | EnumerationConstant | CharacterConstant)

    IntegerConstant = (((DecimalConstant | HexConstant | OctalConstant) is val), IntegerSuffix.opt is mod, Spacing) >> C.Integer.init(val, mod)
    DecimalConstant = ('[1-9][0-9_]*'.r is val) >> val.replace('_', '')
    OctalConstant = ('0[0-7_]*'.r is val) >> val.replace('_', '')
    HexConstant = ('0[xX][0-9A-Fa-f_]+'.r is val) >> val.replace('_', '')
    IntegerSuffix = (('[uU]'.r, Lsuffix.opt).join | (Lsuffix, 'uU'.r.opt).join) // str.upper
    Lsuffix = ( 'll' | 'LL' | 'l' | 'L')

    FloatConstant = (((DecimalFloatConstant | HexFloatConstant) is val), FloatSuffix.opt is mod, Spacing) >> C.Float.init(val, mod)
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

    StringLiteral = ('L'.opt is mod, (('"', StringChar.rep.join is val, '"', Spacing) >> val).rep1.join is val) >> C.String(value=val, mod=mod)
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


def parse(ccode):
    return TranslationUnit.parse(ccode)

# vim: ts=4 sts=4 sw=4 expandtab:
