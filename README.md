# Python C Compiler

## Introduction

This is a simple C compiler experiment in Python (currently a buggy, only
semi-functional work-in-progress).

It includes:

1. A parser, using macropy's cool PEG implementation.
2. An Abstract Synax Tree.
3. A C re-emitter.
4. Transformation from C-AST to an intermediate representation.
5. Transformation from IR to x86-32 assembly.

## TODO

1. Finish implementing C features (initializers, switch/case, etc)
2. Clean up irregularities in the intermediate representation.
3. Better code generation in the backend.

### C Code
```
extern int
printf(const char *fmt, ...);

int
main(int argc, char **argv)
{
	int i;

	for(i=0; i<10; i++) {
		printf("%d: Hello World\n", i);
	}
	return 0;
}
```
### Abstract Syntax Tree
```
TranslationUnit(body=[Declarator(name='printf',
                                 type=['extern',
                                       'function'],
                                 array=[],
                                 args=[Declarator(name='fmt',
                                                  type=['pointer',
                                                        'const',
                                                        'char'],
                                                  array=[],
                                                  args=[],
                                                  returns=None,
                                                  bitfield=None,
                                                  initializer=None),
                                       Declarator(name=None,
                                                  type=['...'],
                                                  array=[],
                                                  args=[],
                                                  returns=None,
                                                  bitfield=None,
                                                  initializer=None)],
                                 returns=Declarator(name=None,
                                                    type=['int'],
                                                    array=[],
                                                    args=[],
                                                    returns=None,
                                                    bitfield=None,
                                                    initializer=None),
                                 bitfield=None,
                                 initializer=None),
                      Function(signature=Declarator(name='main',
                                                    type=['function'],
                                                    array=[],
                                                    args=[Declarator(name='argc',
                                                                     type=['int'],
                                                                     array=[],
                                                                     args=[],
                                                                     returns=None,
                                                                     bitfield=None,
                                                                     initializer=None),
                                                          Declarator(name='argv',
                                                                     type=['pointer',
                                                                           'pointer',
                                                                           'char'],
                                                                     array=[],
                                                                     args=[],
                                                                     returns=None,
                                                                     bitfield=None,
                                                                     initializer=None)],
                                                    returns=Declarator(name=None,
                                                                       type=['int'],
                                                                       array=[],
                                                                       args=[],
                                                                       returns=None,
                                                                       bitfield=None,
                                                                       initializer=None),
                                                    bitfield=None,
                                                    initializer=None),
                               body=StatementList(body=[Declarator(name='i',
                                                                   type=['int'],
                                                                   array=[],
                                                                   args=[],
                                                                   returns=None,
                                                                   bitfield=None,
                                                                   initializer=None),
                                                        For(initialize=Assign(left=Identifier(name='i'),
                                                                              right=Integer(value=0,
                                                                                            mod='',
                                                                                            rep='dec')),
                                                            expr=Compare(left=Identifier(name='i'),
                                                                         op='lt',
                                                                         right=Integer(value=10,
                                                                                       mod='',
                                                                                       rep='dec')),
                                                            ctrl=PostfixOp(op='postinc',
                                                                           operand=Identifier(name='i')),
                                                            stmt=StatementList(body=[Call(expr=Identifier(name='printf'),
                                                                                          args=[String(value='%d: Hello World\\n',
                                                                                                       mod=''),
                                                                                                Identifier(name='i')])])),
                                                        Return(expr=Integer(value=0,
                                                                            mod='',
                                                                            rep='dec'))]))])
```
### Intermediate Representation
```
    Extern(name='printf')
    Enter(name='main')
    Move(target='%1', val=0, size=4, signed=True)
    Move(target='%2', val='i')
    Store(src0='%1', addr='%2', size=4)
Label(name='lbl_2')
    Load(target='%4', addr='i', size=4, signed=True)
    Move(target='%5', val=10, size=4, signed=True)
    Lt(target='%3', src0='%4', src1='%5')
    IfFalse(src0='%3', label='lbl_4')
    Data(name='str_5', type='str', data='%d: Hello World\\n')
    Move(target='%7', val='str_5', size=4)
    Load(target='%8', addr='i', size=4, signed=True)
    Call(target='printf', args=['%7', '%8'], retval='%6')
    Load(target='%9', addr='i', size=4, signed=True)
    Move(target='%11', val=1, size=4, signed=True)
    Add(target='%10', src0='%9', src1='%11')
    Move(target='%12', val='i')
    Store(src0='%10', addr='%12', size=4)
    Jump(label='lbl_2')
Label(name='lbl_4')
    Move(target='%13', val=0, size=4, signed=True)
    Return(src0='%13')
Label(name='lbl_main_exit_1')
    Leave()
```
### Backend Assembly
```
SECTION .data
str_5:
        DB         '%d: Hello World\n',0
SECTION .text
extern printf
global main
main:
        PUSH       ebp
        PUSH       ebx
        PUSH       esi
        PUSH       edi
        MOV        ebp, esp
        SUB        esp, 4
        MOV        eax, 0
        LEA        ebx, [ebp-20]
        MOV        [ebx], eax
lbl_2:
        MOV        ecx, [ebp-20]
        MOV        edx, 10
        CMP        ecx, edx
        JGE        lbl_4
        MOV        esi, str_5
        MOV        edi, [ebp-20]
        PUSH       edi
        PUSH       esi
        CALL       printf
        MOV        eax, [ebp-20]
        MOV        ebx, 1
        ADD        eax, ebx
        LEA        ecx, [ebp-20]
        MOV        [ecx], eax
        JMP        lbl_2
lbl_4:
        MOV        edx, 0
        MOV        eax, edx
lbl_main_exit_1:
        MOV        esp, ebp
        POP        edi
        POP        esi
        POP        ebx
        POP        ebp
        RET        
```
