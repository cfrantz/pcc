from collections import OrderedDict
import subprocess
import ia
import symtab
import typeinfo
import error
import cast as C
import util


def sizeptr(sz, ptr):
    if sz is None:
        return ptr
    if sz == 1:
        return "byte %s" % ptr
    if sz == 2:
        return "word %s" % ptr
    if sz == 4:
        return ptr
    error.fatal("Unknown size %s for %s", sz, ptr)

def fakesym(count=1):
    return C.Declarator(name='%fake', type=['int'], count=count)

class x86inst(object):
    _fields = []
    _defval = []
    def __init__(self, **kwargs):
        C.print_extra = True
        for k in self._fields:
            v = kwargs.pop(k, None)
            setattr(self, k, v)

        if kwargs:
            raise Exception('Unknown initializers for class %s: %r' % (
                self.__class__.__name__, kwargs)) 

    def __str__(self):
        name = getattr(self, 'iname', self.__class__.__name__)
        args = []
        size = None
        for k in self._fields:
            if k == 'size':
                size = self.size
                continue
            elif k == 'cond':
                name += self.cond.upper()
                continue
            else:
                v = getattr(self, k, None)
                if v is None:
                    continue
                if k == 'cl':
                    v = k
                if k == 'src':
                    v = sizeptr(size, v)
            args.append(str(v))

        if name == 'LABEL':
            return '%s:' % self.name
        return '        %-10s %s' % (name, ', '.join(args))
        
    def __repr__(self):
        args = []
        for k in self._fields:
            v = getattr(self, k)
            if v is not None:
                args.append('%s=%r' % (k, v))
        return '%s(%s)' % (self.__class__.__name__, ', '.join(args))

class Comment(x86inst):
    _fields = ['comment']
    def __init__(self, comment):
        self.comment = comment
    def __str__(self):
        return '; %s' % self.comment

class CDQ(x86inst): pass
class RET(x86inst): pass

class LEA(x86inst):
    _fields = ['dst', 'addr']

class rrmimm(x86inst):
    _fields = ['dst', 'src', 'imm']
class MOV(rrmimm): pass
class MOVSX(x86inst):
    _fields = ['size', 'dst', 'src']
class MOVZX(x86inst):
    _fields = ['size', 'dst', 'src']
class ADD(rrmimm): pass
class SUB(rrmimm): pass
class AND(rrmimm): pass
class IMUL(rrmimm): pass
class OR(rrmimm): pass
class XOR(rrmimm): pass

class CMP(rrmimm): pass

class shift(x86inst):
    _fields = ['dst', 'imm', 'cl']
class SHL(shift): pass
class SHR(shift): pass
class SAR(shift): pass

class lblrm(x86inst):
    _fields = ['dst', 'label']
class JMP(lblrm): pass
class CALL(lblrm): pass

class onerm(x86inst):
    _fields = ['dst']
class DIV(onerm): pass
class IDIV(onerm): pass
class MUL(onerm): pass
class NEG(onerm): pass
class NOT(onerm): pass
class PUSH(onerm): pass
class POP(onerm): pass

class SETcc(x86inst):
    iname = 'SET'
    _fields = ['cond', 'dst']
class JMPcc(x86inst):
    iname = 'J'
    _fields = ['cond', 'label']

class LABEL(x86inst):
    _fields = ['name']

class x86data(x86inst):
    _fields = ['val']
class DB(x86data): pass
class DW(x86data): pass
class DD(x86data): pass
class DQ(x86data): pass

class Backend(object):
    _inverted = {
        'e':  'ne',
        'ne': 'e',
        'l':  'ge',
        'le': 'g',
        'g':  'le',
        'ge': 'l',
    }
    _registers = [
        'eax', 'ebx', 'ecx', 'edx', 'esi', 'edi', 
    ]
    _wordreg = {
        'eax': 'ax',
        'ebx': 'bx',
        'ecx': 'cx',
        'edx': 'dx',
        'esi': 'si',
        'edi': 'di',
    }
    _bytereg = {
        'eax': 'al',
        'ebx': 'bl',
        'ecx': 'cl',
        'edx': 'dl',
    }
    def __init__(self, ilist):
        self.wordsz = 4
        self.reset_regs()
        self.flags = None
        self.flagrslt = None

        self.ilist = ilist
        self.alist = ['SECTION .text']
        self.dlist = ['SECTION .data']
        self.stackpatch = None

    def reset_regs(self):
        self.registers = OrderedDict()
        for r in self._registers:
            self.registers[r] = None

    def _reginfo(self):
        r = []
        for k,v in self.registers.items():
            if v:
                v = "%s(%d)" % (v.name, v.count)
            else:
                v = '-'
            r.append('%s:%s' % (k,v))
        return '{ %s }' % (', '.join(r))

    def generate(self):
        for inst in self.ilist:
            name = inst.__class__.__name__
            emit = getattr(self, 'emit_%s' % name, None)
            error.info("Translating %s", inst)
            if emit is None:
                self.a('### NOT IMPLEMENTED: %s' % inst)
                continue
            a = len(self.alist)
            emit(inst)
            b = len(self.alist)
            for i in range(a-b, 0):
                error.info("          %r", self.alist[i])
            error.info('### REGINFO %s', self._reginfo())
        return self.dlist + self.alist

    def a(self, *args):
        self.alist.extend(args)

    def data(self, *args):
        self.dlist.extend(args)

    def set_flags(self, sym, flags):
        sym = self.resolve(sym)
        self.flags = flags
        self.flagrslt = sym.name

    def in_flags(self, sym):
        # is sym's value currently encoded in the flags register
        sym = self.resolve(sym)
        return self.flagrslt == sym.name

    @property
    def inv_flags(self):
        # Inverted form of flags
        return self._inverted[self.flags]

    def resolve(self, sym):
        if isinstance(sym, basestring):
            sym = symtab.ident.get(sym)
        return sym

    def offset(self, sym):
        sym = self.resolve(sym)
        ofs = -sym.offset
        # TODO(cfrantz) make these constants dynamic based on
        # which registers are used.
        if ofs <= 0:
            ofs -= sym.szaligned
            return ofs - 16
        else:
            return ofs + 4

    def spill(self, value):
        # Spill value to memory
        if value.count == 0:
            return

        error.info("spilling %s(%s) to memory", value.name, value.reg)
        if value.offset is None:
            symtab.ident.top().alloc(value.name, value)
            error.info("    allocating space: %d", self.offset(value))
        reg = value.reg
        value.reg = None
        # Increase the count, since we will reload this later
        value.count += 1
        self.a(MOV(dst=self.addr(value), src=reg))

    def getreg(self, reg=None, sym=None):
        # This is a rather dumb register allocator.  It should
        # prefer the call-clobbered registers (eax, edx, ecx) over the
        # preserved registers (ebx, esi, edi).

        # Pick the first register with zero usage count, or
        # the first register in the queue
        sym = self.resolve(sym)
        sym.count -= 1

        # If reg is None the user wants any register
        if reg is None:
            reg = self.registers.keys()

        # If reg is a list, the user wants any one of the registers in the list
        if isinstance(reg, list):
            r0 = None
            for r, val in self.registers.items():
                if r in reg:
                    if not r0:
                        r0 = r
                    if val is None:
                        reg = r
                        break
            if isinstance(reg, list):
                reg = r0

        value = self.registers.pop(reg)
        if value:
            self.spill(value)
        if sym.count == 0:
            self.registers[reg] = None
            return None
        self.registers[reg] = sym
        sym.reg = reg
        return reg

    def allocreg(self, sym, reg=None):
        # Allocate a register for sym (possibly a specific register)
        sym = self.resolve(sym)
        if sym.reg is None:
            error.info("%40s Allocating reg for %s (count=%d)", "", sym.name, sym.count)
            self.getreg(reg, sym)
            error.info("%40s reg=%s (count=%d)", "", sym.reg, sym.count)
        else:
            error.fatal('Symbol %r already using register %r', sym.name, sym.reg)
        return sym.reg

    def usereg(self, sym, reg=None, clear=True):
        # Use the register in which sym resides.
        # Maybe load sym (if it has been spilled or resides in flags).
        # Maybe transfer sym into the requested register.
        # If the use count decrements to zero, free the register (except if
        # clear is false)
        sym = self.resolve(sym)
        if sym.reg is None:
            if self.in_flags(sym):
                reg = self.getreg(self._bytereg.keys(), sym)
                bytereg = self._bytereg[reg]
                self.a(SETcc(dst=bytereg, cond=self.flags),
                       MOVZX(dst=sym.reg, src=bytereg))
            else:
                src = self.addr(sym)
                error.info('Loading %s from memory (offset=%d)->%s', sym.name, sym.offset, src)
                reg = self.allocreg(sym.name, reg)
                self.a(MOV(dst=sym.reg, src=src))
        if reg is None:
            reg = sym.reg

        if sym.reg != reg:
            # Add one because allocreg decrements, transfer
            # to the new register
            error.info("register copy for %s in %s", sym.name, sym.reg)
            sym.count += 1
            symreg = sym.reg; sym.reg = None
            self.registers[symreg] = None
            reg = self.allocreg(sym.name, reg)
            error.info("                        %s", reg)
            self.a(MOV(dst=reg, src=symreg))

        self.decreg(sym, clear)
        return reg

    def decreg(self, sym, clear=True):
        sym = self.resolve(sym)
        if sym.reg:
            self.registers[sym.reg].count -= 1
            if self.registers[sym.reg].count == 0:
                if clear:
                    self.registers[sym.reg] = None
                    sym.reg = None
        return sym.reg

    def modrm(self, sym):
        # return a mod r/m form for sym
        sym = self.resolve(sym)
        if sym.reg:
            return self.usereg(sym)
        if sym.offset is not None:
            return 'dword [ebp%+d]' % self.offset(sym)
        error.fatal('Symbol %r not in register and has no offset', sym.name)
        return self.usereg(sym)

    def lastuse(self, sym):
        sym = self.resolve(sym)
        if sym.reg and sym.count == 1:
            return True
        return False

    def flowdst(self, target, src0):
        # Determine if this is src0's last use and if we can immediately
        # claim src0's register for target.
        target = self.resolve(target)
        src0 = self.resolve(src0)
        self.usereg(src0, clear=False)
        error.info("flowreg: checking %s -> %s", src0.name, src0.reg)

        if self.registers[src0.reg].count == 0:
            target.reg = src0.reg
            self.registers[src0.reg] = target
            target.count -= 1
            src0.reg = None
            return target.reg

        # If we can't immediately reuse target, allocate a new register
        # and copy the value from src0.
        src0 = self.resolve(src0)
        targetregs = [r for r in self._registers if r != src0.reg]
        self.allocreg(target, targetregs)
        error.info('allocating register for target: %s -> %s', target.name, target.reg)
        self.a(MOV(dst=target.reg, src=src0.reg))
        return target.reg

    def addr(self, sym):
        # return the address form for sym
        if not isinstance(sym, basestring):
            sym = sym.name
        tbl, sym  = symtab.ident.find(sym)
        if tbl.type == 'global':
            return '[%s]' % sym.name
        if sym.reg:
            return '[%s]' % self.usereg(sym)
        if sym.offset is not None:
            return '[ebp%+d]' % self.offset(sym)
        if sym.name[0] == '%':
            return '[%s]' % self.usereg(sym)

    def issigned(self, inst, checkboth=True):
        # return whether or not this instruction operates on signed or unsigned
        # values
        sym = self.resolve(inst.src0)
        signed = typeinfo.issigned(sym)
        src1 = getattr(inst, 'src1', None)
        if checkboth and src1:
            sym = self.resolve(inst.src1)
            signed |= typeinfo.issigned(sym)
        return signed

    def emit_SymPush(self, inst):
        # A pseudo-instruction for managing symbol tables
        symtab.ident.push(inst.symtab)

    def emit_SymPop(self, inst):
        # A pseudo-instruction for managing symbol tables
        symtab.ident.pop()

    def emit_Constant(self, inst):
        # Load a constaint value into a register
        target = self.allocreg(inst.target)
        self.a(MOV(dst=target, imm=inst.val))

    def emit_EffectiveAddr(self, inst):
        tab, sym = symtab.ident.find(inst.symbol)
        target = self.allocreg(inst. target)
        if tab.type == 'global':
            self.a(MOV(dst=target, imm=inst.symbol))
        else:
            self.a(LEA(dst=target, addr=self.addr(inst.symbol)))

    def emit_Move(self, inst):
        # Move a value into a register (const, effective addr or reg-to-reg)
        target = self.resolve(inst.target)
        if target.reg:
            # Copy over the top of another value
            src = self.usereg(inst.src0)
            self.a(MOV(dst=target.reg, src=src))
        else:
            self.flowdst(inst.target, inst.src0)

    def emit_Load(self, inst):
        # Load a value from memory
        src = self.resolve(inst.addr)
        error.info('Load: checking %s', src)
        if src.reg and src.count == 1:
            reg = src.reg
            addr = self.addr(inst.addr)
            target = self.getreg(reg, inst.target)
        else:
            addr = self.addr(inst.addr)
            target = self.allocreg(inst.target)

        if inst.size < self.wordsz:
            if inst.signed:
                self.a(MOVSX(dst=target, src=addr, size=inst.size))
            else:
                self.a(MOVZX(dst=target, src=addr, size=inst.size))
        else:
            self.a(MOV(dst=target, src=addr))

    def emit_Store(self, inst):
        # Store a value to memory
        dst = sizeptr(inst.size, self.addr(inst.addr))
        src = self.usereg(inst.src0)
        breg = None
        if inst.size == 1:
            if src not in self._bytereg:
                # If the value isn't in a register with a byte name, transfer
                # it to a better register.
                breg = self.getreg(self._bytereg.keys(), fakesym(2))
                self.a(MOV(dst=breg, src=src))
                src = breg
            src = self._bytereg[src]
        elif inst.size == 2:
            src = self._wordreg[src]
        else:
            pass
        self.a(MOV(dst=dst, src=src))
        if breg:
            self.registers[breg] = None

    def emit_Negate(self, inst):
        # Negate (2's complement) a value
        self.a(NEG(dst=self.flowdst(inst.target, inst.src0)))
        self.set_flags(inst.target, 'e')

    def emit_Complement(self, inst):
        # Complement (1's complement) a value
        self.a(NOT(dst=self.flowdst(inst.target, inst.src0)))

    def emit_Not(self, inst):
        # Logical not of a value
        r = self.usereg(inst.src0)
        self.a(OR(dst=r, src=r))
        self.set_flags(inst.target, 'e')

    def emit_Add(self, inst):
        target = self.flowdst(inst.target, inst.src0)
        self.a(ADD(dst=target, src=self.modrm(inst.src1)))

    def emit_Sub(self, inst):
        target = self.flowdst(inst.target, inst.src0)
        self.a(SUB(dst=target, src=self.modrm(inst.src1)))

    def emit_Mul(self, inst):
        if self.issigned(inst):
            # signed multiply is the only version of mul that is nice
            target = self.flowdst(inst.target, inst.src0)
            self.a(IMUL(dst=target, src=self.modrm(inst.src1)))
        else:
            # Get src0 into eax, and clobber edx since the instruction
            # puts the high part of the result there.  Then inform the
            # target that it is in eax.
            self.usereg(inst.src0, 'eax')
            self.getreg('edx', fakesym())
            self.a(MUL(dst=self.modrm(inst.src1)))
            self.registers['edx'] = None
            self.allocreg(inst.target, 'eax')

    def emit_Div(self, inst):
        # Get src0 into eax, maybe sign-extend into edx.  Divide and
        # then inform the target that it is in eax
        self.usereg(inst.src0, 'eax')
        self.getreg('edx', fakesym())
        if self.issigned(inst):
            divide = IDIV
            self.a(CDQ())
        else:
            divide = DIV
            self.a(XOR(dst='edx', src='edx'))
        self.a(divide(dst=self.modrm(inst.src1)))
        self.registers['edx'] = None
        self.allocreg(inst.target, 'eax')

    def emit_Mod(self, inst):
        # Get src0 into eax, maybe sign-extend into edx.  Divide and
        # then inform the target that it is in edx
        self.usereg(inst.src0, 'eax')
        self.getreg('edx', fakesym())
        if self.issigned(inst):
            divide = IDIV
            self.a(CDQ())
        else:
            divide = DIV
            self.a(XOR(dst='edx', src='edx'))
        self.a(divide(dst=self.modrm(inst.src1)))
        self.registers['eax'] = None
        self.allocreg(inst.target, 'edx')

    def emit_Sub(self, inst):
        target = self.flowdst(inst.target, inst.src0)
        self.a(SUB(dst=target, src=self.modrm(inst.src1)))

    def emit_And(self, inst):
        target = self.flowdst(inst.target, inst.src0)
        self.a(AND(dst=target, src=self.modrm(inst.src1)))

    def emit_Or(self, inst):
        target = self.flowdst(inst.target, inst.src0)
        self.a(OR(dst=target, src=self.modrm(inst.src1)))

    def emit_Xor(self, inst):
        target = self.flowdst(inst.target, inst.src0)
        self.a(XOR(dst=target, src=self.modrm(inst.src1)))

    def emit_Shl(self, inst):
        # shift amount goes into cl (e.g. ecx)
        cl = self.usereg(inst.src1, 'ecx')
        r = self.flowdst(inst.target, inst.src0)
        self.a(SHL(dst=r, cl=cl))

    def emit_Shr(self, inst):
        # shift amount goes into cl (e.g. ecx)
        cl = self.usereg(inst.src1, 'ecx')
        r = self.flowdst(inst.target, inst.src0)
        if self.issigned(inst, checkboth=False):
            self.a(SAR(dst=r, cl=cl))
        else:
            self.a(SHR(dst=r, cl=cl))

    def emit_Eq(self, inst):
        # Logical operations don't allocate a result register, they put
        # their results in the flags bits.
        self.a(CMP(dst=self.usereg(inst.src0), src=self.modrm(inst.src1)))
        self.set_flags(inst.target, 'e')
    def emit_Ne(self, inst):
        self.a(CMP(dst=self.usereg(inst.src0), src=self.modrm(inst.src1)))
        self.set_flags(inst.target, 'ne')
    def emit_Lt(self, inst):
        self.a(CMP(dst=self.usereg(inst.src0), src=self.modrm(inst.src1)))
        self.set_flags(inst.target, 'l')
    def emit_Le(self, inst):
        self.a(CMP(dst=self.usereg(inst.src0), src=self.modrm(inst.src1)))
        self.set_flags(inst.target, 'le')
    def emit_Gt(self, inst):
        self.a(CMP(dst=self.usereg(inst.src0), src=self.modrm(inst.src1)))
        self.set_flags(inst.target, 'g')
    def emit_Ge(self, inst):
        self.a(CMP(dst=self.usereg(inst.src0), src=self.modrm(inst.src1)))
        self.set_flags(inst.target, 'ge')

    def emit_IfTrue(self, inst):
        # Check the flags condition and jump if true
        if self.in_flags(inst.src0):
            self.decreg(inst.src0)
        else:
            r = self.usereg(inst.src0)
            self.a(OR(dst=r, src=r))
            self.set_flags(inst.src0, 'ne')
        self.a(JMPcc(cond=self.flags, label=inst.label))

    def emit_IfFalse(self, inst):
        # Check the flags condition and jump if not true
        if self.in_flags(inst.src0):
            self.decreg(inst.src0)
        else:
            r = self.usereg(inst.src0)
            self.a(OR(dst=r, src=r))
            self.set_flags(inst.src0, 'ne')
        self.a(JMPcc(cond=self.inv_flags, label=inst.label))

    def emit_Label(self, inst):
        self.a(LABEL(name=inst.name))

    def emit_Call(self, inst):
        # Push arguments and call
        sz = 0
        for a in inst.args:
            # FIXME: sz = (sz+sizeof(a))
            error.info("Selecting register for argument %r", a)
            self.a(MOV(dst='[esp+%d]'%sz, src=self.usereg(a)))
            sz += 4
        self.argspc = max(self.argspc, sz)
        # eax, edx and ecx are call clobbered
        self.getreg('eax', fakesym())
        self.getreg('edx', fakesym())
        self.getreg('ecx', fakesym())
        if inst.target[0] == '%':
            self.a(CALL(dst=self.modrm(inst.target)))
        else:
            self.a(CALL(label=inst.target))
        self.allocreg(inst.retval, 'eax')

    def emit_Jump(self, inst):
        if inst.label:
            self.a(JMP(label=inst.label))
        else:
            self.a(JMP(target=sefl.modrm(inst.target)))

    def emit_Return(self, inst):
        # Get the result into eax
        # The real return is actually stored in ia as jump
        # to the leave instruction
        self.usereg(inst.src0, 'eax')

    def emit_Enter(self, inst):
        # Emit a function prologue that allocates enough stack space.
        # Since the amount of space needed isn't known until after the
        # function has been assembled, provide a pointer to a patch location.
        self.reset_regs()
        sym = self.resolve(inst.name)
        if not typeinfo.isstatic(sym):
            self.a('global %s' % inst.name)

        self.argspc = 0
        self.stackpatch = SUB(dst='esp', imm=0)
        self.a(LABEL(name=inst.name))

        values = symtab.ident.top().values(sortkey='offset')
        for v in values:
            if v.offset is None:
                continue
            self.a(Comment('    %s: %r' % (v.name, self.offset(v.name))))

        self.a(
                PUSH(dst='ebp'),
                MOV(dst='ebp', src='esp'),
                AND(dst='esp', imm=-16),
                self.stackpatch,
                MOV(dst='[ebp-4]', src='ebx'),
                MOV(dst='[ebp-8]', src='esi'),
                MOV(dst='[ebp-12]', src='edi'))

    def emit_Leave(self, inst):
        # Emit a function epilogue, and patch the stack allocation in the
        # prologue.
        stacksz = symtab.ident.top().sizeof()
        # FIXME: Don't save space for ebx, esi, edi if we don't use them.
        # 16 bytes for ebp, ebx, esi, edi +
        # stacksz bytes for locals + argspc bytes for arguments +
        # alignment to 16 bytes for calling convention
        self.stackpatch.imm = (16 + stacksz + self.argspc + 15) & ~15
        self.a(
                MOV(src='[ebp-4]', dst='ebx'),
                MOV(src='[ebp-8]', dst='esi'),
                MOV(src='[ebp-12]', dst='edi'),
                MOV(dst='esp', src='ebp'),
                POP(dst='ebp'),
                RET(),
                '', '')

    def emit_Data(self, inst):
        if inst.name:
            self.data(LABEL(name=inst.name))
        if inst.type == 'str':
            self.data(DB(val=util.cstr(inst.data)))
        else:
            error.fatal("Don't know how to emit data %r", inst.type)

    def emit_Extern(self, inst):
        sym = self.resolve(inst.name)
        if not sym.impl:
            self.a('extern %s' % inst.name)


    @staticmethod
    def assembler(filename):
        return subprocess.call(['nasm', '-felf', '-g', filename])

    @staticmethod
    def linker(filenames):
        outfile = util.rename(filenames[0], None)
        filenames = [util.rename(f, 'o') for f in filenames]
        return subprocess.call(['gcc', '-m32', '-o', outfile] + filenames)
