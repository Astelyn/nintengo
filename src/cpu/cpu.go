package cpu

import (
	"mmu"
)

// Status flag bitmasks
const (
	C = 0x01 // Carry
	Z = 0x02 // Zero
	I = 0x03 // Interrupt disable
	D = 0x08 // Decimal mode
	B = 0x10 // Break
	V = 0x40 // Overflow
	S = 0x80 // Sign
)

// Interrupt types
const (
	NONE = iota
	IRQ
	NMI
	RESET
)

type CPU struct {
	A           byte         // Accumulator
	X           byte         // X register (8-bit)
	Y           byte         // Y register (8-bit)
	S           byte         // Status flags
	SP          uint16       // Stack pointer
	PC          uint16       // Program counter
	mmu         mmu.MMU      // Memory Mapped Unit
	opcode      byte         // Current opcode
	operand     byte         // Current operand value
	operAddr    uint16       // Current operand address
	Interrupt   int          // Interrupt requested
	insMap      [256]func () // Instruction function map
	clockIns    int          // Cycle count of current instruction
	clockTotal  int          // Total cycles run
	clockGoal   int          // Cycle count before stopping
	penaltyAddr bool         // Addressing mode cycle penalty
	penaltyOp   bool         // Instruction cycle penalty
}

func NewCPU() *CPU {
	cpu := new(CPU)
	cpu.Reset()
	cpu.initInsMap()
	return cpu
}

func (cpu *CPU) Reset() {
	cpu.A = 0
	cpu.X = 0
	cpu.Y = 0
	cpu.S = 0x34
	cpu.SP = 0x1FD
	cpu.PC = 0
	cpu.operand = 0
	cpu.operAddr = 0
	cpu.Interrupt = NONE
	cpu.clockIns = 0
	cpu.clockTotal = 0
	cpu.clockGoal = 0
	cpu.penaltyAddr = false
	cpu.penaltyOp = false
}

func (cpu *CPU) ConnectMMU(mmu mmu.MMU) {
	cpu.mmu = mmu
}

func (cpu *CPU) InterruptIRQ() {
	cpu.pushWord(cpu.PC)
	cpu.pushByte(cpu.S)
	cpu.PC = cpu.mmu.ReadWord(0xFFFE)
}

func (cpu *CPU) InterruptNMI() {
	cpu.pushWord(cpu.PC)
	cpu.pushByte(cpu.S)
	cpu.PC = cpu.mmu.ReadWord(0xFFFA)
}

func (cpu *CPU) InterruptReset() {
	// TODO: PPU stuff

	cpu.PC = cpu.mmu.ReadWord(0xFFFC)
}

func (cpu *CPU) Run(cycles int) {
	cpu.clockGoal += cycles
	for cpu.clockTotal < cpu.clockGoal {
		cpu.Step()
		cpu.clockTotal += cpu.clockIns
	}
}

func (cpu *CPU) Step() {
	switch cpu.Interrupt {
	case IRQ:
		if !cpu.TestFlag(I) {
			cpu.InterruptIRQ()
		}
		cpu.Interrupt = NONE
	case NMI:
		cpu.InterruptNMI()
		cpu.Interrupt = NONE
	case RESET:
		cpu.InterruptReset()
		cpu.Interrupt = NONE
	}

	cpu.opcode = cpu.mmu.ReadByte(cpu.PC)
	cpu.PC++

	// Debug only
	cpu.disassemble()

	cpu.penaltyAddr = false
	cpu.penaltyOp = false
	cpu.clockIns = 0

	cpu.insMap[cpu.opcode]()

	if cpu.penaltyAddr && cpu.penaltyOp {
		cpu.clockIns++
	}
}

// Stack helper functions
// ----------------------------------------------------------------------------

func (cpu *CPU) pushByte(val byte) {
	cpu.mmu.WriteByte(cpu.SP, val)
	cpu.SP--
}

func (cpu *CPU) pushWord(val uint16) {
	cpu.mmu.WriteWord(cpu.SP, val)
	cpu.SP -= 2
}

func (cpu *CPU) pullByte() byte {
	cpu.SP++
	return cpu.mmu.ReadByte(cpu.SP - 1)
}

func (cpu *CPU) pullWord() uint16 {
	cpu.SP += 2
	return cpu.mmu.ReadWord(cpu.SP - 2)
}

// Status flag helper functions
// ----------------------------------------------------------------------------

func (cpu *CPU) SetFlag(flag byte) {
	cpu.S |= flag
}

func (cpu *CPU) ClearFlag(flag byte) {
	cpu.S &= (^flag)
}

func (cpu *CPU) TestFlag(flag byte) bool {
	return (cpu.S & flag) == flag
}

func (cpu *CPU) calcCarry(val uint16) {
	if (val & 0xFF00) > 0 {
		cpu.SetFlag(C)
	} else {
		cpu.ClearFlag(C)
	}
}

func (cpu *CPU) calcZero(val byte) {
	if val > 0x00 {
		cpu.ClearFlag(Z)
	} else {
		cpu.SetFlag(Z)
	}
}

func (cpu *CPU) calcSign(val byte) {
	if (val & 0x80) > 0 {
		cpu.SetFlag(S)
	} else {
		cpu.ClearFlag(S)
	}
}

func (cpu *CPU) calcOverflow(val uint16) {
	if ((uint16(cpu.A) ^ val) & (uint16(cpu.A) ^ val) & 0x80) > 0 {
		cpu.SetFlag(V)
	} else {
		cpu.ClearFlag(V)
	}
}

// Addressing mode functions
// ----------------------------------------------------------------------------

// Immediate -- nn
func (cpu *CPU) fetchImm() {
	cpu.operAddr = cpu.PC
	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC++
}

// Zero Page -- [nn]
func (cpu *CPU) fetchZpg() {
	cpu.operAddr = uint16(cpu.mmu.ReadByte(cpu.PC))
	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC++
}

// Zero Page, X -- [nn + X]
func (cpu *CPU) fetchZpgX() {
	cpu.operAddr = uint16(cpu.mmu.ReadByte(cpu.PC)) + uint16(cpu.X) & 0xFF
	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC++
}

// Zero Page, Y -- [nn + Y]
func (cpu *CPU) fetchZpgY() {
	cpu.operAddr = uint16(cpu.mmu.ReadByte(cpu.PC)) + uint16(cpu.Y) & 0xFF
	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC++
}

// Absolute -- [nnnn]
func (cpu *CPU) fetchAbs() {
	cpu.operAddr = cpu.mmu.ReadWord(cpu.PC)
	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC += 2
}

// Absolute, X -- [nnnn + X]
func (cpu *CPU) fetchAbsX() {
	cpu.operAddr = cpu.mmu.ReadWord(cpu.PC)

	start := cpu.operAddr & 0xFF00

	cpu.operAddr += uint16(cpu.X)

	if start != (cpu.operAddr & 0xFF00) {
		cpu.penaltyAddr = true
	}

	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC += 2
}

// Absolute, Y -- [nnnn + Y]
func (cpu *CPU) fetchAbsY() {
	cpu.operAddr = cpu.mmu.ReadWord(cpu.PC)

	start := cpu.operAddr & 0xFF00

	cpu.operAddr += uint16(cpu.Y)

	if start != (cpu.operAddr & 0xFF00) {
		cpu.penaltyAddr = true
	}

	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC += 2
}

// Indirect -- [[nnnn]]
func (cpu *CPU) fetchInd() {
	l := cpu.mmu.ReadWord(cpu.PC)
	h := (l & 0xFF00) | ((l + 1) & 0xFF)

	cpu.operAddr = uint16(cpu.mmu.ReadByte(l)) |
			(uint16(cpu.mmu.ReadByte(h))<<8)
	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC += 2
}

// (Indirect, X) -- [[nn + X]]
func (cpu *CPU) fetchIndX() {
	cpu.operAddr = uint16(cpu.mmu.ReadByte(uint16(cpu.mmu.ReadByte(cpu.PC)) +
			uint16(cpu.X)))
	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC++
}

// (Indirect), Y -- [[nn] + Y]
func (cpu *CPU) fetchIndY() {
	cpu.operAddr = uint16(cpu.mmu.ReadByte(uint16(cpu.mmu.ReadByte(cpu.PC))))

	start := cpu.operAddr & 0xFF00

	cpu.operAddr += uint16(cpu.Y)

	if start != (cpu.operAddr & 0xFF00) {
		cpu.penaltyAddr = true
	}

	cpu.operand = cpu.mmu.ReadByte(cpu.operAddr)
	cpu.PC++
}

// Relative -- nn (signed, for branches only)
func (cpu *CPU) fetchRel() {
	cpu.operAddr = uint16(cpu.mmu.ReadByte(cpu.PC))
	if (cpu.operAddr & 0x80) > 0 {
		cpu.operAddr |= 0xFF00
	}
	cpu.PC++
}

// Instruction functions
// ----------------------------------------------------------------------------

// Add M to A with Carry
func (cpu *CPU) adc() {
	cpu.penaltyOp = true

	ret := uint16(cpu.A) + uint16(cpu.operand) + uint16(cpu.S & C)

	cpu.calcCarry(ret)
	cpu.calcOverflow(ret)

	ret &= 0xFF

	cpu.calcZero(byte(ret))
	cpu.calcSign(byte(ret))

	cpu.A = byte(ret)
}

// Logical AND M with A
func (cpu *CPU) and() {
	cpu.penaltyOp = true

	ret := cpu.A & cpu.operand

	cpu.calcZero(ret)
	cpu.calcSign(ret)

	cpu.A = ret
}

// Shift M left one bit
func (cpu *CPU) asl() {
	ret := uint16(cpu.operand)<<1

	cpu.calcCarry(ret)

	ret &= 0xFF

	cpu.calcZero(byte(ret))
	cpu.calcSign(byte(ret))

	cpu.mmu.WriteByte(cpu.operAddr, byte(ret))
}

// Shift A left one bit
func (cpu *CPU) aslAcc() {
	ret := uint16(cpu.A)<<1

	cpu.calcCarry(ret)

	ret &= 0xFF

	cpu.calcZero(byte(ret))
	cpu.calcSign(byte(ret))

	cpu.A = byte(ret)
}

// Branch if Carry not set
func (cpu *CPU) bcc() {
	if !cpu.TestFlag(C) {
		last := cpu.PC

		cpu.PC = cpu.operAddr

		if (last & 0xFF00) != (cpu.PC & 0xFF00) {
			cpu.clockIns += 2
		} else {
			cpu.clockIns++
		}
	}
}

// Branch if Carry set
func (cpu *CPU) bcs() {
	if cpu.TestFlag(C) {
		last := cpu.PC

		cpu.PC = cpu.operAddr

		if (last & 0xFF00) != (cpu.PC & 0xFF00) {
			cpu.clockIns += 2
		} else {
			cpu.clockIns++
		}
	}
}

// Branch if Zero set
func (cpu *CPU) beq() {
	if cpu.TestFlag(Z) {
		last := cpu.PC

		cpu.PC = cpu.operAddr

		if (last & 0xFF00) != (cpu.PC & 0xFF00) {
			cpu.clockIns += 2
		} else {
			cpu.clockIns++
		}
	}
}

// Test bits in M with A
func (cpu *CPU) bit() {
	ret := cpu.A & cpu.operand

	cpu.calcZero(ret)
	cpu.S = (cpu.S & 0x3F) | (cpu.operand & 0xC0)
}

// Branch if Sign set
func (cpu *CPU) bmi() {
	if cpu.TestFlag(S) {
		last := cpu.PC

		cpu.PC = cpu.operAddr

		if (last & 0xFF00) != (cpu.PC & 0xFF00) {
			cpu.clockIns += 2
		} else {
			cpu.clockIns++
		}
	}
}

// Branch if Zero not set
func (cpu *CPU) bne() {
	if !cpu.TestFlag(Z) {
		last := cpu.PC

		cpu.PC = cpu.operAddr

		if (last & 0xFF00) != (cpu.PC & 0xFF00) {
			cpu.clockIns += 2
		} else {
			cpu.clockIns++
		}
	}
}

// Branch if Sign not set
func (cpu *CPU) bpl() {
	if !cpu.TestFlag(S) {
		last := cpu.PC

		cpu.PC = cpu.operAddr

		if (last & 0xFF00) != (cpu.PC & 0xFF00) {
			cpu.clockIns += 2
		} else {
			cpu.clockIns++
		}
	}
}

// Force break
func (cpu *CPU) brk() {
	cpu.PC++
	cpu.pushWord(cpu.PC)
	cpu.pushByte(cpu.S | B)
	cpu.SetFlag(I)
	cpu.PC = cpu.mmu.ReadWord(0xFFFE)
}

// Branch if V not set
func (cpu *CPU) bvc() {
	if !cpu.TestFlag(V) {
		last := cpu.PC

		cpu.PC = cpu.operAddr

		if (last & 0xFF00) != (cpu.PC & 0xFF00) {
			cpu.clockIns += 2
		} else {
			cpu.clockIns++
		}
	}
}

// Branch if V set
func (cpu *CPU) bvs() {
	if cpu.TestFlag(V) {
		last := cpu.PC

		cpu.PC = cpu.operAddr

		if (last & 0xFF00) != (cpu.PC & 0xFF00) {
			cpu.clockIns += 2
		} else {
			cpu.clockIns++
		}
	}
}

// Clear C
func (cpu *CPU) clc() {
	cpu.ClearFlag(C)
}

// Clear D
func (cpu *CPU) cld() {
	cpu.ClearFlag(D)
}

// Clear I
func (cpu *CPU) cli() {
	cpu.ClearFlag(I)
}

// Clear V
func (cpu *CPU) clv() {
	cpu.ClearFlag(V)
}

// Compare M and A
func (cpu *CPU) cmp() {
	cpu.penaltyOp = true

	ret := uint16(cpu.A) - uint16(cpu.operand)

	if cpu.A >= cpu.operand {
		cpu.SetFlag(C)
	} else {
		cpu.ClearFlag(C)
	}

	if cpu.A == cpu.operand {
		cpu.SetFlag(Z)
	} else {
		cpu.ClearFlag(Z)
	}

	ret &= 0xFF

	cpu.calcSign(byte(ret))
}

// Compare M and X
func (cpu *CPU) cpx() {
	ret := uint16(cpu.X) - uint16(cpu.operand)

	if cpu.X >= cpu.operand {
		cpu.SetFlag(C)
	} else {
		cpu.ClearFlag(C)
	}

	if cpu.X == cpu.operand {
		cpu.SetFlag(Z)
	} else {
		cpu.ClearFlag(Z)
	}

	ret &= 0xFF

	cpu.calcSign(byte(ret))
}

// Compare M and Y
func (cpu *CPU) cpy() {
	ret := uint16(cpu.Y) - uint16(cpu.operand)

	if cpu.Y >= cpu.operand {
		cpu.SetFlag(C)
	} else {
		cpu.ClearFlag(C)
	}

	if cpu.Y == cpu.operand {
		cpu.SetFlag(Z)
	} else {
		cpu.ClearFlag(Z)
	}

	ret &= 0xFF

	cpu.calcSign(byte(ret))
}

// Decrement M
func (cpu *CPU) dec() {
	ret := cpu.operand - 1

	cpu.calcZero(ret)
	cpu.calcSign(ret)

	cpu.mmu.WriteByte(cpu.operAddr, ret)
}

// Decrement X
func (cpu *CPU) dex() {
	cpu.X--

	cpu.calcZero(cpu.X)
	cpu.calcSign(cpu.X)
}

// Decrement Y
func (cpu *CPU) dey() {
	cpu.Y--

	cpu.calcZero(cpu.Y)
	cpu.calcSign(cpu.Y)
}

// Logical XOR M with A
func (cpu *CPU) eor() {
	cpu.penaltyOp = true

	ret := cpu.A ^ cpu.operand

	cpu.calcZero(ret)
	cpu.calcSign(ret)

	cpu.A = ret
}

// Increment M
func (cpu *CPU) inc() {
	ret := cpu.operand + 1

	cpu.calcZero(ret)
	cpu.calcSign(ret)

	cpu.mmu.WriteByte(cpu.operAddr, ret)
}

// Increment X
func (cpu *CPU) inx() {
	cpu.X++

	cpu.calcZero(cpu.X)
	cpu.calcSign(cpu.X)
}

// Increment Y
func (cpu *CPU) iny() {
	cpu.Y++

	cpu.calcZero(cpu.Y)
	cpu.calcSign(cpu.Y)
}

// Jump to location
func (cpu *CPU) jmp() {
	cpu.PC = cpu.operAddr
}

// Jump to location, save return address
func (cpu *CPU) jsr() {
	cpu.pushWord(cpu.PC - 1)
	cpu.PC = cpu.operAddr
}

// Load M into A
func (cpu *CPU) lda() {
	cpu.penaltyOp = true

	cpu.A = cpu.operand

	cpu.calcZero(cpu.A)
	cpu.calcSign(cpu.A)
}

// Load M into X
func (cpu *CPU) ldx() {
	cpu.penaltyOp = true

	cpu.X = cpu.operand

	cpu.calcZero(cpu.X)
	cpu.calcSign(cpu.X)
}

// Load M into Y
func (cpu *CPU) ldy() {
	cpu.penaltyOp = true

	cpu.Y = cpu.operand

	cpu.calcZero(cpu.Y)
	cpu.calcSign(cpu.Y)
}

// Shift M right one bit
func (cpu *CPU) lsr() {
	ret := cpu.operand>>1

	if (cpu.operand & 1) > 0 {
		cpu.SetFlag(C)
	} else {
		cpu.ClearFlag(C)
	}

	cpu.calcZero(ret)
	cpu.calcSign(ret)

	cpu.mmu.WriteByte(cpu.operAddr, ret)
}

// Shift A right one bit
func (cpu *CPU) lsrAcc() {
	ret := cpu.operand>>1

	if (cpu.operand & 1) > 0 {
		cpu.SetFlag(C)
	} else {
		cpu.ClearFlag(C)
	}

	cpu.calcZero(ret)
	cpu.calcSign(ret)

	cpu.A = ret
}

// No operation
func (cpu *CPU) nop() {
	switch cpu.opcode {
	case 0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC:
		cpu.penaltyOp = true
	}
}

// Logical OR M with A
func (cpu *CPU) ora() {
	cpu.penaltyOp = true

	ret := cpu.A | cpu.operand

	cpu.calcZero(ret)
	cpu.calcSign(ret)

	cpu.A = ret
}

// Push A onto stack
func (cpu *CPU) pha() {
	cpu.pushByte(cpu.A)
}

// Push S onto stack
func (cpu *CPU) php() {
	cpu.pushByte(cpu.S | B)
}

// Pull A from stack
func (cpu *CPU) pla() {
	cpu.A = cpu.pullByte()

	cpu.calcZero(cpu.A)
	cpu.calcSign(cpu.A)
}

// Pull S from stack
func (cpu *CPU) plp() {
	cpu.S = cpu.pullByte() | 0x20
}

// Rotate M one bit left
func (cpu *CPU) rol() {
	ret := (uint16(cpu.operand)<<1) | uint16(cpu.S & C)

	cpu.calcCarry(ret)

	ret &= 0xFF

	cpu.calcZero(byte(ret))
	cpu.calcSign(byte(ret))

	cpu.mmu.WriteByte(cpu.operAddr, byte(ret))
}

// Rotate A one bit left
func (cpu *CPU) rolAcc() {
	ret := (uint16(cpu.A)<<1) | uint16(cpu.S & C)

	cpu.calcCarry(ret)

	ret &= 0xFF

	cpu.calcZero(byte(ret))
	cpu.calcSign(byte(ret))

	cpu.A = byte(ret)
}

// Rotate M one bit right
func (cpu *CPU) ror() {
	ret := (cpu.operand>>1) | ((cpu.S & C)<<7)

	if (cpu.operand & 1) > 0 {
		cpu.SetFlag(C)
	} else {
		cpu.ClearFlag(C)
	}

	cpu.calcZero(ret)
	cpu.calcSign(ret)

	cpu.mmu.WriteByte(cpu.operAddr, ret)
}

// Rotate A one bit right
func (cpu *CPU) rorAcc() {
	ret := (cpu.A>>1) | ((cpu.S & C)<<7)

	if (cpu.A & 1) > 0 {
		cpu.SetFlag(C)
	} else {
		cpu.ClearFlag(C)
	}

	cpu.calcZero(ret)
	cpu.calcSign(ret)

	cpu.A = ret
}

// Return from interrupt
func (cpu *CPU) rti() {
	cpu.S = cpu.pullByte()
	cpu.PC = cpu.pullWord()
}

// Return from subroutine
func (cpu *CPU) rts() {
	cpu.PC = cpu.pullWord() + 1
}

// Subtract M from A with borrow (Carry)
func (cpu *CPU) sbc() {
	cpu.penaltyOp = true

	cpu.operand ^= 0xFF

	ret := uint16(cpu.A) + uint16(cpu.operand) + uint16(cpu.S & C)

	cpu.calcCarry(ret)
	cpu.calcOverflow(ret)

	ret &= 0xFF

	cpu.calcZero(byte(ret))
	cpu.calcSign(byte(ret))

	cpu.A = byte(ret)
}

// Set C
func (cpu *CPU) sec() {
	cpu.SetFlag(C)
}

// Set D
func (cpu *CPU) sed() {
	cpu.SetFlag(D)
}

// Set I
func (cpu *CPU) sei() {
	cpu.SetFlag(I)
}

// Store A in M
func (cpu *CPU) sta() {
	cpu.mmu.WriteByte(cpu.operAddr, cpu.A)
}

// Store X in M
func (cpu *CPU) stx() {
	cpu.mmu.WriteByte(cpu.operAddr, cpu.X)
}

// Store Y in M
func (cpu *CPU) sty() {
	cpu.mmu.WriteByte(cpu.operAddr, cpu.Y)
}

// Transfer A to X
func (cpu *CPU) tax() {
	cpu.X = cpu.A

	cpu.calcZero(cpu.X)
	cpu.calcSign(cpu.X)
}

// Transfer A to Y
func (cpu *CPU) tay() {
	cpu.Y = cpu.A

	cpu.calcZero(cpu.Y)
	cpu.calcSign(cpu.Y)
}

// Transfer SP to X
func (cpu *CPU) tsx() {
	cpu.X = byte((cpu.SP - 0x100) & 0xFF)

	cpu.calcZero(cpu.X)
	cpu.calcSign(cpu.X)
}

// Transfer X to A
func (cpu *CPU) txa() {
	cpu.A = cpu.X

	cpu.calcZero(cpu.A)
	cpu.calcSign(cpu.A)
}

// Transfer X to SP
func (cpu *CPU) txs() {
	cpu.SP = uint16(cpu.X) + 0x100
}

// Transfer Y to A
func (cpu *CPU) tya() {
	cpu.A = cpu.Y

	cpu.calcZero(cpu.A)
	cpu.calcSign(cpu.A)
}

// Opcode mapping
// ----------------------------------------------------------------------------

func (cpu *CPU) initInsMap() {
	cpu.insMap[0xA9] = func() { // LDA IMM
		cpu.fetchImm()
		cpu.lda()
		cpu.clockIns += 2
	}

	cpu.insMap[0xA5] = func() { // LDA ZP
		cpu.fetchZpg()
		cpu.lda()
		cpu.clockIns += 3
	}

	cpu.insMap[0xB5] = func() { // LDA ZPX
		cpu.fetchZpgX()
		cpu.lda()
		cpu.clockIns += 4
	}

	cpu.insMap[0xAD] = func() { // LDA ABS
		cpu.fetchAbs()
		cpu.lda()
		cpu.clockIns += 4
	}

	cpu.insMap[0xBD] = func() { // LDA ABSX
		cpu.fetchAbsX()
		cpu.lda()
		cpu.clockIns += 4
	}

	cpu.insMap[0xB9] = func() { // LDA ABSY
		cpu.fetchAbsY()
		cpu.lda()
		cpu.clockIns += 4
	}

	cpu.insMap[0xA1] = func() { // LDA INDX
		cpu.fetchIndX()
		cpu.lda()
		cpu.clockIns += 6
	}

	cpu.insMap[0xB1] = func() { // LDA INDY
		cpu.fetchIndY()
		cpu.lda()
		cpu.clockIns += 5
	}

	cpu.insMap[0xA6] = func() { // LDX ZP
		cpu.fetchZpg()
		cpu.ldx()
		cpu.clockIns += 3
	}

	cpu.insMap[0xB6] = func() { // LDX ZPY
		cpu.fetchZpgY()
		cpu.ldx()
		cpu.clockIns += 4
	}

	cpu.insMap[0xAE] = func() { // LDX ABS
		cpu.fetchAbs()
		cpu.ldx()
		cpu.clockIns += 4
	}

	cpu.insMap[0xBE] = func() { // LDX ABSY
		cpu.fetchAbsY()
		cpu.ldx()
		cpu.clockIns += 4
	}

	cpu.insMap[0xA2] = func() { // LDX IMM
		cpu.fetchImm()
		cpu.ldx()
		cpu.clockIns += 2
	}

	cpu.insMap[0xA0] = func() { // LDY IMM
		cpu.fetchImm()
		cpu.ldy()
		cpu.clockIns += 2
	}

	cpu.insMap[0xA4] = func() { // LDY ZP
		cpu.fetchZpg()
		cpu.ldy()
		cpu.clockIns += 3
	}

	cpu.insMap[0xB4] = func() { // LDY ZPX
		cpu.fetchZpgX()
		cpu.ldy()
		cpu.clockIns += 4
	}

	cpu.insMap[0xAC] = func() { // LDY ABS
		cpu.fetchAbs()
		cpu.ldy()
		cpu.clockIns += 4
	}

	cpu.insMap[0xBC] = func() { // LDY ABSX
		cpu.fetchAbsX()
		cpu.ldy()
		cpu.clockIns += 4
	}

	cpu.insMap[0x85] = func() { // STA ZP
		cpu.fetchZpg()
		cpu.sta()
		cpu.clockIns += 3
	}

	cpu.insMap[0x95] = func() { // STA ZPX
		cpu.fetchZpgX()
		cpu.sta()
		cpu.clockIns += 4
	}

	cpu.insMap[0x8D] = func() { // STA ABS
		cpu.fetchAbs()
		cpu.sta()
		cpu.clockIns += 4
	}

	cpu.insMap[0x9D] = func() { // STA ABSX
		cpu.fetchAbsX()
		cpu.sta()
		cpu.clockIns += 5
	}

	cpu.insMap[0x99] = func() { // STA ABSY
		cpu.fetchAbsY()
		cpu.sta()
		cpu.clockIns += 5
	}

	cpu.insMap[0x81] = func() { // STA INDX
		cpu.fetchIndX()
		cpu.sta()
		cpu.clockIns += 6
	}

	cpu.insMap[0x91] = func() { // STA INDY
		cpu.fetchIndY()
		cpu.sta()
		cpu.clockIns += 6
	}

	cpu.insMap[0x86] = func() { // STX ZP
		cpu.fetchZpg()
		cpu.stx()
		cpu.clockIns += 3
	}

	cpu.insMap[0x96] = func() { // STX ZPY
		cpu.fetchZpgY()
		cpu.stx()
		cpu.clockIns += 4
	}

	cpu.insMap[0x8E] = func() { // STX ABS
		cpu.fetchAbs()
		cpu.stx()
		cpu.clockIns += 4
	}

	cpu.insMap[0x84] = func() { // STY ZP
		cpu.fetchZpg()
		cpu.sty()
		cpu.clockIns += 3
	}

	cpu.insMap[0x94] = func() { // STY ZPX
		cpu.fetchZpgX()
		cpu.sty()
		cpu.clockIns += 4
	}

	cpu.insMap[0x8C] = func() { // STY ABS
		cpu.fetchAbs()
		cpu.sty()
		cpu.clockIns += 4
	}

	cpu.insMap[0xAA] = func() { // TAX
		cpu.tax()
		cpu.clockIns += 2
	}

	cpu.insMap[0xA8] = func() { // TAY
		cpu.tay()
		cpu.clockIns += 2
	}

	cpu.insMap[0xBA] = func() { // TSX
		cpu.tsx()
		cpu.clockIns += 2
	}

	cpu.insMap[0x8A] = func() { // TXA
		cpu.txa()
		cpu.clockIns += 2
	}

	cpu.insMap[0x9A] = func() { // TXS
		cpu.txs()
		cpu.clockIns += 2
	}

	cpu.insMap[0x98] = func() { // TYA
		cpu.tya()
		cpu.clockIns += 2
	}

	cpu.insMap[0x69] = func() { // ADC IMM
		cpu.fetchImm()
		cpu.adc()
		cpu.clockIns += 2
	}

	cpu.insMap[0x65] = func() { // ADC ZP
		cpu.fetchZpg()
		cpu.adc()
		cpu.clockIns += 3
	}

	cpu.insMap[0x75] = func() { // ADC ZPX
		cpu.fetchZpgX()
		cpu.adc()
		cpu.clockIns += 4
	}

	cpu.insMap[0x6D] = func() { // ADC ABS
		cpu.fetchAbs()
		cpu.adc()
		cpu.clockIns += 4
	}

	cpu.insMap[0x7D] = func() { // ADC ABSX
		cpu.fetchAbsX()
		cpu.adc()
		cpu.clockIns += 4
	}

	cpu.insMap[0x79] = func() { // ADC ABSY
		cpu.fetchAbsY()
		cpu.adc()
		cpu.clockIns += 4
	}

	cpu.insMap[0x61] = func() { // ADC INDX
		cpu.fetchIndX()
		cpu.adc()
		cpu.clockIns += 6
	}

	cpu.insMap[0x71] = func() { // ADC INDY
		cpu.fetchIndY()
		cpu.adc()
		cpu.clockIns += 6
	}

	cpu.insMap[0xC6] = func() { // DEC zPG
		cpu.fetchZpg()
		cpu.dec()
		cpu.clockIns += 5
	}

	cpu.insMap[0xD6] = func() { // DEC ZPX
		cpu.fetchZpgX()
		cpu.dec()
		cpu.clockIns += 6
	}

	cpu.insMap[0xCE] = func() { // DEC ABS
		cpu.fetchAbs()
		cpu.dec()
		cpu.clockIns += 6
	}

	cpu.insMap[0xDE] = func() { // DEC ABSX
		cpu.fetchAbsX()
		cpu.dec()
		cpu.clockIns += 7
	}

	cpu.insMap[0xCA] = func() { // DEX
		cpu.dex()
		cpu.clockIns += 2
	}

	cpu.insMap[0x88] = func() { // DEY
		cpu.dey()
		cpu.clockIns += 2
	}

	cpu.insMap[0xE6] = func() { // INC ZP
		cpu.fetchZpg()
		cpu.inc()
		cpu.clockIns += 5
	}

	cpu.insMap[0xF6] = func() { // INC ZPX
		cpu.fetchZpgX()
		cpu.inc()
		cpu.clockIns += 6
	}

	cpu.insMap[0xEE] = func() { // INC ABS
		cpu.fetchAbs()
		cpu.inc()
		cpu.clockIns += 6
	}

	cpu.insMap[0xFE] = func() { // INC ABSX
		cpu.fetchAbsX()
		cpu.inc()
		cpu.clockIns += 7
	}

	cpu.insMap[0xE8] = func() { // INX
		cpu.inx()
		cpu.clockIns += 2
	}

	cpu.insMap[0xC8] = func() { // INY
		cpu.iny()
		cpu.clockIns += 2
	}

	cpu.insMap[0xE9] = func() { // SBC IMM
		cpu.fetchImm()
		cpu.sbc()
		cpu.clockIns += 2
	}

	cpu.insMap[0xE5] = func() { // SBC ZP
		cpu.fetchZpg()
		cpu.sbc()
		cpu.clockIns += 3
	}

	cpu.insMap[0xF5] = func() { // SBC ZPX
		cpu.fetchZpgX()
		cpu.sbc()
		cpu.clockIns += 4
	}

	cpu.insMap[0xED] = func() { // SBC ABS
		cpu.fetchAbs()
		cpu.sbc()
		cpu.clockIns += 4
	}

	cpu.insMap[0xFD] = func() { // SBC ABSX
		cpu.fetchAbsX()
		cpu.sbc()
		cpu.clockIns += 4
	}

	cpu.insMap[0xF9] = func() { // SBC ABSY
		cpu.fetchAbsY()
		cpu.sbc()
		cpu.clockIns += 4
	}

	cpu.insMap[0xE1] = func() { // SBC INDX
		cpu.fetchIndX()
		cpu.sbc()
		cpu.clockIns += 6
	}

	cpu.insMap[0xF1] = func() { // SBC INDY
		cpu.fetchIndY()
		cpu.sbc()
		cpu.clockIns += 5
	}

	cpu.insMap[0x29] = func() { // AND IMM
		cpu.fetchImm()
		cpu.and()
		cpu.clockIns += 2
	}

	cpu.insMap[0x25] = func() { // AND ZP
		cpu.fetchZpg()
		cpu.and()
		cpu.clockIns += 3
	}

	cpu.insMap[0x35] = func() { // AND ZPX
		cpu.fetchZpgX()
		cpu.and()
		cpu.clockIns += 4
	}

	cpu.insMap[0x2D] = func() { // AND ABS
		cpu.fetchAbs()
		cpu.and()
		cpu.clockIns += 4
	}

	cpu.insMap[0x3D] = func() { // AND ABSX
		cpu.fetchAbsX()
		cpu.and()
		cpu.clockIns += 4
	}

	cpu.insMap[0x39] = func() { // AND ABSY
		cpu.fetchAbsY()
		cpu.and()
		cpu.clockIns += 4
	}

	cpu.insMap[0x21] = func() { // AND INDX
		cpu.fetchIndX()
		cpu.and()
		cpu.clockIns += 6
	}

	cpu.insMap[0x31] = func() { // AND INDY
		cpu.fetchIndY()
		cpu.and()
		cpu.clockIns += 5
	}

	cpu.insMap[0x0A] = func() { // ASL ACC
		cpu.aslAcc()
		cpu.clockIns += 2
	}

	cpu.insMap[0x06] = func() { // ASL ZP
		cpu.fetchZpg()
		cpu.asl()
		cpu.clockIns += 5
	}

	cpu.insMap[0x16] = func() { // ASL ZPX
		cpu.fetchZpgX()
		cpu.asl()
		cpu.clockIns += 6
	}

	cpu.insMap[0x0E] = func() { // ASL ABS
		cpu.fetchAbs()
		cpu.asl()
		cpu.clockIns += 6
	}

	cpu.insMap[0x1E] = func() { // ASL ABSX
		cpu.fetchAbsX()
		cpu.asl()
		cpu.clockIns += 7
	}

	cpu.insMap[0x24] = func() { // BIT ZP
		cpu.fetchZpg()
		cpu.bit()
		cpu.clockIns += 3
	}

	cpu.insMap[0x2C] = func() { // BIT ABS
		cpu.fetchAbs()
		cpu.bit()
		cpu.clockIns += 4
	}

	cpu.insMap[0x49] = func() { // EOR IMM
		cpu.fetchImm()
		cpu.eor()
		cpu.clockIns += 2
	}

	cpu.insMap[0x45] = func() { // EOR ZP
		cpu.fetchZpg()
		cpu.eor()
		cpu.clockIns += 3
	}

	cpu.insMap[0x55] = func() { // EOR ZPX
		cpu.fetchZpgX()
		cpu.eor()
		cpu.clockIns += 4
	}

	cpu.insMap[0x4D] = func() { // EOR ABS
		cpu.fetchAbs()
		cpu.eor()
		cpu.clockIns += 4
	}

	cpu.insMap[0x5D] = func() { // EOR ABSX
		cpu.fetchAbsX()
		cpu.eor()
		cpu.clockIns += 4
	}

	cpu.insMap[0x59] = func() { // EOR ABSY
		cpu.fetchAbsY()
		cpu.eor()
		cpu.clockIns += 4
	}

	cpu.insMap[0x41] = func() { // EOR INDX
		cpu.fetchIndX()
		cpu.eor()
		cpu.clockIns += 6
	}

	cpu.insMap[0x51] = func() { // EOR INDY
		cpu.fetchIndY()
		cpu.eor()
		cpu.clockIns += 5
	}

	cpu.insMap[0x4A] = func() { // LSR ACC
		cpu.lsrAcc()
		cpu.clockIns += 2
	}

	cpu.insMap[0x46] = func() { // LSR ZP
		cpu.fetchZpg()
		cpu.lsr()
		cpu.clockIns += 5
	}

	cpu.insMap[0x56] = func() { // LSR ZPX
		cpu.fetchZpgX()
		cpu.lsr()
		cpu.clockIns += 6
	}

	cpu.insMap[0x4E] = func() { // LSR ABS
		cpu.fetchAbs()
		cpu.lsr()
		cpu.clockIns += 6
	}

	cpu.insMap[0x5E] = func() { // LSR ABSX
		cpu.fetchAbsX()
		cpu.lsr()
		cpu.clockIns += 7
	}

	cpu.insMap[0x09] = func() { // ORA IMM
		cpu.fetchImm()
		cpu.ora()
		cpu.clockIns += 2
	}

	cpu.insMap[0x05] = func() { // ORA ZP
		cpu.fetchZpg()
		cpu.ora()
		cpu.clockIns += 3
	}

	cpu.insMap[0x15] = func() { // ORA ZPX
		cpu.fetchZpgX()
		cpu.ora()
		cpu.clockIns += 4
	}

	cpu.insMap[0x0D] = func() { // ORA ABS
		cpu.fetchAbs()
		cpu.ora()
		cpu.clockIns += 4
	}

	cpu.insMap[0x1D] = func() { // ORA ABSX
		cpu.fetchAbsX()
		cpu.ora()
		cpu.clockIns += 4
	}

	cpu.insMap[0x19] = func() { // ORA ABSY
		cpu.fetchAbsY()
		cpu.ora()
		cpu.clockIns += 4
	}

	cpu.insMap[0x01] = func() { // ORA INDX
		cpu.fetchIndX()
		cpu.ora()
		cpu.clockIns += 6
	}

	cpu.insMap[0x11] = func() { // ORA INDY
		cpu.fetchIndY()
		cpu.ora()
		cpu.clockIns += 5
	}

	cpu.insMap[0x2A] = func() { // ROL ACC
		cpu.rolAcc()
		cpu.clockIns += 2
	}

	cpu.insMap[0x26] = func() { // ROL ZP
		cpu.fetchZpg()
		cpu.rol()
		cpu.clockIns += 5
	}

	cpu.insMap[0x36] = func() { // ROL ZPX
		cpu.fetchZpgX()
		cpu.rol()
		cpu.clockIns += 6
	}

	cpu.insMap[0x2E] = func() { // ROL ABS
		cpu.fetchAbs()
		cpu.rol()
		cpu.clockIns += 6
	}

	cpu.insMap[0x3E] = func() { // ROL ABSX
		cpu.fetchAbsX()
		cpu.rol()
		cpu.clockIns += 7
	}

	cpu.insMap[0x6A] = func() { // ROR ACC
		cpu.rorAcc()
		cpu.clockIns += 2
	}

	cpu.insMap[0x66] = func() { // ROR ZP
		cpu.fetchZpg()
		cpu.ror()
		cpu.clockIns += 5
	}

	cpu.insMap[0x76] = func() { // ROR ZPX
		cpu.fetchZpgX()
		cpu.ror()
		cpu.clockIns += 6
	}

	cpu.insMap[0x6E] = func() { // ROR ABS
		cpu.fetchAbs()
		cpu.ror()
		cpu.clockIns += 6
	}

	cpu.insMap[0x7E] = func() { // ROR ABSX
		cpu.fetchAbsX()
		cpu.ror()
		cpu.clockIns += 7
	}

	cpu.insMap[0x90] = func() { // BCC
		cpu.bcc()
		cpu.clockIns += 2
	}

	cpu.insMap[0xB0] = func() { // BCS
		cpu.bcs()
		cpu.clockIns += 2
	}

	cpu.insMap[0xD0] = func() { // BNE
		cpu.bne()
		cpu.clockIns += 2
	}

	cpu.insMap[0xF0] = func() { // BEQ
		cpu.beq()
		cpu.clockIns += 2
	}

	cpu.insMap[0x10] = func() { // BPL
		cpu.bpl()
		cpu.clockIns += 2
	}

	cpu.insMap[0x30] = func() { // BMI
		cpu.bmi()
		cpu.clockIns += 2
	}

	cpu.insMap[0x50] = func() { // BVC
		cpu.bvc()
		cpu.clockIns += 2
	}

	cpu.insMap[0x70] = func() { // BVS
		cpu.bvs()
		cpu.clockIns += 2
	}

	cpu.insMap[0x6C] = func() { // JMP IND
		cpu.fetchInd()
		cpu.jmp()
		cpu.clockIns += 5
	}

	cpu.insMap[0x4C] = func() { // JMP ABS
		cpu.fetchAbs()
		cpu.jmp()
		cpu.clockIns += 3
	}

	cpu.insMap[0x20] = func() { // JSR
		cpu.jsr()
		cpu.clockIns += 6
	}

	cpu.insMap[0x40] = func() { // RTI
		cpu.rti()
		cpu.clockIns += 6
	}

	cpu.insMap[0x60] = func() { // RTS
		cpu.rts()
		cpu.clockIns += 6
	}

	cpu.insMap[0x18] = func() { // CLC
		cpu.clc()
		cpu.clockIns += 2
	}

	cpu.insMap[0xD8] = func() { // CLD
		cpu.cld()
		cpu.clockIns += 2
	}

	cpu.insMap[0x58] = func() { // CLI
		cpu.cli()
		cpu.clockIns += 2
	}

	cpu.insMap[0xB8] = func() { // CLV
		cpu.clv()
		cpu.clockIns += 2
	}

	cpu.insMap[0xC9] = func() { // CMP IMM
		cpu.fetchImm()
		cpu.cmp()
		cpu.clockIns += 2
	}

	cpu.insMap[0xC5] = func() { // CMP ZP
		cpu.fetchZpg()
		cpu.cmp()
		cpu.clockIns += 3
	}

	cpu.insMap[0xD5] = func() { // CMP ZPX
		cpu.fetchZpgX()
		cpu.cmp()
		cpu.clockIns += 4
	}

	cpu.insMap[0xCD] = func() { // CMP ABS
		cpu.fetchAbs()
		cpu.cmp()
		cpu.clockIns += 4
	}

	cpu.insMap[0xDD] = func() { // CMP ABSX
		cpu.fetchAbsX()
		cpu.cmp()
		cpu.clockIns += 4
	}

	cpu.insMap[0xD9] = func() { // CMP ABSY
		cpu.fetchAbsY()
		cpu.cmp()
		cpu.clockIns += 4
	}

	cpu.insMap[0xC1] = func() { // CMP INDX
		cpu.fetchIndX()
		cpu.cmp()
		cpu.clockIns += 6
	}

	cpu.insMap[0xD1] = func() { // CMP INDY
		cpu.fetchIndY()
		cpu.cmp()
		cpu.clockIns += 5
	}

	cpu.insMap[0xE0] = func() { // CPX IMM
		cpu.fetchImm()
		cpu.cpx()
		cpu.clockIns += 2
	}

	cpu.insMap[0xE4] = func() { // CPX ZP
		cpu.fetchZpg()
		cpu.cpx()
		cpu.clockIns += 3
	}

	cpu.insMap[0xEC] = func() { // CPX ABS
		cpu.fetchAbs()
		cpu.cpx()
		cpu.clockIns += 4
	}

	cpu.insMap[0xC0] = func() { // CPY IMM
		cpu.fetchImm()
		cpu.cpy()
		cpu.clockIns += 2
	}

	cpu.insMap[0xC4] = func() { // CPY ZP
		cpu.fetchZpg()
		cpu.cpy()
		cpu.clockIns += 3
	}

	cpu.insMap[0xCC] = func() { // CPY ABS
		cpu.fetchAbs()
		cpu.cpy()
		cpu.clockIns += 4
	}

	cpu.insMap[0x38] = func() { // SEC
		cpu.sec()
		cpu.clockIns += 2
	}

	cpu.insMap[0xF8] = func() { // SED
		cpu.sed()
		cpu.clockIns += 2
	}

	cpu.insMap[0x78] = func() { // SEI
		cpu.sei()
		cpu.clockIns += 2
	}

	cpu.insMap[0x48] = func() { // PHA
		cpu.pha()
		cpu.clockIns += 3
	}

	cpu.insMap[0x08] = func() { // PHP
		cpu.php()
		cpu.clockIns += 3
	}

	cpu.insMap[0x68] = func() { // PLA
		cpu.pla()
		cpu.clockIns += 4
	}

	cpu.insMap[0x28] = func() { // PLP
		cpu.plp()
		cpu.clockIns += 4
	}

	cpu.insMap[0x00] = func() { // BRK
		cpu.brk()
		cpu.clockIns += 7
	}

	cpu.insMap[0xEA] = func() { // NOP
		cpu.nop()
		cpu.clockIns += 2
	}
}
