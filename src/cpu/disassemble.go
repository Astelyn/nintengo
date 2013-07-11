package cpu

import (
	"fmt"
)

func (cpu *CPU) disassemble() {
	fmt.Printf("%X  %X  ", cpu.PC - 1, cpu.opcode)

	switch cpu.opcode {
	case 0xA9: // LDA IMM
		fmt.Printf("LDA #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0xA5: // LDA ZP
		fmt.Printf("LDA $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0xB5: // LDA ZPX
		fmt.Printf("LDA $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0xAD: // LDA ABS
		fmt.Printf("LDA $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0xBD: // LDA ABSX
		fmt.Printf("LDA $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0xB9: // LDA ABSY
		fmt.Printf("LDA $%X, Y   ", cpu.mmu.ReadWord(cpu.PC))
	case 0xA1: // LDA INDX
		fmt.Printf("LDA ($%X, X)   ", cpu.mmu.ReadByte(cpu.PC))
	case 0xB1: // LDA INDY
		fmt.Printf("LDA ($%X), Y   ", cpu.mmu.ReadByte(cpu.PC))
	case 0xA6: // LDX ZP
		fmt.Printf("LDX $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0xB6: // LDX ZPY
		fmt.Printf("LDX $%X, Y     ", cpu.mmu.ReadByte(cpu.PC))
	case 0xAE: // LDX ABS
		fmt.Printf("LDX $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0xBE: // LDX ABSY
		fmt.Printf("LDX $%X, Y   ", cpu.mmu.ReadWord(cpu.PC))
	case 0xA2: // LDX IMM
		fmt.Printf("LDX #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0xA0: // LDY IMM
		fmt.Printf("LDY #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0xA4: // LDY ZP
		fmt.Printf("LDY $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0xB4: // LDY ZPX
		fmt.Printf("LDY $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0xAC: // LDY ABS
		fmt.Printf("LDY $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0xBC: // LDY ABSX
		fmt.Printf("LDY $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x85: // STA ZP
		fmt.Printf("STA $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x95: // STA ZPX
		fmt.Printf("STA $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x8D: // STA ABS
		fmt.Printf("STA $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x9D: // STA ABSX
		fmt.Printf("STA $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x99: // STA ABSY
		fmt.Printf("STA $%X, Y   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x81: // STA INDX
		fmt.Printf("STA ($%X, X)   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x91: // STA INDY
		fmt.Printf("STA ($%X), Y   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x86: // STX ZP
		fmt.Printf("STX $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x96: // STX ZPY
		fmt.Printf("STX $%X, Y     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x8E: // STX ABS
		fmt.Printf("STX $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x84: // STY ZP
		fmt.Printf("STY $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x94: // STY ZPX
		fmt.Printf("STY $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x8C: // STY ABS
		fmt.Printf("STY $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0xAA: // TAX
		fmt.Printf("TAX            ")
	case 0xA8: // TAY
		fmt.Printf("TAY            ")
	case 0xBA: // TSX
		fmt.Printf("TSX            ")
	case 0x8A: // TXA
		fmt.Printf("TXA            ")
	case 0x9A: // TXS
		fmt.Printf("TXS            ")
	case 0x98: // TYA
		fmt.Printf("TYA            ")
	case 0x69: // ADC IMM
		fmt.Printf("ADC #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0x65: // ADC ZP
		fmt.Printf("ADC $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x75: // ADC ZPX
		fmt.Printf("ADC $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x6D: // ADC ABS
		fmt.Printf("ADC $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x7D: // ADC ABSX
		fmt.Printf("ADC $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x79: // ADC ABSY
		fmt.Printf("ADC $%X, Y   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x61: // ADC INDX
		fmt.Printf("ADC ($%X, X)   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x71: // ADC INDY
		fmt.Printf("ADC ($%X), Y   ", cpu.mmu.ReadByte(cpu.PC))
	case 0xC6: // DEC zPG
		fmt.Printf("DEC $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0xD6: // DEC ZPX
		fmt.Printf("DEC $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0xCE: // DEC ABS
		fmt.Printf("DEC $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0xDE: // DEC ABSX
		fmt.Printf("DEC $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0xCA: // DEX
		fmt.Printf("DEX            ")
	case 0x88: // DEY
		fmt.Printf("DEY            ")
	case 0xE6: // INC ZP
		fmt.Printf("INC $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0xF6: // INC ZPX
		fmt.Printf("INC $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0xEE: // INC ABS
		fmt.Printf("INC $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0xFE: // INC ABSX
		fmt.Printf("INC $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0xE8: // INX
		fmt.Printf("INX            ")
	case 0xC8: // INY
		fmt.Printf("INY            ")
	case 0xE9: // SBC IMM
		fmt.Printf("SBC #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0xE5: // SBC ZP
		fmt.Printf("SBC $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0xF5: // SBC ZPX
		fmt.Printf("SBC $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0xED: // SBC ABS
		fmt.Printf("SBC $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0xFD: // SBC ABSX
		fmt.Printf("SBC $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0xF9: // SBC ABSY
		fmt.Printf("SBC $%X, Y   ", cpu.mmu.ReadWord(cpu.PC))
	case 0xE1: // SBC INDX
		fmt.Printf("SBC ($%X, X)   ", cpu.mmu.ReadByte(cpu.PC))
	case 0xF1: // SBC INDY
		fmt.Printf("SBC ($%X), Y   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x29: // AND IMM
		fmt.Printf("AND #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0x25: // AND ZP
		fmt.Printf("AND $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x35: // AND ZPX
		fmt.Printf("AND $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x2D: // AND ABS
		fmt.Printf("AND $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x3D: // AND ABSX
		fmt.Printf("AND $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x39: // AND ABSY
		fmt.Printf("AND $%X, Y   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x21: // AND INDX
		fmt.Printf("AND ($%X, X)   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x31: // AND INDY
		fmt.Printf("AND ($%X), Y   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x0A: // ASL ACC
		fmt.Printf("ASL            ")
	case 0x06: // ASL ZP
		fmt.Printf("ASL $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x16: // ASL ZPX
		fmt.Printf("ASL $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x0E: // ASL ABS
		fmt.Printf("ASL $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x1E: // ASL ABSX
		fmt.Printf("ASL $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x24: // BIT ZP
		fmt.Printf("BIT $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x2C: // BIT ABS
		fmt.Printf("BIT $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x49: // EOR IMM
		fmt.Printf("EOR #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0x45: // EOR ZP
		fmt.Printf("EOR $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x55: // EOR ZPX
		fmt.Printf("EOR $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x4D: // EOR ABS
		fmt.Printf("EOR $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x5D: // EOR ABSX
		fmt.Printf("EOR $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x59: // EOR ABSY
		fmt.Printf("EOR $%X, Y   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x41: // EOR INDX
		fmt.Printf("EOR ($%X, X)   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x51: // EOR INDY
		fmt.Printf("EOR ($%X), Y   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x4A: // LSR ACC
		fmt.Printf("LSR            ")
	case 0x46: // LSR ZP
		fmt.Printf("LSR $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x56: // LSR ZPX
		fmt.Printf("LSR $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x4E: // LSR ABS
		fmt.Printf("LSR $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x5E: // LSR ABSX
		fmt.Printf("LSR $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x09: // ORA IMM
		fmt.Printf("ORA #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0x05: // ORA ZP
		fmt.Printf("ORA $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x15: // ORA ZPX
		fmt.Printf("ORA $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x0D: // ORA ABS
		fmt.Printf("ORA $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x1D: // ORA ABSX
		fmt.Printf("ORA $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x19: // ORA ABSY
		fmt.Printf("ORA $%X, Y   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x01: // ORA INDX
		fmt.Printf("ORA ($%X, X)   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x11: // ORA INDY
		fmt.Printf("ORA ($%X), Y   ", cpu.mmu.ReadByte(cpu.PC))
	case 0x2A: // ROL ACC
		fmt.Printf("ROL            ")
	case 0x26: // ROL ZP
		fmt.Printf("ROL $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x36: // ROL ZPX
		fmt.Printf("ROL $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x2E: // ROL ABS
		fmt.Printf("ROL $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x3E: // ROL ABSX
		fmt.Printf("ROL $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x6A: // ROR ACC
		fmt.Printf("ROR            ")
	case 0x66: // ROR ZP
		fmt.Printf("ROR $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0x76: // ROR ZPX
		fmt.Printf("ROR $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0x6E: // ROR ABS
		fmt.Printf("ROR $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x7E: // ROR ABSX
		fmt.Printf("ROR $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0x90: // BCC
		fmt.Printf("BCC            ")
	case 0xB0: // BCS
		fmt.Printf("BCS            ")
	case 0xD0: // BNE
		fmt.Printf("BNE            ")
	case 0xF0: // BEQ
		fmt.Printf("BEQ            ")
	case 0x10: // BPL
		fmt.Printf("BPL            ")
	case 0x30: // BMI
		fmt.Printf("BMI            ")
	case 0x50: // BVC
		fmt.Printf("BVC            ")
	case 0x70: // BVS
		fmt.Printf("BVS            ")
	case 0x6C: // JMP IND
		fmt.Printf("JMP ($%X)", cpu.mmu.ReadWord(cpu.PC))
	case 0x4C: // JMP ABS
		fmt.Printf("JMP $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x20: // JSR
		fmt.Printf("JSR            ")
	case 0x40: // RTI
		fmt.Printf("RTI            ")
	case 0x60: // RTS
		fmt.Printf("RTS            ")
	case 0x18: // CLC
		fmt.Printf("CLC            ")
	case 0xD8: // CLD
		fmt.Printf("CLD            ")
	case 0x58: // CLI
		fmt.Printf("CLI            ")
	case 0xB8: // CLV
		fmt.Printf("CLV            ")
	case 0xC9: // CMP IMM
		fmt.Printf("CMP #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0xC5: // CMP ZP
		fmt.Printf("CMP $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0xD5: // CMP ZPX
		fmt.Printf("CMP $%X, X     ", cpu.mmu.ReadByte(cpu.PC))
	case 0xCD: // CMP ABS
		fmt.Printf("CMP $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0xDD: // CMP ABSX
		fmt.Printf("CMP $%X, X   ", cpu.mmu.ReadWord(cpu.PC))
	case 0xD9: // CMP ABSY
		fmt.Printf("CMP $%X, Y   ", cpu.mmu.ReadWord(cpu.PC))
	case 0xC1: // CMP INDX
		fmt.Printf("CMP ($%X, X)   ", cpu.mmu.ReadByte(cpu.PC))
	case 0xD1: // CMP INDY
		fmt.Printf("CMP ($%X), Y   ", cpu.mmu.ReadByte(cpu.PC))
	case 0xE0: // CPX IMM
		fmt.Printf("CPX #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0xE4: // CPX ZP
		fmt.Printf("CPX $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0xEC: // CPX ABS
		fmt.Printf("CPX $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0xC0: // CPY IMM
		fmt.Printf("CPY #$%X       ", cpu.mmu.ReadByte(cpu.PC))
	case 0xC4: // CPY ZP
		fmt.Printf("CPY $%X        ", cpu.mmu.ReadByte(cpu.PC))
	case 0xCC: // CPY ABS
		fmt.Printf("CPY $%X      ", cpu.mmu.ReadWord(cpu.PC))
	case 0x38: // SEC
		fmt.Printf("SEC            ")
	case 0xF8: // SED
		fmt.Printf("SED            ")
	case 0x78: // SEI
		fmt.Printf("SEI            ")
	case 0x48: // PHA
		fmt.Printf("PHA            ")
	case 0x08: // PHP
		fmt.Printf("PHP            ")
	case 0x68: // PLA
		fmt.Printf("PLA            ")
	case 0x28: // PLP
		fmt.Printf("PLP            ")
	case 0x00: // BRK
		fmt.Printf("BRK            ")
	case 0xEA: // NOP
		fmt.Printf("NOP            ")
	}

	fmt.Printf("A:%X X:%X Y:%X S:%X SP:%X",
		cpu.A, cpu.X, cpu.Y, cpu.S, cpu.SP - 0x100)
}
