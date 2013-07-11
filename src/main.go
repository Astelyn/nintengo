// I have no idea what I'm doing with this yet

package main

import (
	"cpu"
	"mmu"
	"rom"
)

type NES struct {
	cpu *cpu.CPU
	mmu *mmu.MMU
	rom *rom.ROM
	// Other stuff
}

func NewNES() *NES {
	nes := new(NES)

	// MMU here
	nes.cpu = NewCPU()
	nes.cpu.ConnectMMU(nes.mmu)

	return nes
}

func (nes *NES) Reset() {
	nes.cpu.Reset()
}

func main() {
	var nes *NES = NewNES()

	// Load ROM
}
