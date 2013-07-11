package mmu

type MMU struct {

}

func (mmu *MMU) ReadByte(addr uint16) byte {
	// TODO: implement
	return 0x00
}

func (mmu *MMU) ReadWord(addr uint16) uint16 {
	// TODO: implement
	return 0x0000
}

func (mmu *MMU) WriteByte(addr uint16, val byte) {
	// TODO: implement
}

func (mmu *MMU) WriteWord(addr, val uint16) {
	// TODO: implement
}
