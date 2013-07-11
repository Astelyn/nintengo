package mmu

type MMU [0x10000]uint16

func NewMMU() *MMU {
	mmu := new(MMU)
	mmu.Init()
	return mmu
}

func (mmu *MMU) Init() {
	for i, _ := range mmu {
		mmu[i] = 0
	}
}

func (mmu *MMU) ReadByte(addr uint16) byte {
	return mmu[addr]
}

func (mmu *MMU) ReadWord(addr uint16) uint16 {
	return uint16(mmu.ReadByte(addr)) |
		(uint16(mmu.ReadByte(addr+1)) << 8)
}

func (mmu *MMU) WriteByte(addr uint16, val byte) {
	mmu[addr] = val
}

func (mmu *MMU) WriteWord(addr, val uint16) {
	mmu.WriteByte(addr, byte(val&0xFF))
	mmu.WriteByte(addr+1, byte((val>>8)&0xFF))
}

