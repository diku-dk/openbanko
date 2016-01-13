package banko

import (
	"bytes"
	"log"
	"os"
)

type BankoPlade [3][9]uint8
type BankoPlader []BankoPlade

const (
	bankoPladeByteSize = 82
	ascii0             = 48
	bankoRows          = 3
	bankoCols          = 9
)

type BankoPladeReader struct {
	f       *os.File
	p       int
	current BankoPlade
}

func NewBankoPladeReader() BankoPladeReader {
	return BankoPladeReader{
		os.Stdin,
		0,
		BankoPlade{},
	}
}

func (r *BankoPladeReader) Value() BankoPlade {
	return r.current
}

func (r *BankoPladeReader) RecordNo() int {
	return r.p
}

func (r *BankoPladeReader) IsLast() bool {
	if _, err := r.f.Seek(1, 1); err != nil {
		return true
	}
	r.f.Seek(-1, 1)
	return false
}

func (r *BankoPladeReader) Next() bool {
	plade_b := make([]byte, bankoPladeByteSize)
	read, err := r.f.Read(plade_b)
	if err != nil && read != bankoPladeByteSize {
		return false
	}
	if plade_b[bankoPladeByteSize-2] != byte('\n') || plade_b[bankoPladeByteSize-1] != byte('\n') {
		log.Fatal("Slet format: Plade #", r.p, "mangler afsluttende linjeskift!")
	}
	var plade BankoPlade
	plade = [3][9]uint8{
		[9]uint8{0, 0, 0, 0, 0, 0, 0, 0, 0},
		[9]uint8{0, 0, 0, 0, 0, 0, 0, 0, 0},
		[9]uint8{0, 0, 0, 0, 0, 0, 0, 0, 0},
	}
	plade_s := bytes.Split(plade_b, []byte("\n"))
	for y, l := range plade_s {
		if y >= bankoRows {
			break
		}
		line_s := bytes.Split(l, []byte(" "))
		for x, b := range line_s {
			plade[y][x] = (b[0]-ascii0)*10 + (b[1] - ascii0)
		}
	}
	r.p++
	r.current = plade
	return true
}

func ParseBankoPladeFormat(filename string) BankoPlader {
	var r BankoPladeReader
	if filename == "" {
		r = NewBankoPladeReader()
	} else {
		log.Fatal("Ikke implementeret")
	}

	var plader BankoPlader

	for r.Next() {
		plader = append(plader, r.Value())
	}

	return plader
}

