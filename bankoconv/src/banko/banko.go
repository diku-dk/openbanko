package banko

import (
	"bytes"
	"io"
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

func ParseBankoPladeFormat(filename string) BankoPlader {
	var f io.Reader
	if filename == "" {
		f = os.Stdin
	} else {
		log.Fatal("Ikke implementeret")
	}

	var plader BankoPlader
	plade_b := make([]byte, bankoPladeByteSize)

	p := 0

	for {
		read, err := f.Read(plade_b)
		if err != nil && read != bankoPladeByteSize {
			break
		}
		if plade_b[bankoPladeByteSize-2] != byte('\n') || plade_b[bankoPladeByteSize-1] != byte('\n') {
			log.Fatal("Slet format: Plade #", p, "mangler afsluttende linjeskift!")
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
		plader = append(plader, plade)
		p++
	}

	return plader
}

