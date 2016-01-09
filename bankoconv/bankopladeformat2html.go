package main

import (
	"bytes"
	"flag"
	"log"
	"os"
	"html/template"
)

type BankoPlade [3][9]uint8
type BankoPlader []BankoPlade

const bankoPladeSize = 82
const ASCII0 = 48
const BANKOROWS = 3
const BANKOCOLS = 9

func parseBankoPladeFormat(filename string) BankoPlader {
	f, err := os.Open(filename)

	if err != nil {
		log.Fatal(err)
	}

	var plader BankoPlader
	plade_b := make([]byte, bankoPladeSize)

	p := 0

	for {
		read, err := f.Read(plade_b)
		if err != nil && read != bankoPladeSize {
			break
		}
		if plade_b[bankoPladeSize-2] != byte('\n') || plade_b[bankoPladeSize-1] != byte('\n') {
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
			if y >= BANKOROWS {
				break
			}
			line_s := bytes.Split(l, []byte(" "))
			for x, b := range line_s {
				plade[y][x] = (b[0]-ASCII0)*10 + (b[1] - ASCII0)
			}
		}
		plader = append(plader, plade)
		p++
	}

	return plader
}

func main() {
	flag.Parse()
	if len(flag.Args()) != 2 {
		log.Fatal("To filer, tak!")
	}
	filename := flag.Arg(0)
	plader := parseBankoPladeFormat(filename)

	htmlfilename := flag.Arg(1)

	out := bytes.NewBufferString("")
	t, err := template.New("html").Parse(`<!doctype html>
<html>
<head>
<style>
body {
	font-family: sans-serif;
}
.plade {
	width: 900px;
	padding: 25px;
	margin: 0 auto;
	font-size: 27pt;
	margin-bottom: 50px;
	border: 1px #000 dashed;
}
.plade table {
	background: #000;
}
.plade table td {
	background: #fff;
	height: 100px;
	width: 100px;
	vertical-align: middle;
	text-align: center;
}
</style>
</head>
<body>
{{range $i, $e := .Plader}}<div class="plade">
<header>#{{$i}}</header>
<table>
{{range $l := $e}}<tr>{{range $f := $l}}<td{{if eq $f 0}} class="empty"{{end}}>{{if gt $f 0}}{{$f}}{{end}}</td>{{end}}</tr>{{end}}
</table>
</div>
{{end}}
</body>
</html>`)
	if err != nil {
		log.Fatal(err)
	}
	err = t.Execute(out, struct {
		Plader BankoPlader
	}{
		plader,
	})
	if err != nil {
		log.Fatal(err)
	}
	
	f, err := os.Create(htmlfilename)
	if err != nil {
		log.Fatal(err)
	}
	
	_, err = f.Write(out.Bytes())
	if err != nil {
		log.Fatal(err)
	}
}

