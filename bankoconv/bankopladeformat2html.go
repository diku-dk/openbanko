package main

import (
	"banko"
	"bytes"
	"html/template"
	"log"
	"os"
)

func main() {
	reader := banko.NewBankoPladeReader()

	os.Stdout.Write([]byte(`<!doctype html>
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
<body>`))
	rowt, err := template.New("row").Parse(`<div class="plade">
<header>#{{.Num}}</header>
<table>
{{range $l := .Plade}}<tr>{{range $f := $l}}<td{{if eq $f 0}} class="empty"{{end}}>{{if gt $f 0}}{{$f}}{{end}}</td>{{end}}</tr>{{end}}
</table>
</div>
`)
	if err != nil {
		log.Fatal(err)
	}
	for reader.Next() {
		html := bytes.NewBufferString("")
		err := rowt.Execute(html, struct {
			Plade banko.BankoPlade
			Num   int
		}{
			reader.Value(),
			reader.RecordNo(),
		})
		if err != nil {
			continue
		}
		os.Stdout.Write(html.Bytes())
	}
	os.Stdout.Write([]byte(`</body>
</html>`))
}

