package main

import (
	"banko"
	"bytes"
	"html/template"
	"log"
	"os"
)

func main() {
	plader := banko.ParseBankoPladeFormat("")

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
		Plader banko.BankoPlader
	}{
		plader,
	})
	if err != nil {
		log.Fatal(err)
	}

	os.Stdout.Write(out.Bytes())
}

