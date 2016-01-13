package main

import (
	"banko"
	"bytes"
	"log"
	"os"
	"text/template"
)

func main() {
	reader := banko.NewBankoPladeReader()

	os.Stdout.Write([]byte(`\documentclass{article}
\usepackage[a4paper,landscape,margin=12mm]{geometry}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[danish]{babel}
\usepackage{microtype}
\usepackage{palatino}
\usepackage{fancyhdr}
\usepackage{array}
\pagestyle{empty}
\setlength{\extrarowheight}{18mm}
\begin{document}

\fontsize{19mm}{0mm}\selectfont
\begin{center}
`))

	funcMap := template.FuncMap{
		"odd":  func(i int) bool { return i%2 != 0 },
		"even": func(i int) bool { return i%2 == 0 },
	}

	rowt, err := template.New("row").Funcs(funcMap).Parse(`{{if even .Num}}\topskip0pt
\vspace*{\fill}{{end}}{{if odd .Num}}\vspace{16mm}{{end}}
\begin{tabular}{|r|r|r|r|r|r|r|r|r|}
\hline
{{range $row := .Plade}}{{range $ii, $f := $row}}{{if gt $ii 0}} & {{end}}\begin{minipage}[t]{20mm}\begin{center}{{if gt $f 0}}{{$f}}{{end}}\end{center}\end{minipage}{{end}}\\[5mm]\hline
{{end}}\end{tabular}
{{if (odd .Num) and .More}}\vspace*{\fill}
\newpage

{{end}}`)
	if err != nil {
		log.Fatal(err)
	}
	for reader.Next() {
		out := bytes.NewBufferString("")
		err = rowt.Execute(out, struct {
			Plade banko.BankoPlade
			Num   int
			More  bool
		}{
			reader.Value(),
			reader.RecordNo(),
			!reader.IsLast(),
		})
		if err != nil {
			reader.Next()
			continue
		}
		os.Stdout.Write(out.Bytes())
	}
	os.Stdout.Write([]byte(`\end{center}
\end{document}
`))
}

