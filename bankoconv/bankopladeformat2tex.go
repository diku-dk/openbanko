package main

import (
	"banko"
	"bytes"
	"log"
	"os"
	"text/template"
)

func main() {
	plader := banko.ParseBankoPladeFormat("")

	funcMap := template.FuncMap{
		"odd":  func(i int) bool { return i%2 != 0 },
		"even": func(i int) bool { return i%2 == 0 },
	}

	out := bytes.NewBufferString("")

	t, err := template.New("tex").Funcs(funcMap).Parse(`\documentclass{article}
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
{{range $i, $e := .Plader}}{{if even $i}}\topskip0pt
\vspace*{\fill}{{end}}{{if odd $i}}\vspace{16mm}{{end}}
\begin{tabular}{|r|r|r|r|r|r|r|r|r|}
\hline
{{range $row := $e}}{{range $ii, $f := $row}}{{if gt $ii 0}} & {{end}}\begin{minipage}[t]{20mm}\begin{center}{{if gt $f 0}}{{$f}}{{end}}\end{center}\end{minipage}{{end}}\\[5mm]\hline
{{end}}\end{tabular}
{{if (odd $i) and ne $i len $.Plader}}\vspace*{\fill}
\newpage

{{end}}{{end}}\end{center}
\end{document}
`)
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

