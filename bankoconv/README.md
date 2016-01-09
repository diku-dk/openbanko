Bankoconv
=========

Kør `make` for at oversætte programmerne.


bankopladeformat2html
---------------------

Konvertér dine bankoplader til en kæmpe HTML-fil.  Kør

    $ ./bankopladeformat2html < sti/til/plader.bankopladeformat > plader.html

Hvis du ikke vil oversætte Go-koden, kan du i stedet køre

    $ go run bankopladeformat2html.go < sti/til/plader.bankopladeformat > plader.html


bankopladeformat2tex
--------------------

Konvertér dine bankoplader til en fin LaTeX-fil.  Kør

    $ ./bankopladeformat2tex < sti/til/plader.bankopladeformat > plader.tex

eller blot

    $ ./bankopladeformat2pdf < sti/til/plader.bankopladeformat > plader.pdf
