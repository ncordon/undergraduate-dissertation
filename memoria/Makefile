TEX = pdflatex -shell-escape -interaction=nonstopmode -file-line-error
BIB = bibtex

.PHONY: all

all : memoria.pdf

%.pdf : memoria.tex memoria.bbl memoria.blg
	$(TEX) memoria.tex; $(TEX) memoria.tex; $(TEX) memoria.tex;

memoria.bbl memoria.blg : references.bib memoria.aux
	$(BIB) memoria.aux || echo "" > /dev/null

memoria.aux : memoria.tex
	$(TEX) memoria.tex

references.bib : memoria.tex
	$(TEX) memoria.tex
