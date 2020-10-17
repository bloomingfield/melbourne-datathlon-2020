.PHONY: all
all: document.tex document.pdf clean

%.tex: %.rnw
	echo "knitr::knit('${@:.tex=.rnw}')" | R --no-save --no-restore

%.pdf: %.tex
	latexmk -pdf $< 

.PHONY: clean
clean: 
	rm -f *.aux *.bbl *.bcf *.blg *.fdb_latexmk *.fls *.lof *.log *.lot \
		*.code *.loe *.toc *.rec *.out *.run.xml *.nav *.snm
	rm -rf cache/ auto/
 
