all: plots 906Paper.pdf 906Presentation.pdf 

906Paper.pdf: 906Paper.tex 
	pdflatex 906Paper
	bibtex 906Paper
	pdflatex 906Paper
	pdflatex 906Paper

#this needs to be run multiple times
906Presentation.pdf: 906Presentation.tex 
	pdflatex 906Presentation
	bibtex 906Presentation
	pdflatex 906Presentation
	pdflatex 906Presentation

plots:
	rm -rf plots
	mkdir plots
	Rscript DistributionComparison.r

clean: cleantex cleanpdf
	rm -rf plots

cleantex:
	rm -f *.aux *.toc *.log *.blg *.bbl *.dvi *.nav *.out *.snm

cleanpdf:
	rm -f *.pdf

.PHONY: clean cleantex plots 906Paper.pdf 906Presentation.pdf
