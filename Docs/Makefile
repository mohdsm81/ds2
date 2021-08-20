all:	report

report: 
	pdflatex report.tex
	bibtex report
	pdflatex report.tex
	pdflatex report.tex
clean: 
	rm -fr *.out
	rm -fr *.aux
	rm -fr *.bbl
	rm -fr *.blg
	rm -fr Arch-eps-converted-to.pdf ir-detailed-eps-converted-to.pdf
	rm -fr *.dvi
	rm -fr *.log
	rm -fr *~
