# The variable LATEX_OPTS was suggested by chatgpt.
# to include the -pvc option, run as follows
# make LATEX_OPTS=-pvc
LATEX_OPTS ?=
LATEXMK := latexmk --pdf $(LATEX_OPTS)



newton.2025.scalaio.slides.pdf : newton.2025.scalaio.slides.ltx includes/balanced-dfa.pdf includes/balanced-rte.pdf includes/rte-naive-edge.pdf includes/dfa-naive-edge.pdf
	TEXINPUTS="$(TEXINPUTS):.:./includes:./gnu" \
	${LATEXMK} ${FLAGS} newton.2025.scalaio.slides.ltx

includes/balanced-dfa.pdf : includes/balanced-dfa.dot
	dot -Tpdf includes/balanced-dfa.dot -o includes/balanced-dfa.pdf

includes/balanced-rte.pdf : includes/balanced-rte.dot
	dot -Tpdf includes/balanced-rte.dot -o includes/balanced-rte.pdf

includes/rte-naive-edge.pdf : includes/rte-naive-edge.dot
	dot -Tpdf includes/rte-naive-edge.dot -o includes/rte-naive-edge.pdf

includes/dfa-naive-edge.pdf : includes/dfa-naive-edge.dot
	dot -Tpdf includes/dfa-naive-edge.dot -o includes/dfa-naive-edge.pdf

clean:
	rm -rf newton.2025.scalaio.slides.pdf *.sta *.log *.fls *.aux *.fdb_latexmk *.snm *.out *.nav *.toc
