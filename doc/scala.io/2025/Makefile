# The variable LATEX_OPTS was suggested by chatgpt.
# to include the -pvc option, run as follows
# make LATEX_OPTS=-pvc
LATEX_OPTS ?=
LATEXMK := latexmk --pdf $(LATEX_OPTS)

DOT_DIR := ./dots
DOTS := $(wildcard $(DOT_DIR)/*.dot)
GEN_PDFS := $(DOTS:.dot=.pdf)

newton.2025.scalaio.slides.pdf : newton.2025.scalaio.slides.ltx  $(GEN_PDFS)
	TEXINPUTS="$(TEXINPUTS):.:./includes:./gnu:./dots" \
	${LATEXMK} ${FLAGS} newton.2025.scalaio.slides.ltx

$(DOT_DIR)/%.pdf: $(DOT_DIR)/%.dot
	dot -Tpdf $< -o $@


clean:
	rm -rf newton.2025.scalaio.slides.pdf *.sta *.log *.fls *.aux *.fdb_latexmk *.snm *.out *.nav *.toc
