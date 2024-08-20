FLAGS = -pvc -pdf
%FLAGS = -pdf

newton.2024.scalaio.slides.pdf : newton.2024.scalaio.slides.ltx
	TEXINPUTS="$(TEXINPUTS):.:./includes" \
	latexmk ${FLAGS} newton.2024.scalaio.slides.ltx


clean:
	rm -rf newton.2024.scalaio.slides.pdf *.sta *.log *.fls *.aux *.fdb_latexmk *.snm *.out *.nav *.toc
