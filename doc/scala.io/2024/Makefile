FLAGS = -pvc -pdf
%FLAGS = -pdf

newton.2024.scalaio.slides.pdf : newton.2024.scalaio.slides.ltx includes/example1.pdf includes/example2.pdf
	TEXINPUTS="$(TEXINPUTS):.:./includes" \
	latexmk ${FLAGS} newton.2024.scalaio.slides.ltx

includes/example1.pdf : includes/example1.dot
	dot  -Tpdf includes/example1.dot -o includes/example1.pdf

includes/example2.pdf : includes/example2.dot
	dot  -Tpdf includes/example2.dot -o includes/example2.pdf

clean:
	rm -rf newton.2024.scalaio.slides.pdf *.sta *.log *.fls *.aux *.fdb_latexmk *.snm *.out *.nav *.toc
