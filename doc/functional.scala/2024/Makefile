FLAGS = -pvc -pdf
%FLAGS = -pdf

newton.2024.functional.scala.slides.pdf : newton.2024.functional.scala.slides.ltx
	TEXINPUTS="$(TEXINPUTS):.:./includes:./images" \
	latexmk ${FLAGS} newton.2024.functional.scala.slides.ltx

clean:
	rm -rf newton.2024.functional.scala.slides.pdf *.sta *.log *.fls *.aux *.fdb_latexmk *.snm *.out *.nav *.toc
