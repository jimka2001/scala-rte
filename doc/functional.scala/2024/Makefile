%FLAGS = -pvc -pdf
FLAGS = -pdf

TEX_FILES := $(wildcard includes/*.tex)

IMG_FILES := $(wildcard images/*.* includes/*.pdf includes/*.png includes/*.jpg )

newton.2024.functional.scala.slides.pdf : newton.2024.functional.scala.slides.ltx ${IMG_FILES} ${TEX_FILES}
	TEXINPUTS="$(TEXINPUTS):.:./includes:./images" \
	latexmk ${FLAGS} newton.2024.functional.scala.slides.ltx
	([ -d ~/Google\ Drive/R+I/. ] && cp -f $@ ~/Google\ Drive/R+I/.) || true

clean:
	rm -rf newton.2024.functional.scala.slides.pdf *.sta *.log *.fls *.aux *.fdb_latexmk *.snm *.out *.nav *.toc
