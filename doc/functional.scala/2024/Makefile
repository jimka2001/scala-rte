%FLAGS = -pvc -pdf
FLAGS = -pdf

TEX_FILES := $(wildcard includes/*.tex)

IMG_FILES := $(wildcard images/*.* includes/*.pdf includes/*.png includes/*.jpg )

BASE = newton.2024.functional.scala

${BASE}.slides.pdf : ${BASE}.slides.ltx ${IMG_FILES} ${TEX_FILES}
	TEXINPUTS="$(TEXINPUTS):.:./includes:./images" \
	latexmk ${FLAGS} ${BASE}.slides.ltx
	([ -d ~/Google\ Drive/R+I/. ] && cp -f $@ ~/Google\ Drive/R+I/.) || true

clean:
	rm -rf ${BASE}.slides.pdf *.sta *.log *.fls *.aux *.fdb_latexmk *.snm *.out *.nav *.toc
