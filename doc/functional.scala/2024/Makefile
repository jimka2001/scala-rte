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

WEBPAGE_REPO := git@gitlab.lre.epita.fr:lre/lre-web/members/jim-newton.git

webpage: newton.2024.functional.scala.slides.pdf
	tmp=$$(mktemp -d) && \
	git clone --depth 1 --quiet $(WEBPAGE_REPO) $$tmp && \
	cp newton.2024.functional.scala.slides.pdf $$tmp/ && \
	cd $$tmp && \
	git add newton.2024.functional.scala.slides.pdf && \
	(git diff --cached --quiet || git commit --quiet -m "Update newton.2024.functional.scala.slides.pdf") && \
	git push --quiet ; \
	status=$$? ; rm -rf $$tmp ; exit $$status
