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

WEBPAGE_REPO := git@gitlab.lre.epita.fr:lre/lre-web/members/jim-newton.git

webpage: newton.2024.scalaio.slides.pdf
	tmp=$$(mktemp -d) && \
	git clone --depth 1 --quiet $(WEBPAGE_REPO) $$tmp && \
	cp newton.2024.scalaio.slides.pdf $$tmp/ && \
	cd $$tmp && \
	git add newton.2024.scalaio.slides.pdf && \
	(git diff --cached --quiet || git commit --quiet -m "Update newton.2024.scalaio.slides.pdf") && \
	git push --quiet ; \
	status=$$? ; rm -rf $$tmp ; exit $$status
