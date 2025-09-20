STATS = src/main/resources/statistics

balanced:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvBalanced 50" ; \
	done

tuned:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvTuned 50" ; \
	done

tunedME:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvTunedME 50" ; \
	done

naive:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvNaive 50"  ; \
	done





