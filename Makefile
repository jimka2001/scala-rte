STATS = src/main/resources/statistics

balanced:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvBalanced 10" ; \
	done

naive-mid:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvNaiveMid 10"  ; \
	done

naive-edge:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvNaiveEdge 10"  ; \
	done





