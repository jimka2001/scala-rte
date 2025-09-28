STATS = src/main/resources/statistics

balanced:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvBalanced 50" ; \
	done

naive-mid:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvNaiveMid 50"  ; \
	done

naive-edge:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvNaiveEdge 50"  ; \
	done





