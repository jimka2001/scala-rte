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



loop:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvNaiveEdge 1" ; \
		sbt "runMain demos.scalaio2025.GenCsvNaiveMid 1" ; \
		sbt "runMain demos.scalaio2025.GenCsvBalanced 1" ; \
	done

simple:
	sbt "runMain demos.scalaio2025.GenCsvNaiveEdge 10"
	sbt "runMain demos.scalaio2025.GenCsvNaiveMid 10"
	sbt "runMain demos.scalaio2025.GenCsvBalanced 10"

sample-64:
	for i in $$(seq 1 50); do \
		sbt "runMain demos.scalaio2025.GenCsvFixedLeafCount 10 64" ;\
	done

sample-128:
	for i in $$(seq 1 50); do \
		sbt "runMain demos.scalaio2025.GenCsvFixedLeafCount 10 128" ;\
	done
