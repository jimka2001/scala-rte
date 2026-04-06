STATS = src/main/resources/statistics

flajolet:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvFlajolet 10" ; \
	done

tree-split-mid:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvTreeSplitMid 10"  ; \
	done

tree-split-edge:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvTreeSplitEdge 10"  ; \
	done

tree-split-linear:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvTreeSplitLinear 10"  ; \
	done

comb:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvComb 10"  ; \
	done



loop:
	for i in $$(seq 1 10); do \
		sbt "runMain demos.scalaio2025.GenCsvTreeSplitEdge 1" ; \
		sbt "runMain demos.scalaio2025.GenCsvTreeSplitLinear 1" ; \
		sbt "runMain demos.scalaio2025.GenCsvTreeSplitMid 1" ; \
		sbt "runMain demos.scalaio2025.GenCsvBalanced 1" ; \
		sbt "runMain demos.scalaio2025.GenCsvComb 1" ; \
	done

simple:
	sbt "runMain demos.scalaio2025.GenCsvTreeSplitEdge 10"
	sbt "runMain demos.scalaio2025.GenCsvTreeSplitLinear 10"
	sbt "runMain demos.scalaio2025.GenCsvTreeSplitMid 10"
	sbt "runMain demos.scalaio2025.GenCsvBalanced 10"
	sbt "runMain demos.scalaio2025.GenCsvComb 10"

tree-split-linear-64:
	for i in $$(seq 1 50); do \
		sbt "runMain demos.scalaio2025.GenCsvFixedLeafCount 10 64 tree-split-linear" ;\
	done

tree-split-linear-128:
	for i in $$(seq 1 50); do \
		sbt "runMain demos.scalaio2025.GenCsvFixedLeafCount 10 128 tree-split-linear" ;\
	done

sample-64:
	for i in $$(seq 1 50); do \
		sbt "runMain demos.scalaio2025.GenCsvFixedLeafCount 10 64" ;\
	done

sample-128:
	for i in $$(seq 1 50); do \
		sbt "runMain demos.scalaio2025.GenCsvFixedLeafCount 10 128" ;\
	done

sbt-test-2:
	bin/sbt-test.csh

sbt-test-3:
	bin/sbt-test.csh try-2-conv-3.x

