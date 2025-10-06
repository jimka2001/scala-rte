package demos.scalaio2025

import adjuvant.GnuPlot.runGnuPlot
import demos.scalaio2025.RteTree.algos

object Histogram {
  import demos.scalaio2025.CsvLine.{readCsvLines}

  import adjuvant.GnuPlot.histogram
  import DataPlot.plotCB

  def plotHistogram() = {

    val buckets = for{algo <- algos
                      csvlines = readCsvLines(algo)
                      state_counts = csvlines.map(_.state_count)
                      } yield ( algo, state_counts)

    histogram(basename="histogram",
      xlabel = "DFA state count",
      ylabel = "Percentage of DFAs per state count",
      title = "DFA State distribution for Rte",
      gnuFileCB = plotCB("plot-histogram"),
      buckets = buckets,
      keepIf = (c:Int) => c <= 9,
      otherLabel = ">= 10")
  }

}
