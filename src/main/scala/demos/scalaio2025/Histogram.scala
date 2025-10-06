package demos.scalaio2025

import adjuvant.GnuPlot.runGnuPlot
import demos.scalaio2025.RteTree.algos

object Histogram {
  import demos.scalaio2025.CsvLine.{readCsvLines}

  import adjuvant.GnuPlot.histogram
  import DataPlot.plotCB

  def plotHistogram(prefix:String="") = {

    val buckets = for{algo <- algos
                      csvlines = readCsvLines(algo,prefix)
                      state_counts = csvlines.map(_.state_count)
                      } yield ( algo, state_counts)

    histogram(basename= prefix + "histogram",
      xlabel = "DFA state count",
      ylabel = "Percentage of DFAs per state count",
      title = f"DFA State distribution for Rte $prefix",
      gnuFileCB = plotCB(s"plot-${prefix}histogram"),
      buckets = buckets,
      keepIf = (c:Int) => c <= 9,
      otherLabel = ">= 10")
  }

}
