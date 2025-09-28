package demos.scalaio2025

object DataPlot {
  def main(array: Array[String]):Unit = {
  }
}

object BalancePlot {
  def main(array: Array[String]):Unit = {
    RteTree.plotBalance1()
    RteTree.plotBalance2()
    RteTree.plotBalance3()
    RteTree.plotBalance4()
    RteTree.plotBalance5()
  }
}

object DiversityPlot {
  def main(array: Array[String]):Unit = {
    RteTree.plotDiversity()
  }
}

object Plots {
  import demos.scalaio2025.GnuPlot.histogram
  def main(argv: Array[String]): Unit = {
    RteTree.plotPopulation()
    RteTree.plotDiversity()
    RteTree.plotThreshold()
    RteTree.plotAverageCsv()
    histogram()
    BalancePlot.main(argv)
  }
}

object Histograms {
  import demos.scalaio2025.GnuPlot.histogram
  def main(argv: Array[String]): Unit = {
    histogram()
  }
}
