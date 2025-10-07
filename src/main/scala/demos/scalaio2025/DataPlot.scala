package demos.scalaio2025

import adjuvant.GnuPlot.{gnuPlot, runGnuPlot}
import demos.scalaio2025.CsvLine.readCsvLines
import demos.scalaio2025.DataPlot.{plotTimeOut, plotTimes}
import demos.scalaio2025.RteTree.algos
import demos.scalaio2025.CsvLine.readAllCsvLines

import scala.sys.process.stringSeqToProcess
import scala.language.postfixOps

object DataPlot {
  def plotCB(base:String): (String=>Unit) = {
    assert(!base.contains("."))
    val reportDir = "/Users/jnewton/Repos/rte/scala-rte/doc/scala.io/2025/includes/"
    val gnuFileName = reportDir + base + ".gnu"
    val pngFileName = reportDir + base + ".png"
    (fn:String) => locally {
      Seq("cp", fn, gnuFileName).!
      runGnuPlot("png", gnuFileName, pngFileName)
      ()
    }
  }

  def plotAverageCsv(): Unit = {

    for {algo <- algos
         alllines = for {prefix <- Seq("", "64-", "128-")
                         line <- readCsvLines(algo)} yield line
         }{
      val sample_count = alllines.size
      val xys = for {(leaf_count, tuples) <- alllines.groupBy(_.leaf_count)
                     state_counts = tuples.map(_.state_count)
                     average = state_counts.sum / state_counts.size.toDouble
                     } yield (leaf_count.toDouble, average)
      val descrs = Seq((algo, xys.to(List).sortBy(_._1)))
      gnuPlot(descrs)(title = f"Average ${algo} node count (${sample_count} samples)",
        xAxisLabel = "AST leaf count",
        yAxisLabel = "DFA state count",
        yLog = true,
        grid = true,
        gnuFileCB = plotCB(s"plot-average-$algo-node-count"),
        plotWith = "points",
        view = true)
    }
  }
  

  val imbalanceAlgos = Seq(((cl: CsvLine) => cl.aspectRatio(), "Aspect-Ratio"),
    ((cl: CsvLine) => cl.imbalance(), "Imbalance-Factor"),
    ((cl: CsvLine) => cl.ratioLongestShortest(), "Ratio-longest:shortest"))

  def plotStateLossage(prefix:String=""): Unit = {
    for {(imb, algoName) <- imbalanceAlgos
         } {
      val descrs = for {algo <- algos
                        cvslines = readCsvLines(algo, prefix)
                        xys = for {cl <- cvslines
                                   if cl.node_count != 0
                                   } yield (imb(cl), cl.state_count / cl.node_count.toDouble )
                        } yield (algo + s" ${cvslines.length} samples", xys.to(List).sortBy(_._1))
      gnuPlot(descrs.to(Seq))(title = s"Lossage: Ratio node count per state count $prefix",
        xAxisLabel = algoName,
        yAxisLabel = "lossage",
        yLog = true,
        plotWith = "points",
        gnuFileCB = plotCB(s"plot-${prefix}lossage-${algoName}"),
        view = true
      )
    }
  }
  def plotBalanceLocalAverage(): Unit = {
    import scala.math.abs
    val delta_balance = 0.25
    for {(imb, algoName) <- imbalanceAlgos
         descrs = for {str <- algos
                       alllines = readCsvLines(str)
                       grouped = alllines.groupBy(imb)
                       // for each value of balance, compute the percentage of state_count <= 2
                       xys = for {(balance, _) <- grouped
                                  cls = alllines.filter { cl => abs(imb(cl) - balance) <= delta_balance }
                                  num_samples = cls.length
                                  num_small = cls.count(cl => cl.state_count <= 2)
                                  } yield (balance, (100.0 * num_small) / num_samples)
                       } yield (str + s" ${alllines.length} samples", xys.to(List).sortBy(_._1))
         }
    gnuPlot(descrs.to(Seq))(title = "Local average " + algoName,
      xAxisLabel = algoName,
      yAxisLabel = "Percentage count >= 2 for x=imbalance",
      plotWith = "lines",
      gnuFileCB = plotCB("plot-local-average"),
      view = true
    )
  }

  def plotRunningNonTrivalVsImbalance(prefix:String=""): Unit = {
    for {(imb, algoName) <- imbalanceAlgos} {
      val descrs = for {str <- algos
                        alllines = readCsvLines(str, prefix)
                        num_samples = alllines.length
                        grouped = alllines.groupBy(imb)
                        // for each value of balance, compute the percentage of state_count <= 2
                        xys = for {(imbalance, _) <- grouped
                                   num_small = alllines.count(cl => imb(cl) <= imbalance && cl.state_count <= 2)
                                   } yield (imbalance, (100.0 * num_small) / num_samples)
                        } yield (str + s" ${alllines.length} samples", xys.to(List).sortBy(_._1))

      gnuPlot(descrs.to(Seq))(title = s"Running Balances $prefix for $algoName",
        xAxisLabel = algoName,
        yAxisLabel = "Percentage count >= 2 for imbalance <= x",
        plotWith = "lines",
        gnuFileCB = plotCB(s"plot-${prefix}running-balances-$algoName"),
        grid = true,
        view = true
      )
    }
  }

  def plotBalanceDfaCountVsAspectRatio(prefix:String=""): Unit = {
    for {(imb, xlabel) <- imbalanceAlgos
         descrs = for {str <- algos
                       alllines = readCsvLines(str,prefix)
                       num_samples = alllines.length
                       xys = for {cl <- alllines
                                  } yield (imb(cl), cl.state_count.toDouble)
                       } yield (str + s" ${num_samples} samples", xys.to(List).sortBy(_._1))
         }
    gnuPlot(descrs.to(Seq))(title = s"DFA state count ${prefix} vs " + xlabel,
      xAxisLabel = xlabel,
      yAxisLabel = "DFA state count",
      plotWith = "points",
      yLog = true,
      gnuFileCB = plotCB(s"plot-${prefix}dfa-state-count-vs-$xlabel"),
      grid = true,
      view = true
    )
  }

  def plotBalanceRunningSum(prefix:String=""): Unit = {
    def integral(xys: List[(Double, Double)], acc: List[(Double, Double)]): List[(Double, Double)] = {
      xys match {
        case (x1, y1) :: (x2, y2) :: _ =>
          val average_x = (x1 + x2) / 2.0
          val average_y = (y1 + y2) / 2.0
          val delta_x = x2 - x1
          val trap_area = delta_x * average_y
          val sum = acc.head._2
          integral(xys.tail, (average_x, sum + trap_area) :: acc)
        case _ => acc
      }
    }

    for {(imb, xlabel) <- imbalanceAlgos
         descrs = for {algo <- algos
                       alllines = readCsvLines(algo,prefix)
                       num_samples = alllines.length
                       xys_pre = (for {cl <- alllines
                                       } yield (imb(cl), cl.state_count.toDouble)).to(List).sortBy(_._1)
                       xys = integral(xys_pre, List((xys_pre.head._1, 0.0)))
                       } yield (algo + s" ${num_samples} samples", xys)
         }
      gnuPlot(descrs.to(Seq))(title = s"Running Sum Balances $prefix per " + xlabel,
        xAxisLabel = xlabel,
        yAxisLabel = "DFA state count",
        plotWith = "lines",
        gnuFileCB = plotCB(s"plot-${prefix}running-sum-balances-per-$xlabel"),
        grid = true,
        view = true
      )
  }

  def plotLeafCountPopulation(): Unit = {
    val descrs = for {str <- algos
                       alllines = readCsvLines(str)
                       xys = for {(leaf_count, csvlines) <- alllines.groupBy(cl => cl.leaf_count)
                                  } yield (leaf_count.toDouble, csvlines.length.toDouble)                
                       } yield (s"${str} samples=${alllines.length}", xys.to(List).sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title="Rte Leaf Count Histogram",
      xAxisLabel = "Leaf Count",
      yAxisLabel = "Frequency",
      gnuFileCB = plotCB("plot-rte-leaf-count-histogram"),
      plotWith = "points",
      pointSize = 1.25,
      view = true,
      grid= true)
  }

  def plotStateCountPopulation(prefix:String=""): Unit = {
    val descrs = for {str <- algos
                       alllines = readCsvLines(str,prefix)
                       xys = for {(state_count, csvlines) <- alllines.groupBy(cl => cl.state_count)
                                  } yield (state_count.toDouble, csvlines.length.toDouble )
                       } yield (s"${str} samples=${alllines.length}", xys.to(List).sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title=s"DFA State Count Histogram $prefix",
      xAxisLabel = "DFA State Count",
      yAxisLabel = "Frequency",
      yLog = true, xLog = true,
      gnuFileCB = plotCB(s"plot-${prefix}dfa-state-count-histogram"),
      plotWith = "points",
      pointSize = 1.25,
      view = true,
      grid= true)
  }

  def genThresholdCurve(cvslines: Seq[CsvLine]): Seq[(Double, Double)] = {
    val num_per_depth = cvslines.size
    val state_count_to_dfa_count = cvslines
      .groupBy(_.state_count)
      .collect { case (state_count, cvslines) => (state_count, cvslines.size) }
    val xys = for {(this_state_count, _) <- state_count_to_dfa_count.toList
                   // compute percentage of cvslines which have state_count > this_state_count
                   // 1st, how many cvslines have state_count > this_state_count
                   n = state_count_to_dfa_count.collect { case (state_count, count) if state_count >= this_state_count => count}.sum
                   } yield (this_state_count.toDouble, 100.0 * n.toDouble / num_per_depth)
    xys.sortBy(_._1)
  }

  // make plot of y vs x where y = percentage of samples where number of state_counts > x
  def plotThreshold(prefix:String=""): Unit = {

    val descrs = for {str <- algos
                      alllines = readCsvLines(str,prefix)
                      xys = genThresholdCurve(alllines)
                      } yield (s"${str} samples=${alllines.length}", xys)
    gnuPlot(descrs.to(Seq))(title = s"Threshold $prefix",
      xAxisLabel = s"DFA state count $prefix",
      xLog = true,
      grid = true,
      yLog = true,
      gnuFileCB = plotCB(s"plot-${prefix}threshold"),
      yAxisLabel = "Percentage dfa >= state count",
      view = true)
  }

  // for each algorithm  we generate a curve of (x,y) pairs
  //   where x = rte leaf count
  //         y = count of unique dfas produced
  //    for now we count the raw number of different dfas, later we need to normalize by how
  //         many rtes of this leaf size contributed to the sample.
  def plotDiversity():Unit = {
    locally {
      val descrs = for {str <- algos
                        alllines = readCsvLines(str)
                        numsamples = alllines.length
                        xys = for {(leafCount: Int, csvlines) <- alllines.groupBy(_.leaf_count)
                                   diversity = csvlines.map(cl => (cl.state_pre_count,
                                     cl.transition_pre_count,
                                     cl.state_count,
                                     cl.transition_count)).distinct.length
                                   } yield (leafCount.toDouble, diversity.toDouble)
                        } yield (s"$str - $numsamples samples", xys.toList.sortBy(_._1))
      gnuPlot(descrs.to(Seq))(title = "Diversity",
        xAxisLabel = "RTE leaf count",
        yAxisLabel = "Unique DFA count",
        plotWith = "points",
        pointSize = 1.24,
        gnuFileCB = plotCB(f"plot-diversity"),
        view = true)
    }
    locally {
      import scala.math.log
      val descrs = for {str <- algos
                        alllines = for{prefix <- Seq("", "64-", "128-")
                                       line <- readCsvLines(str,prefix)
                                       } yield line
                        numsamples = alllines.length
                        xys = for {(leafCount: Int, csvlines) <- alllines.groupBy(_.leaf_count)
                                   line_count = csvlines.length
                                   if line_count > 1  // only plot points if line_count > 1 because we need the log_2
                                   log_line_count = log(line_count)/log(2)
                                   diversity = csvlines.map(cl => (cl.state_pre_count,
                                     cl.transition_pre_count,
                                     cl.state_count,
                                     cl.transition_count)).distinct.length
                                   } yield (leafCount.toDouble, diversity.toDouble / log_line_count)
                        } yield (s"$str - $numsamples samples", xys.toList.sortBy(_._1))
      gnuPlot(descrs.to(Seq))(title = "Diversity Per log(Sample)",
        xAxisLabel = "RTE leaf count",
        yAxisLabel = "Unique DFA count / log(sample count)",
        plotWith = "lines",
        gnuFileCB = plotCB(f"plot-diversity-per-sample"),
        view = true)
    }
  }

  def plotTimes():Unit = {
    val descrs = for {str <- algos
                      alllines = readCsvLines(str)
                      xys = for{(leaf_count, csvlines) <- alllines.groupBy(_.leaf_count)
                                avg = csvlines.map(_.duration).sum / csvlines.length.toDouble
                                } yield (leaf_count.toDouble, avg / 1000)
                      } yield (s"$str - ${alllines.length} samples", xys.toList.sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title="Computation Time",
      xAxisLabel = "RTE leaf count",
      yAxisLabel = "Average Computation Time (sec)",
      grid = true,
      yLog = true,
      gnuFileCB = plotCB(f"plot-computation-time"),
      plotWith = "linespoints",
      view = true)
  }

  def plotTimeOut():Unit = {
    val descrs = for {str <- algos
      alllines = for{prefix <- Seq("", "64-", "128-")
                     line <- readAllCsvLines(str,prefix)
      } yield line
                      xys = for{(leaf_count, csvlines) <- alllines.filter((cl) => cl.state_count <= 0 || cl.transition_count <= 0).groupBy(_.leaf_count)
                                } yield (leaf_count.toDouble, csvlines.length.toDouble)
                      if (xys.size > 0)
                      } yield (s"$str - ${alllines.length} samples", xys.toList.sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title="Time Outs",
      xAxisLabel = "RTE leaf count",
      yAxisLabel = "Frequency",
      grid = true,
      pointSize = 2.0,
      gnuFileCB = plotCB("plot-time-outs"),
      plotWith = "points",
      view = true)
  }
}

object LossagePlot {
  def main(array: Array[String]):Unit = {
    DataPlot.plotStateLossage()
    DataPlot.plotStateLossage("64-")
    DataPlot.plotStateLossage("128-")
  }
}

object BalancePlot {
  def main(array: Array[String]):Unit = {
    DataPlot.plotBalanceLocalAverage()
    DataPlot.plotRunningNonTrivalVsImbalance()
    DataPlot.plotRunningNonTrivalVsImbalance("64-")
    DataPlot.plotRunningNonTrivalVsImbalance("128-")
    DataPlot.plotBalanceDfaCountVsAspectRatio()
    DataPlot.plotBalanceDfaCountVsAspectRatio("64-")
    DataPlot.plotBalanceDfaCountVsAspectRatio("128-")
    DataPlot.plotBalanceRunningSum()
    DataPlot.plotBalanceRunningSum("64-")
    DataPlot.plotBalanceRunningSum("128-")
  }
}

object DiversityPlot {
  def main(array: Array[String]):Unit = {
    DataPlot.plotDiversity()
  }
}

object TimeOutPlot {
  def main(array: Array[String]):Unit = {
    plotTimeOut()
    plotTimes()
  }
}

object Histograms {
  def main(argv: Array[String]): Unit = {
    Histogram.plotHistogram()
    Histogram.plotHistogram("64-")
    Histogram.plotHistogram("128-")
  }
}

object Average {
  def main(argv: Array[String]): Unit = {
    DataPlot.plotAverageCsv()
  }
}

object PopulationPlot {
  def main(argv:Array[String]):Unit = {
    DataPlot.plotLeafCountPopulation()
    DataPlot.plotStateCountPopulation()
    DataPlot.plotStateCountPopulation("64-")
    DataPlot.plotStateCountPopulation("128-")
  }
}

object ThresholdPlot {
  def main(argv:Array[String]):Unit = {
    DataPlot.plotThreshold()
    DataPlot.plotThreshold("64-")
    DataPlot.plotThreshold("128-")
  }
}


object Plots {
  def main(argv: Array[String]): Unit = {
    PopulationPlot.main(argv)
    DiversityPlot.main(argv)
    ThresholdPlot.main(argv)
    Average.main(argv)
    Histograms.main(argv)
    BalancePlot.main(argv)
    LossagePlot.main(argv)
    TimeOutPlot.main(argv)
    DiversityPlot.main(argv)
  }
}
