package demos.scalaio2025

import adjuvant.GnuPlot.gnuPlot
import rte.Rte
import scala.util.Random

object RteTree {
  import demos.scalaio2025.CsvLine.{statisticsResource,writeCsvStatistic,readCsvLines,readAllCsvLines}
  import demos.scalaio2025.Random.{randomNaiveRteBySizeMid,randomNaiveRteBySizeEdge}
  import demos.scalaio2025.Random.randomTotallyBalancedRteBySize
  val random = new Random

  val algos = Seq("naive-mid", "naive-edge", "balanced")

  val csv = Map("naive-mid" -> (statisticsResource + "naive-mid.csv"),
                "naive-edge" -> (statisticsResource + "naive-edge.csv"),
                "balanced" -> (statisticsResource + "balanced.csv" ))

  val genRte = Map("naive-edge" -> randomNaiveRteBySizeEdge _,
                   "naive-mid" -> randomNaiveRteBySizeMid _,
                   "balanced" -> randomTotallyBalancedRteBySize _
                   )

  def genCsvBySize(num_repetitions: Int,
                   algo:String): Unit = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    for {r <- 0 to num_repetitions
         futures = (0 to 5).map((_) => Future {
           val size = random.between(3, 1<<6)
           println(s"r=$r size=$size")
           writeCsvStatistic(genRte=() => genRte(algo)(size), prefix=algo, csvFileName=csv(algo))
         })
         combined = Future.sequence(futures)} {
      Await.result(combined, Duration.Inf)
    }
  }


  def genThresholdCurve(cvslines: Seq[CsvLine]): Seq[(Double, Double)] = {
    val num_per_depth = cvslines.size
    val state_count_to_dfa_count = cvslines
      .groupBy(_.state_count)
      .collect { case (state_count, cvslines) => (state_count, cvslines.size) }
    val xys = for {(this_state_count, _) <- state_count_to_dfa_count.toList
                   // compute percentage of cvslines which have state_count > this_state_count
                   // 1st, how many cvslines have state_count > this_state_count
                   n = state_count_to_dfa_count.collect { case (state_count, count) if state_count <= this_state_count => count}.sum
                   } yield (this_state_count.toDouble, 100.0 * n.toDouble / num_per_depth)
    xys.sortBy(_._1)
  }

  def plotPopulation(): Unit = {
    val descrs1 = for {str <- algos
                      alllines = readCsvLines(str)
                      xys = for {(leaf_count, csvlines) <- alllines.groupBy(cl => cl.leaf_count)
                                 } yield (leaf_count.toDouble, csvlines.length.toDouble)
                      } yield (s"${str} samples=${alllines.length}", xys.to(List).sortBy(_._1))
    gnuPlot(descrs1.to(Seq))(title="Rte Leaf Count Histogram",
                            xAxisLabel = "Leaf Count",
                            yAxisLabel = "Frequency",
                            view = true,
                            grid= true)

    val descrs2 = for {str <- algos
                      alllines = readCsvLines(str)
                      xys = for {(state_count, csvlines) <- alllines.groupBy(cl => cl.state_count)
                                 } yield (state_count.toDouble, csvlines.length.toDouble )
                      } yield (s"${str} samples=${alllines.length}", xys.to(List).sortBy(_._1))
    gnuPlot(descrs2.to(Seq))(title="DFA State Count Histogram",
                            xAxisLabel = "DFA State Count",
                            yAxisLabel = "Frequency",
      yLog = true, xLog = true,
                            view = true,
                            grid= true)


  }

  // make plot of y vs x where y = percentage of samples where number of state_counts > x
  def plotThreshold(): Unit = {

    val descrs = for {str <- algos
                      alllines = readCsvLines(str)
                      xys = genThresholdCurve(alllines)
                      } yield (s"${str} samples=${alllines.length}", xys)
   gnuPlot(descrs.to(Seq))(title = "Threshold",
      xAxisLabel = "DFA state count",
      xLog = true,
      grid = true,
      yLog = false,
      yAxisLabel = "Percentage dfa <= state count",
      view = true)
  }

  // for each algorithm  we generate a curve of (x,y) pairs
  //   where x = rte leaf count
  //         y = count of unique dfas produced
  //    for now we count the raw number of different dfas, later we need to normalize by how
  //         many rtes of this leaf size contributed to the sample.
  def plotDiversity():Unit = {
    val descrs = for {str <- algos
                      alllines = readCsvLines(str)
                      numsamples = alllines.length
                      xys = for {(leafCount:Int, csvlines) <- alllines.groupBy(_.leaf_count)
                                 diversity = csvlines.map(cl => (cl. state_pre_count,
                                   cl.transition_pre_count,
                                   cl.state_count,
                                   cl.transition_count)).distinct.length
                                 } yield (leafCount.toDouble, diversity.toDouble)
         } yield (s"$str - $numsamples samples", xys.toList.sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title="Diversity",
                            xAxisLabel = "RTE leaf count",
                            yAxisLabel = "Unique DFA count",
                            view = true)
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
      yAxisLabel = "Computation Time (sec)",
      grid = true,
      yLog = true,
      gnuFileCB = println,
      plotWith = "linespoints",
      view = true)
  }

  def plotTimeOut():Unit = {
    val descrs = for {str <- algos
                      alllines = readAllCsvLines(str)
                      xys = for{(leaf_count, csvlines) <- alllines.filter((cl) => cl.state_count <= 0 || cl.transition_count <= 0).groupBy(_.leaf_count)
                                } yield (leaf_count.toDouble, csvlines.length.toDouble)
                      if (xys.size > 0)
                      } yield (s"$str - ${alllines.length} samples", xys.toList.sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title="Time Outs",
      xAxisLabel = "RTE leaf count",
      yAxisLabel = "Frequency",
      grid = true,
      gnuFileCB = println,
      plotWith = "points",
      view = true)
  }

  def plotAverageCsv(): Unit = {

    for {str <- algos
         alllines = readCsvLines(str)} {

      val sample_count = alllines.size
      val xys = for {(leaf_count, tuples) <- alllines.groupBy(_.leaf_count)
                      state_counts = tuples.map(_.state_count)
                      average = state_counts.sum / state_counts.size.toDouble
                      } yield (leaf_count.toDouble, average)
      val descrs = Seq((str, xys.to(List).sortBy(_._1)))
      gnuPlot(descrs)(title = f"Average ${str} node count (${sample_count} samples)",
        xAxisLabel = "AST leaf count",
        yAxisLabel = "DFA state count",
        yLog = true,
        grid = true,
        gnuFileCB = println,
        plotWith = "points",
        view = true)
    }
  }

  def plotBalance1(): Unit = {
    val descrs = for {str <- algos
                      alllines = readCsvLines(str)
                      grouped = alllines.groupBy(cl => cl.longest.toDouble / cl.shortest)
                      xys = for {(balance, cvslines) <- grouped
                                 ratios = cvslines.map(cl => cl.node_count.toDouble / cl.state_count )
                                 } yield (balance, ratios.sum / ratios.length)
                      } yield (str + s" ${alllines.length} samples", xys.to(List).sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title = "1 Balances",
      xAxisLabel = "Imbalance factor",
      yAxisLabel = "node:state ratio",
      plotWith = "points",
      gnuFileCB = println,
      view = true
    )
  }

  def plotBalance2(): Unit = {
    import scala.math.abs
    val delta_balance = 0.25
    val descrs = for {str <- algos
                      alllines = readCsvLines(str)
                      grouped = alllines.groupBy(cl => cl.imbalance())
                      _ = println()
                      // for each value of balance, compute the percentage of state_count <= 2
                      xys = for {(balance, _) <- grouped
                                 cls = alllines.filter{cl => abs(cl.imbalance() - balance) <= delta_balance }
                                 num_samples = cls.length
                                 num_small = cls.count(cl => cl.state_count <= 2)
                                 } yield (balance, (100.0 * num_small) / num_samples)
                      } yield (str + s" ${alllines.length} samples", xys.to(List).sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title = "2 Balances",
      xAxisLabel = "Imbalance factor",
      yAxisLabel = "Percentage count >= 2 for x=imbalance",
      plotWith = "lines",
      gnuFileCB = println,
      view = true
    )
  }

  def plotBalance3(): Unit = {
    val descrs = for {str <- algos
                      alllines = readCsvLines(str)
                      num_samples = alllines.length
                      grouped = alllines.groupBy(_.imbalance())
                      // for each value of balance, compute the percentage of state_count <= 2
                      xys = for {(imbalance, _) <- grouped
                                 num_small = alllines.count(cl => cl.imbalance() <= imbalance && cl.state_count <= 2)
                                 } yield (imbalance, (100.0 * num_small) / num_samples)
                      } yield (str + s" ${alllines.length} samples", xys.to(List).sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title = "Running Balances",
                            xAxisLabel = "Imbalance Factor",
                            yAxisLabel = "Percentage count >= 2 for imblance <= x",
                            plotWith = "lines",
                            gnuFileCB = println,
                            grid = true,
                            view = true
                            )
  }

  def plotBalance4(): Unit = {
    import scala.math.log
    val descrs = for {str <- algos
                      alllines = readCsvLines(str)
                      num_samples = alllines.length
                      xys = for {cl <- alllines
                                 aspect_ratio = cl.longest / log(cl.leaf_count)
                                 } yield (aspect_ratio, cl.state_count.toDouble)
                      } yield (str + s" ${alllines.length} samples", xys.to(List).sortBy(_._1))
    gnuPlot(descrs.to(Seq))(title = "Running Balances per Aspect Ratio",
                            xAxisLabel = "Aspect Ratio",
                            yAxisLabel = "DFA state count",
                            plotWith = "points",
                            gnuFileCB = println,
                            grid = true,
                            view = true
                            )
  }

  def plotBalance5(): Unit = {
    import scala.math.log
    def integral(xys:List[(Double,Double)], acc:List[(Double,Double)]):List[(Double,Double)] = {
      xys match {
        case (x1, y1)::(x2,y2)::_ =>
          val average_x = (x1 + x2) / 2.0
          val average_y = (y1 + y2) / 2.0
          val delta_x = x2 - x1
          val trap_area = delta_x * average_y
          val sum = acc.head._2
          integral(xys.tail, (average_x, sum+trap_area)::acc)
        case _ => acc
      }
    }
    val descrs = for {str <- algos
                      alllines = readCsvLines(str)
                      num_samples = alllines.length
                      xys_pre = (for {cl <- alllines
                      // Aspect ratio is an approximation of length / width.
                      // By length, we mean distance from top to bottom of the AST (rte) tree
                      // By width, we mean
                                 aspect_ratio = cl.longest.toDouble /  (log(cl.leaf_count)/log(2))
                                 } yield (aspect_ratio, cl.state_count.toDouble)).to(List).sortBy(_._1)
                      xys = integral(xys_pre, List((xys_pre.head._1, 0.0)))
                      } yield (str + s" ${alllines.length} samples", xys)
    gnuPlot(descrs.to(Seq))(title = "Running Sums Balances per Aspect Ratio",
                            xAxisLabel = "Aspect Ratio",
                            yAxisLabel = "DFA state count",
                            plotWith = "points",
                            gnuFileCB = println,
                            grid = true,
                            view = true
                            )
  }
}

object TestBalance {
  import rte.{Sigma, EmptySet, Or, EmptySeq}
  import rte.Rte.rteViewAst

  def main(argv:Array[String]):Unit = {
    val rte1 = Or(Sigma, Or(EmptySet,Or(Sigma,EmptySeq)))
    println(rte1.measureBalance())
    println(rte1.linearize().length)
    rteViewAst(rte1, "testing")

    val rte2 = Or(Sigma, Or(Sigma, Or(Sigma, Or(Sigma, Or(Sigma,EmptySeq)))))
    println(rte2.measureBalance())
    println(rte2.linearize().length)
    rteViewAst(rte2, "testing2")
  }
}

object ViewAst {

  import rte.Rte.rteViewAst
  import xymbolyco.GraphViz.dfaView
  import xymbolyco.Minimize.minimize

  def main(argv: Array[String]): Unit = {
    val depth: Int = if (argv.length == 0) 4 else argv(0).toInt
    for {algo <- RteTree.algos
         rte = RteTree.genRte(algo)(1 << depth)
         dfa = minimize(rte.toDfa())
         } {
      rteViewAst(rte, title = algo)
      println(rte.measureBalance())
      println(rte.linearize().length)
      println(CsvLine.imbalanceFactor(rte.linearize().length, 1))
      dfaView(dfa, title = algo)
    }
  }
}