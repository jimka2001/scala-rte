package demos.rte

import adjuvant.Adjuvant.{callWithTimeout, openGraphicalFile}
import adjuvant.GnuPlot.gnuPlot
import rte.Rte
import demos.scalaio2025.CsvLine

import java.io.FileWriter
import java.nio.file.Paths
import java.util.UUID
import scala.sys.process.stringSeqToProcess
import scala.util.Random

object RteTree {

  val random = new Random
  val statisticsResource: String = Paths.get("src/main/resources/statistics").toString + "/"
  val naiveCsv: String = statisticsResource + "naive.csv"
  val tunedCsv: String = statisticsResource + "tuned.csv"
  val tunedMECsv: String = statisticsResource + "tunedME.csv" // tuned but maybe empty = true
  val balancedCsv: String = statisticsResource + "balanced.csv"

  def withLock[A](lockFile: String, f: () => A): A = {
    import adjuvant.FileLock.callInBlock
    callInBlock(lockFile + ".lock")(f)
  }


  /**
   * @param csvFileName name of a CSV file in resources/statistics
   * @param writeRecord function that writes one record into the given FileWriter
   */
  def mergeFile(csvFileName: String, prefix: String = "")(writeRecord: FileWriter => Unit): Unit = {
    // create temporary files ending in ~ so that .gitignore will ignore them.
    val tmp1 = statisticsResource + prefix + UUID.randomUUID().toString + "~"
    val tmp2 = statisticsResource + prefix + UUID.randomUUID().toString + "~"

    // write one record into tmp1
    val outFile = new FileWriter(tmp1, true)
    try {
      writeRecord(outFile)
    } finally {
      outFile.close()
    }

    // critical section: merge tmp1 with the CSV and update it atomically
    withLock(csvFileName, () => {
      Seq("touch", csvFileName).!
      Seq("sort", "-t,", "-k1,4n", "-m", tmp1, csvFileName, "-o", tmp2).!
      Seq("mv", tmp2, csvFileName).!
    })

    // cleanup
    Seq("trash", tmp1).!
  }

  // generate an Rte of size targetSize
  // count the actual size (which should be close to the target size)
  // generate the Dfa
  // count the states and transitions
  // record targetSize
  // write an entry to the csv file so we can plot later

  def writeCsvStatistic(genRte: (() => Rte), prefix:String, csvFileName: String,
                        probability: Option[Float]): Unit = {

    import xymbolyco.Minimize.minimize
    val rte = genRte()
    val nodeCount = rte.linearize().size
    val (shortest, longest, total) = rte.measureBalance()
    def report(nodeCount:Int, stateCount:Option[Int], transitionCount:Option[Int],
               minStateCount:Option[Int], minTransitionCount:Option[Int]):Unit = {

      mergeFile(csvFileName)((outFile: FileWriter) => {
        outFile.write(s"$nodeCount,")
        for {op <- Seq(stateCount, transitionCount, minStateCount, minTransitionCount)
             } outFile.write(
          op match {
            case Some(s) => s"$s,"
            case None => "-1,"
          })
        outFile.write(f"$shortest,$longest,$total")
        probability match {
          case Some(probability) =>
            outFile.write(f",$probability%.2f")
          case _ => ()
        }
      })
    }

    val timeout = longest * longest * 4000
    for {dfa <- callWithTimeout(timeout,
      () => rte.toDfa(true),
      () => {
        println(s"cancelling DFA generation after ${timeout}ms nodeCount=$nodeCount, csv=$csvFileName")
        report(nodeCount, None, None, None, None)
      })
         stateCount = dfa.Qids.size
         transitionCount = dfa.Q.map(q => q.transitions.size).sum
         mindfa <- callWithTimeout(timeout,
           () => minimize(dfa),
           () => {
             println(s"cancelling DFA minimization after ${timeout}ms state-count=${stateCount} transition-count=${transitionCount}")
             report(nodeCount, Some(stateCount), Some(transitionCount), None, None)
           }
         )
         minStateCount = mindfa.Qids.size
         minTransitionCount = mindfa.Q.map(q => q.transitions.size).sum
         } {
      report(nodeCount, Some(stateCount), Some(transitionCount), Some(minStateCount), Some(minTransitionCount))
    }
  }

  def genCsvTunedME(num_repetitions: Int): Unit = {
    genCsvTuned(num_repetitions, avoidEmpty = false, prefix="tunedME", csv = tunedMECsv)
  }

  def genCsvNaive(num_repetitions: Int): Unit = {
    import rte.Random.randomNaiveRteByDepth
    genCsvByDepth(num_repetitions, naiveCsv, prefix="naive",
                  genRte=randomNaiveRteByDepth)
  }

  def genCsvTuned(num_repetitions: Int, avoidEmpty: Boolean = true, prefix:String="tuned",csv: String = tunedCsv): Unit = {
    import rte.Random.randomRte
    genCsvByDepth(num_repetitions, csv=csv, prefix=prefix,
                  genRte=(n: Int) => randomRte(n, avoidEmpty = avoidEmpty))
  }

  def genCsvByDepth(num_repetitions: Int,
                    csv: String,
                    prefix:String,
                    genRte: Int => Rte): Unit = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    for {r <- 0 to num_repetitions} {
      for {m <- 4 to 7
           futures = (m to 8).map((depth) => Future {
             println(s"r=$r m=$m depth=$depth")
             writeCsvStatistic(genRte=()=>genRte(depth), prefix=prefix, csvFileName=csv, probability=None)
           })
           combined = Future.sequence(futures)} {
        Await.result(combined, Duration.Inf)
      }
    }
  }

  def genCsvBySize(num_repetitions: Int,
                   csv: String,
                   prefix:String,
                   genRte: Int => Rte): Unit = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    for {m <- 0 to num_repetitions
         futures = (0 to 5).map((depth) => Future {
           val size = random.between(3, 1<<6)
           println(s"m=$m size=$size")
           writeCsvStatistic(genRte=()=>genRte(size), prefix=prefix, csvFileName=csv, probability=None)
         })
         combined = Future.sequence(futures)} {
      Await.result(combined, Duration.Inf)
    }
  }


  def genCsvBalanced(num_repetitions: Int): Unit = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import rte.Random.randomTotallyBalancedRteByDepth
    for {
      r <- 0 to num_repetitions} {
      for {m <- 4 to 8
           p = 0.90F
           futures = for { depth <- m to 8} yield Future {
             println(s"r=$r m=$m depth=$depth")
             writeCsvStatistic(genRte=() => randomTotallyBalancedRteByDepth(p, depth), prefix="balanced",
                               csvFileName=balancedCsv, probability=Some(p))
           }
           combined = Future.sequence(futures)
           } {
        Await.result(combined, Duration.Inf)
      }
    }
  }



  def readCsvLines(str: String): Vector[CsvLine] = {
    import java.io.InputStream
    import scala.io.{BufferedSource, Source}
    val s: InputStream = getClass.getResourceAsStream(s"/statistics/${str}.csv")
    val fp = Source.createBufferedSource(s)

    val csvlines = fp.getLines()
      .filter(line => line.length > 1 && '#' != line(0)) // skip comments and empty lines
      .map(line => line.split(",").to(Vector))
      .collect {
        case line@Vector(depth, node_count, state_pre_count, transition_pre_count, state_count, transition_count,
        shortest, longest, total, probability) =>
          CsvLine(depth.toInt, node_count.toInt, state_pre_count.toInt, transition_pre_count.toInt, state_count.toInt, transition_count.toInt,
            shortest.toInt, longest.toInt, total.toInt, probability.toDouble)
        case Vector(depth, node_count, state_pre_count, transition_pre_count, state_count, transition_count,
        shortest, longest, total) =>
          CsvLine(depth.toInt, node_count.toInt, state_pre_count.toInt, transition_pre_count.toInt, state_count.toInt, transition_count.toInt,
            shortest.toInt, longest.toInt, total.toInt)
      }
      .to(Vector)
    fp.close()
    // TODO - at the moment we just remove CsvLine objects containing -1, meaning the measurement timed-out.
    csvlines.filter{cl => cl.state_count > 0 && cl.transition_count > 0}
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

  // make plot of y vs x where y = percentage of samples where number of state_counts > x
  def plotThreshold(): Unit = {
    for {str <- Seq("balanced",
      "tuned",
      //"tunedME",
      "naive")
         alllines = readCsvLines(str)
         depthmap = alllines.groupBy(_.depth)
         } {
      // build a map to associate a depth to all CvsLines of that depth
      // we will create one curve for each depth

      val curves = for {(depth, cvslines) <- depthmap
                        xys = genThresholdCurve(cvslines)
                        } yield (s"depth=${depth}", xys.sortBy(_._1))

      gnuPlot(curves.to(Seq))(title = s"Analysis of ${str} for ${alllines.size} Samples",
        xAxisLabel = "DFA state count",
        xLog = true,
        grid = true,
        yLog = false,
        yAxisLabel = "Percentage dfa <= state count",
        view = true)
    }
    for {depth <- (4 to 8)
         descrs = for {str <- Seq("balanced",
           "tuned",
           //"tunedME",
           "naive")
                       alllines = readCsvLines(str).filter(_.depth == depth)
                       xys = genThresholdCurve(alllines)
                       } yield (s"${str} samples=${alllines.length}", xys)
         } gnuPlot(descrs.to(Seq))(title = s"Threshold depth=${depth}",
      xAxisLabel = "DFA state count",
      xLog = true,
      grid = true,
      yLog = false,
      yAxisLabel = "Percentage dfa <= state count",
      view = true)
  }

  def plotAverageCsv(): Unit = {

    for {str <- Seq("tuned", "tunedME", "naive")
         alllines = readCsvLines(str)} {

      val sample_count = alllines.size
      val xys = for {(node_count, tuples) <- alllines.groupBy(_.node_count)
                      state_counts = tuples.map(_.state_count)
                      average = state_counts.sum / state_counts.size.toDouble
                      } yield (node_count.toDouble, average)
      val descrs = Seq((str, xys.to(List).sortBy(_._1)))
      gnuPlot(descrs)(title = f"Average ${str} node count (${sample_count} samples)",
        xAxisLabel = "AST node count",
        yAxisLabel = "DFA state count",
        yLog = true,
        grid = true,
        gnuFileCB = println,
        plotWith = "points",
        view = true)
    }
  }

  def plotBalance1(): Unit = {
    val descrs = for {str <- Seq("tuned", "tunedME", "naive", "balanced")
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
    val descrs = for {str <- Seq("tuned", "tunedME", "naive", "balanced")
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
    val descrs = for {str <- Seq("tuned", "tunedME", "naive", "balanced")
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
}

object GenCsvBalanced {
  def main(argv: Array[String]): Unit = {
    val limit:Int = if (argv.length == 0) 10 else argv(0).toInt
    RteTree.genCsvBalanced(limit)
  }
}

object GenCsvTuned {
  def main(argv: Array[String]): Unit = {
    val limit:Int = if (argv.length == 0) 200 else argv(0).toInt
    RteTree.genCsvTuned(limit)
  }
}

object GenCsvNaive {
  def main(argv: Array[String]): Unit = {
    val limit:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvNaive(limit)
  }
}

object GenCsvTunedME {
  def main(argv: Array[String]): Unit = {
    val limit:Int = if (argv.length == 0) 200 else argv(0).toInt
    RteTree.genCsvTunedME(limit)
  }
}

object BalancePlot {
  def main(array: Array[String]):Unit = {
    RteTree.plotBalance1()
    RteTree.plotBalance2()
    RteTree.plotBalance3()
  }
}

object Plots {
  import demos.scalaio2025.GnuPlot.histogram
  def main(argv: Array[String]): Unit = {
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
  import rte.Random._
  import xymbolyco.GraphViz.dfaView
  import xymbolyco.Minimize.minimize
  import demos.scalaio2025.GnuPlot.imbalanceFactor

  def main(argv: Array[String]): Unit = {
    val depth: Int = if (argv.length == 0) 4 else argv(0).toInt
    for {(algo, gen) <- Seq( ("naive", () => randomNaiveRteBySize(1 << depth)),
                             //("tunedME", () => randomRte(depth, false)),
                             //("tuned", () => randomRte(depth, true)),
                             ("balanced", () => randomTotallyBalancedRteBySize(0.90F, 1 << depth))
                             )
         rte = gen()
         dfa = minimize(rte.toDfa())
         } {
      rteViewAst(rte, title = algo)
      println(rte.measureBalance())
      println(rte.linearize().length)
      println(imbalanceFactor(rte.linearize().length, 1))
      dfaView(dfa, title = algo)
    }
  }
}