package demos.scalaio2025

import adjuvant.Adjuvant.callWithTimeout
import adjuvant.GnuPlot.gnuPlot
import rte.Rte

import java.io.FileWriter
import java.nio.file.Paths
import java.util.UUID
import scala.sys.process.stringSeqToProcess

object Scalaio2025 {

  val statisticsResource: String = Paths.get("src/main/resources/statistics").toString + "/"
  val naiveCsv:String = statisticsResource + "naive.csv"
  val tunedCsv:String = statisticsResource + "tuned.csv"
  val tunedMECsv:String = statisticsResource + "tunedME.csv" // tuned but maybe empty = true
  val balancedCsv:String = statisticsResource + "balanced.csv"

  def withLock[A](lockFile:String, f: () => A): A = {
    import adjuvant.FileLock.callInBlock
    callInBlock(lockFile + ".lock")(f)
  }


  /**
   * @param csvFileName name of a CSV file in resources/statistics
   * @param writeRecord function that writes one record into the given FileWriter
   */
  def mergeFile(csvFileName: String, prefix:String="")(writeRecord: FileWriter => Unit): Unit = {
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

  def writeCsvStatistic(depth: Int, genRte: (Int => Rte), csvFileName: String, probability: Option[Float]): Unit = {
    val rte = genRte(depth)
    val actualSize = rte.linearize().size
    val timeout = depth*4000
    for {dfa <- callWithTimeout(timeout,
                                () => rte.toDfa(true),
                                () => println(s"cancelling after ${timeout}ms depth=$depth, csv=$csvFileName"))} {
      val stateCount = dfa.Qids.size
      val transitionCount = dfa.Q.map(q => q.transitions.size).sum
      mergeFile(csvFileName)((outFile: FileWriter) => {
        probability match {
          case Some(probability) =>
            outFile.write(s"$depth,$actualSize,$stateCount,$transitionCount,$probability")
          case None =>
            outFile.write(s"$depth,$actualSize,$stateCount,$transitionCount")
        }
      })
    }
  }


  def genCsvClassicME(num_repetitions: Int): Unit = {
    genCsvClassic(num_repetitions, avoidEmpty=false, csv=tunedMECsv)
  }

  def genCsvNaive(num_repetitions: Int): Unit = {
    import rte.Random.randomNaiveRte
    genCsv(num_repetitions, naiveCsv, randomNaiveRte)
  }

  def genCsvClassic(num_repetitions: Int, avoidEmpty:Boolean=true,csv:String=tunedCsv): Unit = {
    import rte.Random.randomRte
    genCsv(num_repetitions, csv, (n:Int) => randomRte(n, avoidEmpty=avoidEmpty))
  }

  def genCsv(num_repetitions: Int,
             csv:String=tunedCsv,
             genRte:(Int=>Rte)): Unit = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    for {r <- 0 to num_repetitions} {
      for {m <- 4 to 7
           futures = (m to 8).map((depth) => Future {
             println(s"r=$r m=$m depth=$depth")
             writeCsvStatistic(depth, genRte, csv, None)
           })
           combined = Future.sequence(futures)} {
        Await.result(combined, Duration.Inf)
      }
    }
  }



  def genCsvBalanced(num_repetitions: Int): Unit = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import rte.Random.randomTotallyBalancedRte
    for {
      r <- 0 to num_repetitions} {
      for {m <- 4 to 8
           depth <- m to 8
           futures = for {p <- List(0.25F, 0.50F, 0.75F, 0.90F)} yield Future {
             println(s"r=$r m=$m depth=$depth, percentage=$p")
             writeCsvStatistic(depth, (n: Int) => randomTotallyBalancedRte(0.75F, n), balancedCsv, Some(p))
           }
           combined = Future.sequence(futures)
           } {
        Await.result(combined, Duration.Inf)
      }
    }
  }

  case class CsvLine(depth: Int, node_count: Int, state_count: Int, transition_count: Int, probability: Float = 0.0F)

  def genThresholdCurve(cvslines:Seq[CsvLine]):Seq[(Double,Double)] = {
    val num_per_depth = cvslines.size
    val state_count_to_dfa_count = cvslines
      .groupBy(_.state_count)
      .collect { case (state_count, cvslines) => (state_count, cvslines.size) }
    val xys = for {(this_state_count, _) <- state_count_to_dfa_count.toList
         // compute percentage of cvslines which have state_count > this_state_count
         // 1st, how many cvslines have state_count > this_state_count
         n = state_count_to_dfa_count.collect { case (state_count, count) if count <= this_state_count => count }.sum
         } yield (this_state_count.toDouble, 100.0 * n.toDouble / num_per_depth)
    xys.sortBy(_._1)
  }

  def readCsvLines(str:String, width:Int):Vector[CsvLine] = {
    import java.io.InputStream
    import scala.io.{BufferedSource, Source}
    val s: InputStream = getClass.getResourceAsStream(s"/statistics/${str}.csv")
    val fp = Source.createBufferedSource(s)

    val csvlines = fp.getLines()
      .map(line => line.split(",").to(Vector).take(width))
      .collect { case Vector(depth, node_count, state_count, transition_count, probability) =>
        CsvLine(depth.toInt, node_count.toInt, state_count.toInt, transition_count.toInt, probability.toFloat)
      case Vector(depth, node_count, state_count, transition_count) =>
        CsvLine(depth.toInt, node_count.toInt, state_count.toInt, transition_count.toInt)
      }
      .to(Vector)
    fp.close()
    csvlines
  }

  // make plot of y vs x where y = percentage of samples where number of state_counts > x
  def plotThreshold(): Unit = {
    import scala.io.Source

    for {str <- Seq("balanced", "tuned", "tunedME", "naive")
         alllines = readCsvLines(str, 4)
         } {
      // build a map to associate a depth to all CvsLines of that depth
      // we will create one curve for each depth
      val depthmap = alllines.groupBy(_.depth)
      val curves = for {(depth, cvslines) <- depthmap
                        xys = genThresholdCurve(cvslines)
                        } yield (s"depth=${depth}", xys.sortBy(_._1))

      gnuPlot(curves.to(Seq))(title = s"Analysis of ${str} for ${alllines.size} Samples",
                              xAxisLabel = "DFA state count",
                              xLog = true,
                              grid = true,
                              yLog = true,
                              yAxisLabel = "Percentage dfa <= state count",
                              view = true)
    }
    for {depth <- (4 to 8)
         descrs = for {str <- Seq("balanced", "tuned", "tunedME", "naive")
                       alllines = readCsvLines(str, 4).filter(_.depth == depth)
                       xys = genThresholdCurve(alllines)
                       } yield (s"${str} samples=${alllines.length}", xys)
         } gnuPlot(descrs.to(Seq))(title = s"Threshold depth=${depth}",
                                   xAxisLabel = "DFA state count",
                                   xLog = true,
                                   grid = true,
                                   yLog = true,
                                   yAxisLabel = "Percentage dfa <= state count",
                                   view = true)
  }


  def plotAverageCsv(): Unit = {

    val alllines = readCsvLines("balanced", 5)
    val sample_count = alllines.size
    val descrs = for {(percentage, tuples) <- alllines.groupBy(_.probability)
                      xys = for {(node_count, tuples) <- tuples.groupBy(_.node_count)
                                 state_counts = tuples.map(_.state_count)
                                 average = state_counts.sum / state_counts.size.toDouble
                                 } yield (node_count.toDouble, average)
                      descr = (s"percentage=$percentage", xys.to(List).sortBy(_._1))
                      _ = gnuPlot(Seq(descr))(title = s"Average balanced percentage=${percentage} node count (${tuples.size} samples)",
                                              xAxisLabel = "AST node count",
                                              yAxisLabel = "DFA state count",
                                              yLog = true,
                                              grid = true,
                                              plotWith = "points",
                                              view = true)
                      } yield descr

    gnuPlot(descrs.to(Seq))(title = s"Average balanced node count (${sample_count} samples)",
                            xAxisLabel = "AST node count",
                            yAxisLabel = "DFA state count",
                            yLog = true,
                            grid = true,
                            plotWith = "points",
                            view = true)

    for {str <- Seq("tuned", "tunedME", "naive")
         alllines = readCsvLines(str, 4)} {

      val sample_count = alllines.size
      val xys = (for {(node_count, tuples) <- alllines.groupBy(_.node_count)
                     state_counts = tuples.map(_.state_count)
                     average = state_counts.sum / state_counts.size.toDouble
                     } yield (node_count.toDouble, average))
      val descr = Seq((str, xys.to(List).sortBy(_._1)))
      gnuPlot(descr)(title = f"Average ${str} node count (${sample_count} samples)",
                     xAxisLabel = "AST node count",
                     yAxisLabel = "DFA state count",
                     yLog = true,
                     grid = true,
                     plotWith = "points",
                     view = true)
    }
  }
}

object GenCsvBalanced {
  def main(argv: Array[String]): Unit = {
    val limit:Int = (if (argv.length == 0) 500 else argv(0).toInt)
    Scalaio2025.genCsvBalanced(limit)
  }
}

object GenCsvClassic {
  def main(argv: Array[String]): Unit = {
    val limit:Int = (if (argv.length == 0) 500 else argv(0).toInt)
    Scalaio2025.genCsvClassic(limit)
  }
}

object GenCsvNaive {
  def main(argv: Array[String]): Unit = {
    val limit:Int = (if (argv.length == 0) 5 else argv(0).toInt)
    Scalaio2025.genCsvNaive(limit)
  }
}

object GenCsvClassicME {
  def main(argv: Array[String]): Unit = {
    val limit:Int = (if (argv.length == 0) 500 else argv(0).toInt)
    Scalaio2025.genCsvClassicME(limit)
  }
}

object Plots {
  def main(argv: Array[String]): Unit = {
    Scalaio2025.plotThreshold()
    Scalaio2025.plotAverageCsv()
  }
}