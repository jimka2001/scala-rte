package demos.scalaio2025

import adjuvant.Adjuvant.callWithTimeout
import adjuvant.FileLock.callInBlock
import adjuvant.GnuPlot.gnuPlot
import genus.SimpleTypeD
import rte.Rte
import rte.Rte.{randomRte, randomTotallyBalancedRte, rteViewAst, rteViewDfa}
import xymbolyco.Dfa

import java.io.FileWriter
import java.nio.file.Paths
import java.util.UUID

import scala.sys.process.stringSeqToProcess

object Scalaio2025 {

  val statisticsResource: String = Paths.get("src/main/resources/statistics").toString + "/"
  val lockFile = statisticsResource + "statistics.lockfile"
  val classicCsv = statisticsResource + "classic.csv"
  val balancedCsv = statisticsResource + "balanced.csv"

  def withLock[A](f: () => A): A = {
    import adjuvant.FileLock.callInBlock
    callInBlock(lockFile)(f)
  }


  /**
   * @param csvFileName name of a CSV file in resources/statistics
   * @param writeRecord function that writes one record into the given FileWriter
   */
  def mergeFile(csvFileName: String)(writeRecord: FileWriter => Unit): Unit = {
    val tmp1 = statisticsResource + UUID.randomUUID().toString
    val tmp2 = statisticsResource + UUID.randomUUID().toString

    // write one record into tmp1
    val outFile = new FileWriter(tmp1, true)
    try {
      writeRecord(outFile)
    } finally {
      outFile.close()
    }

    // critical section: merge tmp1 with the CSV and update it atomically
    withLock(() => {
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

  def writeCsvStatistic(depth: Int, genRte: (Int => Rte), csvFileName: String, probability:Option[Float]): Unit = {
    val rte = genRte(depth)
    val actualSize = rte.linearize().size
    for {dfa <- callWithTimeout(depth * 2000,
                                () => rte.toDfa(true),
                                () => println(s"cancelling after ${depth * 2000}ms depth=$depth, csv=$csvFileName"))} {
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

  def genCsv1(num_repetitions:Int) = {
    for {r <- 0 to num_repetitions
         depth <- 4 to 8} {
      println(s"r=$r depth=$depth")
      writeCsvStatistic(depth, (n: Int) => randomTotallyBalancedRte(0.75F, n), balancedCsv, Some(0.75F))
      writeCsvStatistic(depth, (n: Int) => randomRte(n), classicCsv, None)
    }
  }

  def genCsvBalanced(num_repetitions:Int) = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    for {
      r <- 0 to num_repetitions
      depth <- 4 to 8

    } {
      val futures = for {p <- List(0.25F, 0.50F, 0.75F, 0.90F) } yield Future {
        println(s"r=$r depth=$depth, percentage=$p")
        writeCsvStatistic(depth, (n: Int) => randomTotallyBalancedRte(0.75F, n), balancedCsv, Some(p))
      }
      val combined = Future.sequence(futures)
      Await.result(combined,Duration.Inf)
    }
  }

  def plotBalancedCsv() = {
    import java.io.InputStream
    import scala.io.{BufferedSource, Source}
    val s: InputStream = getClass.getResourceAsStream(s"/statistics/balanced.csv")
    val fp = Source.createBufferedSource(s)

    val tuples = fp.getLines()
      .map(line => line.split(",").to(Vector))
      .map{case Vector(depth, node_count, state_count, transition_count, probability) =>
        (depth.toInt, node_count.toInt, state_count.toInt, transition_count.toInt, probability.toFloat) }
      .to(Vector)
    val descr = for { (percentage, tuples) <- tuples.groupBy(_._5)
                      xys = for {(node_count, tuples) <- tuples.groupBy(_._2)
                                 state_counts = tuples.map(_._3)
                                 average = state_counts.sum / state_counts.size.toFloat
                                 } yield (node_count.toDouble, average.toDouble)
                      } yield (s"percentage=$percentage", xys.to(List).sortBy(_._1))

    gnuPlot(descr.to(Seq))(title="balanced node count", view=true)
  }

  def main(argv: Array[String]): Unit = {
    for (_ <- 0 to 5) {
      genCsvBalanced(100)
      plotBalancedCsv()
    }
  }
}
