package demos.scalaio2025

import adjuvant.Adjuvant.callWithTimeout
import adjuvant.FileLock.callInBlock
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

  def writeCsvStatistic(depth: Int, genRte: (Int => Rte), csvFileName: String): Unit = {
    val rte = genRte(depth)
    val actualSize = rte.linearize().size
    for {dfa <- callWithTimeout(depth * 2000,
                                () => rte.toDfa(true),
                                () => println(s"cancelling after ${depth*2000}ms depth=$depth, csv=$csvFileName"))} {
      val stateCount = dfa.Qids.size
      val transitionCount = dfa.Q.map(q => q.transitions.size).sum
      mergeFile(csvFileName)((outFile: FileWriter) => {
        outFile.write(s"$depth,$actualSize,$stateCount,$transitionCount")
      })
    }
  }

  def main(argv: Array[String]): Unit = {
    val num_repetitions = 300
    for {r <- 0 to num_repetitions
         depth <- 4 to 8} {
      println(s"r=$r depth=$depth")
      writeCsvStatistic(depth, (n: Int) => randomTotallyBalancedRte(0.75F, n), balancedCsv)
      writeCsvStatistic(depth, (n: Int) => randomRte(n), classicCsv)
    }
  }
}
