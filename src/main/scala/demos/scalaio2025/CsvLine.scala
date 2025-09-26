package demos.scalaio2025


case class CsvLine(depth: Int,
                   node_count: Int,
                   state_pre_count:Int,
                   transition_pre_count:Int,
                   state_count: Int,
                   transition_count: Int,
                   shortest:Int,
                   longest:Int,
                   total:Int) {
  def imbalance():Double = {
    CsvLine.imbalanceFactor(node_count, total)
  }
}

object CsvLine {
  import java.nio.file.Paths
  import java.io.FileWriter
  import java.util.UUID
  import scala.sys.process.stringSeqToProcess
  import adjuvant.Adjuvant.{callWithTimeout, openGraphicalFile}
  import rte.Rte

  val statisticsResource: String = Paths.get("src/main/resources/statistics").toString + "/"

  def imbalanceFactor(node_count:Int, total:Int):Double = {
    // compute an imbalance factor.
    // 1 ==> perfectly balanced
    // > 1 ==> average path length > perfect path length
    // < 1 ==> average path length < perfect path length
    import scala.math.log
    if (node_count == 1 && total == 0)
      1.0
    else {
      // number of total nodes, leaf + internal = 2^d - 1 + 2^d
      val d = log(node_count + 1) / log(2)
      // balanced path total length from root to leaf = num leafs * average length i.e. n*d
      val imbalanced_total = d * node_count
      //println(f"node_count = $node_count")
      //println(f"total = $total")
      //println(f"d=$d imbalanced_total=$balanced_total")
      val ibf = imbalanced_total / total.toDouble
      //println(f"bf = $bf")
      assert(total > 0)
      ibf
    }
  }


  def withLock[A](lockFile: String, f: () => A): A = {
    import adjuvant.FileLock.callInBlock
    callInBlock(lockFile + ".lock")(f)
  }

  /**
   * @param csvFileName name of a CSV file in resources/statistics
   * @param writeRecord function that writes one record into the given FileWriter
   */
  def mergeFile(csvFileName: String, prefix: String)(writeRecord: FileWriter => Unit): Unit = {
    // create temporary files ending in ~ so that .gitignore will ignore them.
    val tmp1 = statisticsResource + prefix + "-" + UUID.randomUUID().toString + "~"
    val tmp2 = statisticsResource + prefix + "-" + UUID.randomUUID().toString + "~"

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

  def writeCsvStatistic(genRte: (() => Rte), prefix:String, csvFileName: String): Unit = {

    import xymbolyco.Minimize.minimize
    val rte = genRte()
    val nodeCount = rte.linearize().size
    val (shortest, longest, total) = rte.measureBalance()
    def report(nodeCount:Int, stateCount:Option[Int], transitionCount:Option[Int],
               minStateCount:Option[Int], minTransitionCount:Option[Int]):Unit = {

      mergeFile(csvFileName,prefix)((outFile: FileWriter) => {
        outFile.write(s"$nodeCount,")
        for {op <- Seq(stateCount, transitionCount, minStateCount, minTransitionCount)
             } outFile.write(
          op match {
            case Some(s) => s"$s,"
            case None => "-1,"
          })
        outFile.write(f"$shortest,$longest,$total")

      })
    }

    val timeout = Seq(longest * longest * 4000, 5000).max
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


  def readCsvLines(str: String): Vector[CsvLine] = {
    import java.io.InputStream
    import scala.io.{Source}
    val s: InputStream = getClass.getResourceAsStream(s"/statistics/${str}.csv")
    val fp = Source.createBufferedSource(s)

    val csvlines = fp.getLines()
      .filter(line => line.length > 1 && '#' != line(0)) // skip comments and empty lines
      .map(line => line.split(",").to(Vector))
      .collect {
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
}