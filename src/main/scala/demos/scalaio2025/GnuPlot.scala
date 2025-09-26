package demos.scalaio2025

object GnuPlot {
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

  def histogram(): Unit = {
    import java.io._
    def gnuheader(basename: String, depth: Option[Int]): String = {
      val depthComment = depth match {
        case Some(d) => s"of depth=$d"
        case None => ""
      }
      "set terminal pngcairo size 600,400\n" +
        s"set output '${basename}.png'\n" +
        """|
           |set boxwidth 0.9 absolute
           |set style fill solid 1.00 border lt -1
           |set style histogram clustered gap 5 title textcolor lt -1
           |set style data histograms
           |set key outside top center horizontal
           |set xtics rotate by -45
           |set yrange [0:100]
           |""".stripMargin +
        s"set title \"DFA State distribution for Rte " + depthComment + "\"\n"
    }

    def gnufooter(algos:Seq[String]): String = {
      Seq("plot $MyData using 2:xtic(1) ti col,",
          "     $MyData using 3 ti col,",
          "     $MyData using 4 ti col,",
          "     $MyData using 5 ti col").take(algos.length).mkString(" \\\n")
    }

    def histo(depth: Option[Int]): Unit = {
      val counts = (1 to 10).to(List)
      val basename = depth match {
        case Some(d) => s"histogram-${d}"
        case None => "histogram"
      }
      val gnuFileName = basename + ".gnu"
      val gnu = new PrintWriter(new File(gnuFileName))
      val algos = Seq("balanced", "tuned", "tunedME", "naive")

      def readlines(str:String):Vector[CsvLine] = depth match {
        case None => readCsvLines(str)
        case Some(depth) => readCsvLines(str).filter(_.depth == depth)
      }

      gnu.write(gnuheader(basename, depth) + "\n\n")
      gnu.write("$MyData << EOD\n")
      gnu.write("State-count")
      for {str <- algos
           depthlines = readlines(str)
           num_samples = depthlines.length} gnu.write(s" \"$str samples=${num_samples}\"")
      gnu.write("\n")
      val mixed = for {str <- algos
                       depthlines = readlines(str)
                       xys = for {(state_count, sclines) <- depthlines.groupBy(_.state_count).to(Seq).sortBy(_._1)
                                  percentage = 100 * sclines.length.toDouble / depthlines.length
                                  } yield (state_count, percentage)
                       sc_to_percent = xys.to(Map)
                       max_count = counts.max
                       restPercent = (for {(sc, percent) <- xys
                                           if sc > max_count
                                           } yield percent).sum
                       tuples = for {sc <- counts} yield (sc, sc_to_percent.getOrElse(sc, 0.0))
                       } yield (str, restPercent, tuples)
      val outOfRange = (for {(str, restPercent, _) <- mixed
                             } yield (str, restPercent)).to(Map)
      val matrix = for {(str, _, tuples) <- mixed
                        (sc, percent) <- tuples} yield (str, sc, percent)
      val grouped = matrix.groupBy(_._2)

      for {sc <- counts
           triples = grouped(sc)} {
        gnu.write(s"${sc} ")
        for {str <- algos
             triple <- triples
             if str == triple._1
             percent = triple._3
             } gnu.write(f" $percent%.3f")
        gnu.write("\n")
      }
      gnu.write("""">10" """)
      for {str <- algos
           percent = outOfRange(str)} gnu.write(f" $percent%.3f")
      gnu.write("\n")
      gnu.write("EOD\n\n")
      gnu.write(gnufooter(algos))
      gnu.close()
      Seq(adjuvant.GnuPlot.gnuPlotPath, gnuFileName).!
      openGraphicalFile(basename + ".png")
    }

    (4 to 8).foreach(depth => histo(Some(depth)))
    histo(None)
  }

}
