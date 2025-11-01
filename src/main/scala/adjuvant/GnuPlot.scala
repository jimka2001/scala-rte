package adjuvant

import adjuvant.Adjuvant.{existingFile, makeTmpFileName}

object GnuPlot {

  val gnuPlotPath: String = existingFile(Seq("/opt/local/bin/gnuplot", "/usr/local/bin/gnuplot"),
                                         "gnuplot")

  def writeCsv(dataToPlot: Seq[(String, Seq[Double], Seq[Double])],
               xAxisLabel: String,
               outputFileBaseName: String,
               verbose: Boolean): Unit = {
    import java.io._
    val csvName = makeTmpFileName(outputFileBaseName, ".csv")
    val csv = new PrintWriter(new File(csvName))
    if (verbose)
      println(s"[writing to $csvName\n")
    // write header
    csv.write(s"$xAxisLabel")
    for {(text, _, _) <- dataToPlot} csv.write(s",$text")
    csv.write(s"\n")
    // get all x values in case they are different per curve
    val uniqXs: List[Double] = (for {(_, xs, _) <- dataToPlot}
      yield xs.toSet).toSet.flatten.toList.sorted
    val dataMaps = for {
      (text, xs, ys) <- dataToPlot
    } yield (text, xs.zip(ys).toMap)
    // print 1 line per uniq x in order, even if some of the y values are missing
    uniqXs.foreach { x =>
      csv.write(s"$x")
      dataMaps.foreach { case (_, mapXtoY) =>
        mapXtoY.get(x) match {
          case None => csv.write(",")
          case Some(y) => csv.write(s",$y")
        }
      }
      csv.write("\n")
    }
    csv.close()
    if (verbose)
      println(s"finished $csvName]")
  }

  case class GnuPlotDescriptor(terminals: Set[String] = Set("png"), // e.g Set("png","tikz")
                               title: String = "",
                               comment: String = "",
                               xAxisLabel: String = "",
                               xLog: Boolean = false,
                               yAxisLabel: String = "",
                               yLog: Boolean = false,
                               grid: Boolean = false,
                               // outputDirName     : String = "/tmp/",
                               // outputFileBaseName is basename without the .pdf, .gnu, .png etc. and without leading path
                               outputFileBaseName: String = "curves",
                               plotWith: String = "linespoints",
                               pointSize: Double = 0.8,
                               key: String = "horizontal bmargin",
                               gnuFileCB: String => Unit = (_) => (),
                               verbose: Boolean,
                               view: Boolean = false) {
    def plot(dataToPlot: Seq[(String, Seq[Double], Seq[Double])]): Unit = {

      // TODO verify that "lines" can be used as plotWith, not 100% sure
      require(Set("linespoints", "points", "lines").contains(plotWith))

      import adjuvant.Accumulators.withOutputToString
      import adjuvant.Adjuvant.openGraphicalFile

      import java.io._

      val gnuName = makeTmpFileName(outputFileBaseName, ".gnu")
      val gnu = new PrintWriter(new File(gnuName))

      writeCsv(dataToPlot, xAxisLabel, outputFileBaseName, verbose)
      if (verbose)
        println(s"[writing to $gnuName\n")
      gnu.write(s"# $comment\n")

      def logCompatible(projection: ((String, Seq[Double], Seq[Double])) => Seq[Double]): Boolean = {
        dataToPlot.forall { data =>
          val numbers = projection(data)
          numbers.forall(_ > 0) && numbers.nonEmpty && (numbers.max > numbers.min)
        }
      }
      // turn off log scale if range on axis is 0 or if it contains 0 or negative values
      if (xLog && logCompatible(_._2))
        gnu.write("set logscale x\n")
      if (yLog && logCompatible(_._3))
        gnu.write("set logscale y\n")
      if ("" != xAxisLabel)
        gnu.write(s"""set xlabel "$xAxisLabel" font ",10"\n""")
      if ("" != yAxisLabel)
        gnu.write(s"""set ylabel "$yAxisLabel" font ",10"\n""")
      if (grid)
        gnu.write(s"set grid\n")
      gnu.write("set key font ',10'\n")
      gnu.write("set xtics font ',10'\n")
      gnu.write("set ytics font ',10'\n")
      if (plotWith == "points") {
        for {i <- dataToPlot.indices} {
          gnu.write(s"set style line ${i+1} pt 7 ps ${pointSize}\n")
        }
      }
      gnu.write(s"set key $key\n") // TODO can also use set key at x,y
      if ("" != title)
        gnu.write(s"""set title "$title" font ",12"\n""")
      gnu.write("plot ")
      val footer: String = withOutputToString { prFooter =>
        val header: String = withOutputToString { prHeader =>
          def write_curve(title: String, i:Int, xys: Seq[(Double, Double)]): Unit = {
            prHeader(""""-" using 1:2""")
            prHeader(s" with $plotWith")
            if (plotWith == "points"){
              prHeader(s" ls $i")
            }
            prHeader(s""" title "$title"""")
            prFooter(s"""#$title\n""")
            xys.foreach { case (x, y) =>
              prFooter(f"$x%.3f $y%.3f\n")
            }
            prFooter("end\n")
          }

          if (dataToPlot.nonEmpty) {
            write_curve(dataToPlot.head._1, 1, dataToPlot.head._2.zip(dataToPlot.head._3))

            dataToPlot.zipWithIndex.tail.foreach { case ((curveTitle, xs, ys),i) =>
              prHeader(",\\\n    ")
              write_curve(curveTitle, i+1, xs.zip(ys))
            }
          }
        }
        gnu.write(s"$header\n")
      }
      gnu.write(footer)
      gnu.close()
      gnuFileCB(gnuName)
      if (verbose)
        println(s"finished $gnuName]")

      if (dataToPlot.exists { data: (String, Seq[Double], Seq[Double]) =>
        data._2.nonEmpty && data._3.nonEmpty
      })
        terminals.foreach { terminal =>

          val outputFileName = makeTmpFileName(outputFileBaseName, terminal)

          if (verbose)
            println(s"[generating $outputFileName")
          gnu.write("\n")
          val exitCode = runGnuPlot(terminal, gnuName, outputFileName)

          if (exitCode != 0)
            println(s"finished $outputFileName with exit code=$exitCode verbose=$verbose]")
          else if (verbose)
            println(s"finished $outputFileName]")
          if (exitCode == 0 && view)
            openGraphicalFile(outputFileName)
        }
    }
  }
  def runGnuPlot(terminal:String, gnuName:String, outputFileName:String):Int = {
    import sys.process._
    import java.io._
    val cmd = Seq(gnuPlotPath, "-e", s"set terminal $terminal", gnuName)
    // println(cmd)
    // println(s" --> $outputFileName")
    val process = Process(cmd,
      None,
      // The LC_CTYPE env var prevents the following diagnostic from gnuplot
      // Fontconfig warning: ignoring UTF-8: not a valid region tag
      "LC_CTYPE" -> "en_US.UTF-8")
    Seq("rm", "-f", outputFileName).!
    (process #>> new File(outputFileName)).!
  }

  // Create a plot using gnuplot (which we assume is stalled
  //  on the system).
  //   dataToPlot specifies a sequence of *curves*:
  //      each curve is designated by a string, naming the curve
  //      and a sequence of x-values and a sequence of y-values, both Double.
  //      The curves will be drawn each in a different color, each curve
  //      will be labeled with its name, in a legend.
  //      Each curve will be drawn either as points, lines, or points-and-lines,
  //         controlled by the parameter plotWith which must have a value,
  //         "linespoints" (default), "points", "lines".
  //  view: indicates whether to graphically display the image file once
  //      it has been created.
  //  gnuFileCB, if given, is a function which will be called given the name
  //      of the .gnu file, i.e., the file containing the gnuplot commands,
  //      before the .png file has been created.  This function may be used
  //      to manipulate the file (add lines, delete lines, etc.).   The function
  //      is allowed to manipulate the file, but is expected to leave a file named
  //      the same in the same directory, which will be used by the system
  //      to create the .png file.
  //  comment: a text string to write as a comment in the .gnu file,
  //       this comment does not have any effect on the plot, but may be
  //       used to distinguish the .gnu file as necessary.
  //  title: optional title to display on the plot file.
  def gnuPlot[A](dataToPlot: Seq[A]
                 // dataToPlot: Seq[(String, Seq[Double], Seq[Double])]
                 // dataToPlot: Seq[(String, Seq[(Double,Double)])]
                )(
                  terminals: Set[String] = Set("png"), // e.g Set("png","tikz")
                  title: String = "",
                  comment: String = "",
                  xAxisLabel: String = "",
                  xLog: Boolean = false,
                  yAxisLabel: String = "",
                  yLog: Boolean = false,
                  grid: Boolean = false,
                  // outputDirName     : String = "/tmp/",
                  // outputFileBaseName is basename without the .pdf, .gnu, .png etc. and without leading path
                  outputFileBaseName: String = "curves",
                  plotWith: String = "linespoints",
                  pointSize:Double = 0.8,
                  key: String = "horizontal bmargin",
                  gnuFileCB: String => Unit = (_) => (),
                  verbose: Boolean = false,
                  view: Boolean = false
                ): Unit = {
    val gpd = GnuPlotDescriptor(terminals = terminals,
                                title = title,
                                comment = comment,
                                xAxisLabel = xAxisLabel,
                                xLog = xLog,
                                yAxisLabel = yAxisLabel,
                                yLog = yLog,
                                grid = grid,
                                outputFileBaseName = outputFileBaseName,
                                plotWith = plotWith,
                                pointSize = pointSize,
                                key = key,
                                gnuFileCB = gnuFileCB,
                                verbose = verbose,
                                view = view)
    // convert dataToPlot to the form Seq[(String,Seq[Double],Seq[Double])]
    val curves = dataToPlot.map {
      case (label: String, xys: Seq[_]) => {
        val pairs = xys.collect { case xy@(x: Any, y: Double) => x match {
          case x: Int => (x.toDouble, y)
          case x: Double => (x, y)
          case _ => throw new NotImplementedError(s"invalid data: $xys, in $dataToPlot")
        }
        }
        Tuple3(label, pairs.map(_._1), pairs.map(_._2))
      }
      case (label: String, xs1: Seq[_], ys1: Seq[_]) => {
        val xs = xs1.collect {
          case x: Double => x
          case x: Int => x.toDouble
          case _ => throw new NotImplementedError(s"invalid data: $xs1, in $dataToPlot")
        }
        val ys = ys1.collect {
          case y: Double => y
          case y: Int => y.toDouble
          case _ => throw new NotImplementedError(s"invalid data: $ys1, in $dataToPlot")
        }
        Tuple3(label, xs, ys)
      }
      case data => throw new NotImplementedError(s"invalid data: $data, in $dataToPlot")
    }
    gpd.plot(dataToPlot = curves)
  }

  def histogram(basename:String,
                xlabel:String,
                ylabel:String,
                title:String,
                gnuFileCB:String=>Unit,
                buckets:Seq[(String,Seq[Int])],
                keepIf:Int=>Boolean,
                otherLabel:String = "other",
                view:Boolean) = {
    import java.io._
    import adjuvant.Adjuvant.{openGraphicalFile}
    assert(! xlabel.contains("\""))
    assert(! xlabel.contains("\\"))
    assert(! ylabel.contains("\""))
    assert(! ylabel.contains("\\"))

    def gnuheader(): String = {
      f"""|
         |set boxwidth 0.9 absolute
         |set style fill solid 1.00 border lt -1
         |set style histogram clustered gap 5 title textcolor lt -1
         |set style data histograms
         |set key outside top center horizontal
         |set xtics rotate by -45
         |set grid
         |set xlabel "$xlabel"
         |set ylabel "$ylabel"
         |set title "$title" font ",10"
         |""".stripMargin +
        s"\n"
    }
    def gnufooter(): String = {
      val line1 = "plot $MyData using 2:xtic(1) ti col,"
      val lines = for {i <- buckets.indices.tail
           } yield "     $MyData using " + (i+2).toString + " ti col,"
      (Seq(line1) ++ lines).mkString(" \\\n")
    }

    val gnuFileName = basename + ".gnu"
    val gnu = new PrintWriter(new File(gnuFileName))
    gnu.write(gnuheader() + "\n\n")
    gnu.write("$MyData << EOD\n")
    gnu.write(s"\"$xlabel\"")
    for {(label, counts) <- buckets
         num_samples = counts.length} gnu.write(s" \"$label samples=${num_samples}\"")
    gnu.write("\n")
    // create map of label -> xys, where xys = Seq((Some(count), percentage), (Some(count), percentage) ...)
    val data: Seq[(String, Seq[(Option[Int], Double)])] = for {(label, counts) <- buckets
                                                               groups = counts.groupBy(n => n).to(Seq).sortBy(_._1)
                                                               xys = for {(state_count, samples) <- groups
                               if keepIf(state_count)
                               percentage = 100.0 * samples.length.toDouble / counts.length
                               } yield (Some(state_count), percentage)
                                                               leftOverPercent = 100.0 - xys.map(_._2).sum
                                                               } yield (label, xys.sortBy(_._1.get) ++ Seq((None, leftOverPercent)))

    val counts = (for {(label, xys) <- data
                      (col, percent) <- xys
                      c <- col} yield c).distinct.sorted.map(c=>Some(c)) ++ Seq(None)
    for {sc1 <- counts
         } {
      val label = sc1 match {
        case Some(p) => p.toString
        case None => otherLabel
      }
      val percentages = for {(_bucket_label, xys) <- data
                         (sc2, percent) <- xys
                         if sc2 == sc1
                         } yield f"$percent%.3f"
      val text = percentages.mkString(" ")
      gnu.write(s"\"$label\" $text \n")
    }
    gnu.write("EOD\n\n")
    gnu.write(gnufooter())
    gnu.close()

    gnuFileCB(gnuFileName)
    runGnuPlot("png", gnuFileName, basename + ".png")

    if(view)
      openGraphicalFile(basename + ".png")
  }

  def sup(view:Boolean) = {
    gnuPlot(dataToPlot = Seq( // gnuPlot can handle list of Xs followed by list of Ys
      ("curve1",
        Seq(1.0, 2.0, 3, 4), // Xs can be Int or Double
        Seq(1.0, 2.0, 2.5, 2.75)),
      // can also handle list of xy pairs
      ("curve2", Seq((1.0, 2.0), // x can be Double
        (2, 2.25), // x can also be Int
        (3, 2.125),
        (4, 2.012)))))(
      view = view)
  }

  def main(argv: Array[String]): Unit = {
    sup(view=false)
  }
}
