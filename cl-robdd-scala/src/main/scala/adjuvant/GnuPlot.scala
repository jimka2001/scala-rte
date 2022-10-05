package adjuvant

import adjuvant.Adjuvant.makeTmpFileName

object GnuPlot {

  val gnuPlotPath: String = List("/opt/local/bin/gnuplot", "/usr/local/bin/gnuplot").find { fName =>
    import java.nio.file.{Files, Paths}

    Files.exists(Paths.get(fName))
  }.getOrElse("gnuplot")

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
                               // outputFileBaseName is basename without the .pdf, .gnu, .png etc and without leading path
                               outputFileBaseName: String = "curves",
                               plotWith: String = "linespoints",
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
        gnu.write(s"""set xlabel "$xAxisLabel" font ",15"\n""")
      if ("" != yAxisLabel)
        gnu.write(s"""set ylabel "$yAxisLabel" font ",15"\n""")
      if (grid)
        gnu.write(s"set grid\n")
      gnu.write("set key font ',15'\n")
      gnu.write("set xtics font ',15'\n")
      gnu.write("set ytics font ',15'\n")
      gnu.write(s"set key $key\n") // TODO can also use set key at x,y
      if ("" != title)
        gnu.write(s"""set title "$title"\n""")
      gnu.write("plot ")
      val footer: String = withOutputToString { prFooter =>
        val header: String = withOutputToString { prHeader =>
          def plot(title: String, xys: Seq[(Double, Double)]): Unit = {
            prHeader(""""-" using 1:2""")
            prHeader(s" with $plotWith")
            prHeader(s""" title "$title"""")
            prFooter(s"""#$title\n""")
            xys.foreach { case (x, y) =>
              prFooter(s"$x $y\n")
            }
            prFooter("end\n")
          }

          if (dataToPlot.nonEmpty) {
            plot(dataToPlot.head._1, dataToPlot.head._2.zip(dataToPlot.head._3))

            dataToPlot.tail.foreach { case (curveTitle, xs, ys) =>
              prHeader(",\\\n    ")
              plot(curveTitle, xs.zip(ys))
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
          import sys.process._
          val outputFileName = makeTmpFileName(outputFileBaseName, terminal)

          if (verbose)
            println(s"[generating $outputFileName")
          gnu.write("\n")
          val process = Process(Seq(gnuPlotPath, "-e", s"set terminal $terminal", gnuName),
                                None,
                                // The LC_CTYPE env var prevents the following diagnostic from gnuplot
                                // Fontconfig warning: ignoring UTF-8: not a valid region tag
                                "LC_CTYPE" -> "en_US.UTF-8")
          val exitCode = (process #>> new File(outputFileName)).!

          if (exitCode != 0)
            println(s"finished $outputFileName with exit code=$exitCode verbose=$verbose]")
          else if (verbose)
            println(s"finished $outputFileName]")
          if (exitCode == 0 && view)
            openGraphicalFile(outputFileName)
        }
    }
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
  //      to manipulate the file (add lines, delete lines, etc).   The function
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
    // outputFileBaseName is basename without the .pdf, .gnu, .png etc and without leading path
    outputFileBaseName: String = "curves",
    plotWith: String = "linespoints",
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
                                key = key,
                                gnuFileCB = gnuFileCB,
                                verbose = verbose,
                                view = view)
    // convert dataToPlot to the form Seq[(String,Seq[Double],Seq[Double])]
    val curves = dataToPlot.map{
      case (label:String,xys:Seq[(Any,Double)]) => Tuple3(label,xys.map {
        case (x:Double,_) => x
        case (x:Int,_) => x.toDouble
        case data => throw new NotImplementedError(s"invalid data: $data, in $dataToPlot")
      } ,xys.map(_._2))
      case (label:String,xs:Seq[Any],ys:Seq[Double]) =>
        Tuple3(label,
               xs.map{
                 case x:Double => x
                 case x:Int => x.toDouble
                 case data => throw new NotImplementedError(s"invalid data: $data, in $dataToPlot")
               },ys)
      case data => throw new NotImplementedError(s"invalid data: $data, in $dataToPlot")
    }
    gpd.plot(dataToPlot=curves)
  }

  def main(argv:Array[String]):Unit = {
    gnuPlot(dataToPlot=Seq(// gnuPlot can handle list of Xs followed by list of Ys
                           ("curve1",
                             Seq(1.0, 2.0, 3, 4), // Xs can be Int or Double
                             Seq(1.0, 2.0, 2.5, 2.75)),
                           // can also handle list of xy pairs
                           ("curve2", Seq((1.0, 2.0), // x can be Double
                                          (2, 2.25),  // x can also be Int
                                          (3, 2.125),
                                          (4, 2.012)))))(
            view=true)
  }
}
