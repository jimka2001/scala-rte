// Copyright (c) 2019 EPITA Research and Development Laboratory
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// A significant portion of this typeclass definition was supplied by
// Justin du Coeur @jducoeur.
// The original text https://scastie.scala-lang.org/K2ar8VdMTpSUXqeJLkZcsQ
// was modified by @jducoeur https://scastie.scala-lang.org/9EFL87qySHSNMmG93LIvMQ
//

package gnuplot

object GnuPlot {

  var gnuPlotPath: String = List("/opt/local/bin/gnuplot", "/usr/local/bin/gnuplot").find { fName =>
    import java.nio.file.{Paths, Files}

    Files.exists(Paths.get(fName))
  }.getOrElse("gnuplot")

  def writeCsv(dataToPlot: List[(String, List[Double], List[Double])],
               xAxisLabel:String,
               outputDirName     : String,
               outputFileBaseName: String) = {
    require(outputDirName.takeRight(1) == "/")
    import java.io._
    val csvName = s"${outputDirName}${outputFileBaseName}.csv"
    val csv = new PrintWriter(new File(csvName))
    // println(s"[writing to $csvName\n")
    // write header
    csv.write(s"${xAxisLabel}")
    for {(text, _, _) <- dataToPlot} csv.write(s",${text}")
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
      dataMaps.foreach { case (text, mapXtoY) =>
        mapXtoY.get(x) match {
          case None => csv.write(",")
          case Some(y) => csv.write(s",$y")
        }
      }
      csv.write("\n")
    }
    csv.close()
    //println(s"finished $csvName]")
  }
  def gnuPlot(dataToPlot: List[(String, List[Double], List[Double])])(
              terminals: Set[String]=Set("png"), // e.g Set("png","tikz")
              title             : String = "",
              comment           : String = "",
              xAxisLabel: String = "",
              xLog: Boolean = false,
              yAxisLabel        : String="",
              yLog              : Boolean=false,
              grid              : Boolean=false,
              outputDirName     : String = "/tmp/",
              // outputFileBaseName is basename without the .pdf, .gnu, .png etc and without leading path
              outputFileBaseName: String = "curves",
              plotWith          : String = "linespoints",
              key:String = "horizontal bmargin"
             ): Unit = {
    // TODO verify that "lines" can be used as plotWith, not 100% sure
    require(Set("linespoints", "points", "lines").contains(plotWith))
    assert(outputDirName.takeRight(1) == "/", s"outputDirName=$outputDirName must end with /")

    import java.io._
    import cl.Accumulators.withOutputToString

    val gnuName = s"${outputDirName}${outputFileBaseName}.gnu"
    val gnu = new PrintWriter(new File(gnuName))

    writeCsv(dataToPlot,xAxisLabel,outputDirName,outputFileBaseName)
    //println(s"[writing to $gnuName\n")
    gnu.write(s"# $comment\n")
    if (xLog)
      gnu.write("set logscale x\n")
    if (yLog)
      gnu.write("set logscale y\n")
    if ("" != xAxisLabel)
      gnu.write(s"""set xlabel "${xAxisLabel}"\n""")
    if ("" != yAxisLabel)
      gnu.write(s"""set ylabel "${yAxisLabel}"\n""")
    if (grid)
      gnu.write(s"set grid\n")

    gnu.write(s"set key $key\n") // TODO can also use set key at x,y
    if ("" != title)
      gnu.write(s"""set title "$title"\n""")
    gnu.write("plot ")
    val footer: String = withOutputToString { prFooter =>
      val header: String = withOutputToString { prHeader =>
        def plot(title: String, xys: List[(Double, Double)]): Unit = {
          prHeader(""""-" using 1:2""")
          prHeader(s" with ${plotWith}")
          prHeader(s""" title "$title"""")
          prFooter(s"""#${title}\n""")
          xys.foreach { case (x, y) =>
            prFooter(s"$x $y\n")
          }
          prFooter("end\n")
        }

        if (dataToPlot.nonEmpty) {
          plot(dataToPlot.head._1, dataToPlot.head._2.zip(dataToPlot.head._3))

          dataToPlot.tail.foreach { case (curveTitle, xs, ys) => {
            prHeader(",\\\n    ")
            plot(curveTitle, xs.zip(ys))
          }
          }
        }
      }
      gnu.write(s"$header\n")
    }
    gnu.write(footer)
    gnu.close()
    //println(s"finished $gnuName]")

    terminals.foreach { terminal=>
      import sys.process._
      var outputFileName = s"${outputDirName}${outputFileBaseName}.${terminal}"

      //println(s"[generating $outputFileName")
      val exitCode = (Seq(gnuPlotPath, "-e", s"set terminal ${terminal}", gnuName) #>> new File(outputFileName)).!

      //if (exitCode != 0)
      //  println(s"finished $outputFileName with exit code=$exitCode]")
      //else
      //  println(s"finished $outputFileName]")
    }
  }

}
