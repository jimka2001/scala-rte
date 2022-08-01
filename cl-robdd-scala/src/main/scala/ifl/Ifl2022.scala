// Copyright (©) 2022 EPITA Research and Development Laboratory
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

package ifl

// Code associated with my submission to IFL 2022
// IFL 2022 = The 34th Symposium on Implementation and Application of Functional Languages

import adjuvant.Adjuvant
import adjuvant.Adjuvant.{copyFile, filterFile}
import graphcolor.MapColoring.timeColorizeGraphs
import spire.math.Rational
import treereduce.RationalFoldTest.rationalFoldTest
import treereduce.TreeReduce.RichReducible

import scala.math.abs

object Ifl2022 {
  val gnuPlotDataDirName = "/Users/jnewton/Repos/research/gnuplot/"
  val dotDirName = "/Users/jnewton/Repos/research/dot/"
  def rationalAdd(a: Rational, b: Rational): Rational = a + b
  val rationalZero = Rational(0, 1)

  def suppressGnuTitle(line: String): Boolean = {
    !line.startsWith("set title")
  }

  def suppressDotTitle(line: String): Boolean = {
    !(line.startsWith("  labelloc=") || line.startsWith("  label="))
  }
  // measure round-off error
  def floatAddition(n:Int):(Rational,Double,Double) = {
    val tenth = Rational(1,10)
    val exact = (1 to n).foldLeft(rationalZero)((acc:Rational,i:Int) =>
                                                  rationalAdd(acc,rationalAdd(i,tenth)))

    val byFoldLeft = (1 to n).foldLeft(0.0)((acc:Double,i:Int) =>
                                            acc + (i + 0.1))

    val byTreeFold = (1 to n).treeMapReduce(0.0)((i:Int)=>(i.toDouble + 0.1),
                                                 (acc:Double,d:Double)=> acc + d)

    (exact,byFoldLeft,byTreeFold)
  }

  def floatSums(n:Int=50000):Unit = {
    import gnuplot.GnuPlot.gnuPlot
    def indices(scale:Double, data:List[Int]):List[Int] = {
      data match {
        case a::as if a > n => as.reverse
        case a::_ => indices(scale, (a * scale).round.toInt :: data)
      }
    }

    val rawData = for{k<- indices(1.1, List(100))
                      (exact, byFoldLeft, byTreeFold) = floatAddition(k)
                      if exact != byFoldLeft
                      if exact != byTreeFold
        } yield (k, exact, byFoldLeft, byTreeFold)

    {
      val xs = rawData.map(_._1).map(_.toDouble)
      val dataToPlot:List[(String, Seq[Double], Seq[Double])] =
        List(("fold-left",xs,rawData.map{case (_,exact,fold,_) => abs(exact.toDouble - fold) }),
             ("tree-fold",xs,rawData.map{case (_,exact,_,tree) => abs(exact.toDouble - tree)}))
      gnuPlot(dataToPlot)(
        title = "Fold Strategy Error of Floating Point Addition",
        comment = "Fold Strategy Error of Floating Point Addition",
        xAxisLabel = "Number of terms added", xLog = true,
        yAxisLabel = "Error", yLog = true,
        grid = true,
        key = "inside left",
        outputFileBaseName = "float-accuracy",
        gnuFileCB = (gnuName:String) => filterFile(gnuName,
                                                   gnuPlotDataDirName + "float-accuracy.gnu",
                                                   suppressGnuTitle),

        verbose = true,
        view = true
        )
    }
  }

  // rational sums
  def rationalSums():Unit = {
    // Rational sums
    // summing 1/n from n=-1000 to n=1000 (excluding 1/0)
    //   in sorted order and randomized order
    rationalFoldTest(1000,
                     verbose = true,
                     randomize=true,
                     gnuFileCB=(fn:String)=>{
                       filterFile(fn, gnuPlotDataDirName+"ifl-rational-addition-random.gnu",
                                  suppressGnuTitle)
                     })
    rationalFoldTest(1000,
                     verbose = true,
                     randomize=false,
                     gnuFileCB=(fn:String)=>{
                       filterFile(fn, gnuPlotDataDirName+"ifl-rational-addition.gnu",
                                  suppressGnuTitle)
                     })
  }

  def fourColor():Unit = {
    import graphcolor.sampleColoring.europeTimedMapColoringTest
    def captureGnu(fn:String):Unit = {
      val worked = fn
        .replaceAll("-[0-9]+.gnu", ".gnu")
        .replaceAll("^/.*/europe-[0-9]+-", gnuPlotDataDirName + "europe-")
      println(s"    fn = $fn")
      println(s"worked = $worked")
      filterFile(fn, worked, suppressGnuTitle)
    }
    europeTimedMapColoringTest(30,
                               gnuFileCB=captureGnu ,
                               view=false,
                               verbose=true)
    // need to also make some measurements of time of 4-coloring
    //  1. for different sized graphs
    //  2. without instrumentation.
    //     i.e., does the instrumentation effect the computation time.
  }

  def drawMapConstraints():Unit = {
    import bdd.{Bdd, Or, Xor, And}
    import bdd.GraphViz.GraphVizOps

    // france <-> spain
    // Af != As  || Bf != Bs
    val Af = 1
    val Bf = 2
    val As = 3
    val Bs = 4
    val Ag = 5
    val Bg = 6
    def labelToString(k:Int):String = {
      val ar = Array("no-label",
                     "<A<sub>FR</sub>>","<B<sub>FR</sub>>",
                     "<A<sub>ES</sub>>","<B<sub>ES</sub>>",
                     "<A<sub>DE</sub>>","<B<sub>DE</sub>>"
                     )
      ar(k)
    }
    def dotFileCB(fn:String):String=>Unit = {
      (rawDotFileName:String) => {
        println(s"fn = $fn")
        filterFile(rawDotFileName,
                   dotDirName + fn,
                   suppressDotTitle
                   )
        ()
      }
    }
    Bdd.withNewBddHash {
      val bddFS = Or(Xor(Af, As), Xor(Bf, Bs))
      val bddFG = Or(Xor(Af, Ag), Xor(Bf, Bg))
      bddFS.bddView(drawFalseLeaf = true, title = "France -> Spain",
                    labelToString=labelToString,
                    dotFileCB=dotFileCB("france-spain.dot"),
                    htmlLabels=true)
      bddFG.bddView(drawFalseLeaf = true, title = "France -> Germany",
                    labelToString=labelToString,
                    dotFileCB=dotFileCB("france-germany.dot"),
                    htmlLabels=true)
      And(bddFS, bddFG).bddView(drawFalseLeaf = true, title = "2 borders",
                                labelToString=labelToString,
                                dotFileCB=dotFileCB("france-spain-germany.dot"),
                                htmlLabels=true)
    }
  }

  def timeColorGraph():Unit = {
    timeColorizeGraphs(30,
                       gnuFileCB = (fn: String) => {
                         println(s"fn = $fn")
                         if (fn.matches(".*time-per-num-states.*"))

                           filterFile(fn, gnuPlotDataDirName + "time-per-num-states.gnu",
                                      suppressGnuTitle)
                         else if (fn.matches(".*time-ratio-num-states.*"))
                           filterFile(fn, gnuPlotDataDirName + "time-ratio-num-states.gnu",
                                      suppressGnuTitle)
                       }
                       )
  }

  def main(argv: Array[String]): Unit = {
    // produce files
    //   ifl-rational-addition-random.gnu
    //   ifl-rational-addition.gnu
    rationalSums()

    // produce file
    //    float-accuracy.gnu
    floatSums()

    // produce files
    //   europe-4-color-allocation.gnu
    //   europe-4-color-gc-count.gnu
    //   europe-4-color-gc-time.gnu
    //   europe-4-color-hash-size.gnu
    //   europe-4-color-num-allocations.gnu
    //   europe-4-color-reclaimed.gnu
    //   europe-4-color-time.gnu
    fourColor()

    // produce files
    //   time-per-num-states.gnu
    //   time-ratio-num-states.gnu
    timeColorGraph()

    // produce file
    //   france-spain.dot
    //   france-germany.dot
    //   france-spain-germany.dot
    drawMapConstraints()
  }
}
