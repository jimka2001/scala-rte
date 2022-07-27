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

package Ifl

// Code associated with my submission to IFL 2022
// IFL 2022 = The 34th Symposium on Implementation and Application of Functional Languages

import spire.math.Rational
import treereduce.RationalFoldTest.rationalFoldTest
import treereduce.TreeReduce.RichReducible
import scala.math.abs

object Ifl2022 {
  def rationalAdd(a: Rational, b: Rational): Rational = a + b
  val rationalZero = Rational(0, 1)
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
        title = "Fold Strategy Accuracy of Floating Point Addition",
        comment = "Fold Strategy Accuracy of Floating Point Addition",
        xAxisLabel = "Number of terms", xLog = true,
        yAxisLabel = "Error", yLog = true,
        grid = true,
        outputFileBaseName = "float-accuracy",
        verbose = true,
        view = true
        )
    }
  }

  // rational sums
  def rationalSums():Unit = {
    // Rational sums
    rationalFoldTest(5000, verbose = true)
  }

  def main(argv: Array[String]): Unit = {
    rationalSums()
    floatSums()
  }
}
