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

package bdd

object Histogram {

  import bdd.Bdd._
  import bdd.BitFiddle._

  def foreachBdd(n  : Int)(f: Bdd => Unit): Unit = {
    require(n <= 5, s"foreachBdd cannot calculate numSamples for n=$n")
    foreachBdd(n, 1L << (1L << n))(f)
  }

  def foreachBdd(n  : Int, numSamples: Long)(f: Bdd => Unit): Unit = {
    if (n <= 5)
      (0L until math.min(numSamples, 1L << (1L << n))).foreach { k => f(genKthBdd(n, k)) }
    else
      (1L to numSamples).foreach { _ => f(genRandomBdd(n)) }
  }

  def foldBdds[A](n  : Int, init: A)(f: (A, Bdd) => A): A = {
    require(n <= 5, s"foldBdds cannot calculate numSamples for n=$n")
    foldBdds(n, 1L << (1L << n), init)(f)
  }

  def foldBdds[A](n  : Int, numSamples: Long, init: A)(f: (A, Bdd) => A): A = {
    if (n <= 5)
      (0L until math.min(numSamples, 1L << (1L << n))).foldLeft(init) {
        case (acc, k: Long) => f(acc, genKthBdd(n, k))
      }
    else
      (1L to numSamples).foldLeft(init) {
        case (acc, _) => f(acc, genRandomBdd(n))
      }
  }

  def genBddSizeHistogram(n  : Int): Map[Int, Long] = {
    require(n <= 5, s"foldBdds cannot calculate numSamples for n=$n")
    genBddSizeHistogram(n, numSamples = 1L << (1L << n))
  }

  def addMaps(m1  : Map[Int, Long], m2: Map[Int, Long]): Map[Int, Long] = {
    m1.foldLeft(m2) { case (acc, (k, v)) => acc + (k -> (acc.getOrElse(k, 0L) + v)) }
  }

  def genBddSizeHistogram(n  : Int, numSamples: Long): Map[Int, Long] = {
    // if n <=5, then we want to iterate through ALL the Bdds of n variables
    //   so we must not do this in multiple threads.  Doing, so would cause the number
    //   less than numSamples= 2^2^n to be passed to foldBdds, resulting in each
    //   iteration of foldBdds to calculate the same histogram.
    val numPartitions = if (n <= 5) 1 else 64
    (1 to numPartitions).par.map { x =>
      withNewBddHash {
        foldBdds(n, numSamples / numPartitions, Map[Int, Long]()) { (m  : Map[Int, Long], bdd: Bdd) => {
          val size = bdd.size()
          m + (size -> (1L + m.getOrElse(size, 0L)))
        }
        }
      }
    }.fold(Map[Int, Long]())(addMaps)
  }

  def genBddNormalizedSizeHistogram(n  : Int): Map[Int, Double] = {
    require(n <= 5, s"genBddNormalizedSizeHistogram cannot calculate numSamples for n=$n")
    val normalizer: Double = 1.0 / (1L << (1L << n))
    genBddSizeHistogram(n)
      .map { case (size: Int, population: Long) => (size -> population * normalizer) }
  }

  def genBddNormalizedSizeHistogram(n  : Int, numSamples: Long): Map[Int, Double] = {
    val normalizer: Double = 1.0 / numSamples
    genBddSizeHistogram(n, numSamples)
      .map { case (size: Int, population: Long) => (size -> population * normalizer) }
  }

  def gnuPlotBddSizeHistogram(r  : Range, numSamples: Int => Long): Unit = {
    import org.sameersingh.scalaplot.Implicits._
    import org.sameersingh.scalaplot._
    import org.sameersingh.scalaplot.jfreegraph.JFGraphPlotter

    import scala.concurrent.ExecutionContext

    implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
    val data = new XYData()
    withNewBddHash {
      r.foreach { n =>
        println(s"calculating histogram for n=$n numSamples=${numSamples(n)}")
        val xys = genBddNormalizedSizeHistogram(n, numSamples(n)).toList.sortBy {
          _._1
        }
        println(s"finished ============ n=$n =================")
        val xs = xys.map(_._1.toDouble)
        val ys = xys.map(_._2)
        data += new MemXYSeries(xs, ys, name = s"n=$n")
      }
    }

    val chart = new XYChart("BDD size distribution n Boolean variables",
                            data, x = Axis(log = true), y = Axis(label = "Normalized size"))
    chart.showLegend = true

    val plotter = new JFGraphPlotter(chart)
    plotter.gui()
  }


  def main(argv: Array[String]): Unit = {
    import bdd.GraphViz._

    genBddSizeHistogram(6, 1000).toList.sortBy(_._1)
    //gnuPlotBddSizeHistogram(18 to 5 by -1, i => 100000 / (i * i))
    // gnuPlotBddSizeHistogram(13 to 5 by -1, i => 1000)
    withNewBddHash{
      (1 to 3).foldLeft(List[Bdd]()){(<<< : List[Bdd],n:Int)=>
        val bdds = Bdd(n):: <<<
        val xorbdd = Xor(bdds)
        xorbdd.bddView(true,s"Xor(1..$n)")
        println(s"n=$n, xor size=${xorbdd.size}")
        bdds
      }
    }
  }
}
