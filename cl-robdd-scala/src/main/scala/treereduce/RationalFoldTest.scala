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

package treereduce

object RationalFoldTest {
  // returns the average amount of time of evaluating block
  // after evaluating block repetitions number of times.
  def time[R](repetitions: Int, name: String, block: => R): Double = {
    val t0 = System.nanoTime()
    (1 to repetitions).foreach { _ => block }
    val t1 = System.nanoTime()
    val elapsed = {
      (t1 - t0) / (repetitions * 1.0e6)
    }
    println(s"$name: Elapsed time: $elapsed ms")
    elapsed
  }
  def rationalFoldTest():Unit = {
    val bounds = List(5, 10, 15, 20, 25, 30, 40, 45, 50, 55, 60, 70, 80, 90, 95
                      , 100, 125, 150, 175, 200, 300, 400, 500, 750
                      , 1000, 2000, 3000, 4000, 5000, 7500
                      , 10000
                      )
    rationalFoldTest(bounds=bounds,randomize=true)
    rationalFoldTest(bounds=bounds,randomize=false)
  }
  def rationalFoldTest(maxBound:Int):Unit = {
    require(maxBound >= 10)
    val bases = List(10, 12, 15, 20, 25, 30, 40, 45, 50, 55, 60, 70, 80, 90)
    val bounds = for {
      exp <- 0 to math.log10(maxBound).toInt
      base <- bases
      candidate = (base * math.pow(10.0, exp.toDouble)).toInt
      if candidate <= maxBound
    } yield candidate
    rationalFoldTest(bounds.toList,randomize=true)
    rationalFoldTest(bounds.toList,randomize=false)
  }
  def rationalFoldTest(bounds:List[Int],randomize:Boolean):Unit = {
    import spire.implicits._
    import spire.math._
    import treereduce.TreeParallelReduce._
    import treereduce.TreeReduce._
    import treereduce.TreeReducible._
    import gnuplot.GnuPlot.gnuPlot
    def rationalAdd(a: Rational, b: Rational): Rational = a + b

    val zero = Rational(0, 1)
    val rawDataForPlot = (for {r <- bounds.sorted.reverse.par // allow computation in parallel
                               piercedInterval = (if (randomize)
                                 scala.util.Random.shuffle( (( -r to -1) ++ (1 to r)).toList)
                               else
                                 (( -r to -1) ++ (1 to r)).toList)

                               t1 = time(3, s"$r  treeMapReduce", {
                                 val sum = piercedInterval.treeMapReduce(Rational(0, 1))(Rational(1, _), _ + _)
                                 assert(sum === zero)
                               })
                               t2 = time(3, s"$r  pairMapReduce", {
                                 val sum = pairMapReduce(piercedInterval)(Rational(0, 1), Rational(1, _), rationalAdd) //(indexedSeqPairable)
                                 assert(sum === zero)
                               })
                               t3 = time(3, s"$r           fold", {
                                 val sum = piercedInterval.map {
                                   Rational(1, _)
                                 }.foldLeft(Rational(0, 1))(_ + _)
                                 assert(sum === zero)
                               })
                               _ = println()
                               } yield (r.toDouble, t1, t2, t3)).toList.sortBy(_._1)

    val rs = rawDataForPlot.map(_._1)
    val t1s = rawDataForPlot.map(_._2)
    val t2s = rawDataForPlot.map(_._3)
    val t3s = rawDataForPlot.map(_._4)
    val dataForPlot = List(("tree-fold", rs, t1s)
                           , ("pair-wise-fold", rs, t2s)
                           , ("fold-left", rs, t3s))
    gnuPlot(dataForPlot)(
      title = "Fold Strategy Performance of Rational Addition" + (if (randomize) " (shuffled)" else " (sorted)"),
      comment = "Fold Strategy Performance of Rational Addition",
      xAxisLabel = "Number of terms (density)", xLog = true,
      yAxisLabel = "Time (ms)", yLog = true,
      grid = true,
      outputFileBaseName = "rational-addition" + (if (randomize) "-random" else ""))
  }

  def main(argv:Array[String]):Unit = {
    rationalFoldTest(5000)
  }
}