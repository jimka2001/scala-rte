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


package treereduce

import org.scalatest.funsuite.AnyFunSuite


class TreeParallelReduceSuite extends AnyFunSuite {

  test("parallel") {
    import treereduce.TreeParallelReduce._
    import scala.collection.parallel.CollectionConverters._

    def id[A](a: A): A = a

    def addInt(a: Int, b: Int): Int = a + b

    var data = (1 to 100).toList
    assert(5050 == pairMapReduce(data)(init = 0, id, addInt))
    assert(5050 == pairMapReduce(data)(init = 0, id, addInt))
    assert(5050 == pairMapReduce(data.par)(init = 0, id, addInt))
    assert(5050 == pairMapReduce(data.toArray)(init = 0, id, addInt))
    assert(5050 == pairMapReduce(data.toArray.par)(init = 0, id, addInt))
    //assert(5050 == pairMapReduce(1 to 100)(init = 0,id, addInt))
    assert(5050 == pairMapReduce((1 to 100).toIterator)(init = 0, id, addInt))
    assert(5050 == pairMapReduce(data)(init = 0, id, addInt))
  }
  test("tree map tree reduce non-commutative") {
    import treereduce.TreeParallelReduce._
    def id(i: (Int, Int)): (Int, Int) = i

    def mergeIntervals(interval1: (Int, Int), interval2: (Int, Int)): (Int, Int) = {
      // merge intervals is a non-commutative function. we thus test whether
      //   treeMapReduce respects the order of the arguments in the same way
      //   that foldLeft does.
      val (a_lower, a_upper) = interval1
      val (b_lower, b_upper) = interval2
      assert(a_upper == b_lower, s"adjacent intervals not compatible: $interval1 $interval2")
      assert(a_upper >= a_lower)
      assert(b_upper >= b_lower)
      (a_lower, b_upper)
    }

    for {
      upper <- (1 to 1000)
      intervals = ((0 to upper).map(i => (i, i + 1))).toList
    } {
      assert(intervals.foldLeft((0, 0))(mergeIntervals) == pairMapReduce(intervals)(init = (0, 0), id, mergeIntervals))
    }
  }
  test("rational numbers") {
    import spire.implicits._
    import spire.math._
    import treereduce.TreeParallelReduce._
    def time[R](name: String, block: => R): R = {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()
      println(s"$name: Elapsed time: ${(t1 - t0) / 1.0e6} ms")
      result
    }

    def rationalAdd(a: Rational, b: Rational): Rational = a + b

    val zero = Rational(0, 1)
    for {r <- List(10, 20, 40, 75, 100, 200, 400, 750, 1000, 2000, 4000, 7500)} {
      val piercedInterval = (-r to -1) ++ (1 to r)
      time(s"$r  pairMapReduce", {
        import scala.collection.immutable.IndexedSeq
        val sum = pairMapReduce(piercedInterval)(Rational(0, 1), Rational(1, _), rationalAdd)//(indexedSeqPairable)
        assert(sum === zero)
      })
      time(s"$r           fold", {
        val sum = piercedInterval.map {
          Rational(1, _)
        }.foldLeft(Rational(0, 1))(_ + _)
        assert(sum === zero)
      })
      println()
    }
  }
}