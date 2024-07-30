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

import adjuvant.AdjFunSuite
import org.scalatest.funsuite.AnyFunSuite

class HistogramTestSuite extends AdjFunSuite {

  test("foreachBdd 1") {
    import Bdd._
    import Histogram._
    import adjuvant.Accumulators._
    withNewBddHash {
      (1 to 4).foreach { n =>
        assert(withCounter(count =>
                             foreachBdd(n) { _ => count() }) == (1L << (1L << n)))
      }
    }
  }
  test("foreachBdd 2") {
    import Bdd._
    import Histogram._
    import adjuvant.Accumulators._
    withNewBddHash {
      (1 to 4).foreach { n: Int =>
        val bdds: Set[Bdd] = withSetCollector(collect =>
                                                foreachBdd(n)(collect))
        assert(bdds.size == (1L << (1L << n)), s"n=$n expecting size=${1L << (1L << n)} got size=${bdds.size}")
      }
    }
  }
  test("genBddSizeHistogram") {
    import Histogram._
    val h1 = genBddSizeHistogram(1)
    assert(h1 == Map(1 -> 2,
                     3 -> 2))
    val h2 = genBddSizeHistogram(2)
    assert(h2 == Map(1 -> 2,
                     3 -> 4,
                     4 -> 8,
                     5 -> 2
                     ))
    assert(genBddNormalizedSizeHistogram(1) == Map(1 -> 0.5, 3 -> 0.5))

    val h3 = genBddSizeHistogram(3)
    assert(h3 == Map(1 -> 2,
                     3 -> 6,
                     4 -> 24,
                     5 -> 62,
                     6 -> 88,
                     7 -> 74))

    genBddSizeHistogram(6, 1000).toList.sortBy(_._1)
  }
}
