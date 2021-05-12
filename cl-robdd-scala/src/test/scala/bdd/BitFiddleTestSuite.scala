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

import Bdd._
import BitFiddle._
import org.scalatest.funsuite.AnyFunSuite

class BitFiddleTestSuite extends AnyFunSuite {

  test("genSample"){
    for{
      n <- 1 to 5
      m <- 1 to n
      sample <- Some(genSample(n,m))
      _ = assert(sample.size == m)
    } ()
  }
  test("genDnfFromBitMask"){
    for{
      n <- 1 to 5
      m <- 1 to n
      minterm <- genDnfFromBitMask(n,m, 5000, _=>true).iterator
    } assert(minterm.size == m, s"n=$n minterm=$minterm expecting positive size=$m")
    for{
      n <- 1 to 5
      m <- 1 to n
      minterm <- genDnfFromBitMask(n,m, 5000, _=>true).iterator
    } assert(!minterm.contains(0), s"n=$n minterm=$minterm should never contain 0 size=$m")
    for{
      n <- 1 to 5
      m <- 1 to n
      minterm <- genDnfFromBitMask(n,m, 5000, _=>true).iterator
      bit <- minterm
    } assert(! minterm.contains(-bit), s"n=$n minterm=$minterm should never simultaneously contain $bit and -$bit")

    for{
      n <- 12 to 14
      m <- 3 to 5
      lim <- 12 to 13
      terms = genDnfFromBitMask(n,m, lim, _=>true)
    } assert(terms.iterator.size == lim, s"n=$n m=$m, lim=$lim, size=${terms.iterator.size}")
  }
  test("extractBits") {

    assert(minTerm(3, 0) == List(-3, -2, -1))
    assert(minTerm(3, 1) == List(-3, -2, 1))
    assert(minTerm(3, 2) == List(-3, 2, -1))
    assert(minTerm(3, 3) == List(-3, 2, 1))
    assert(minTerm(3, 7) == List(3, 2, 1))

    (2 to 12).foreach { j =>
      (0 to 1 << j - 1).foreach{ i =>
        val mt = minTerm(j, i)
        assert(mt.length == j)
        assert(mt.toSet.size == j)
        assert((1 to j).forall{d => mt.contains(d) || mt.contains(-d)})
        assert(! mt.contains(j+1), s"mt=$mt contains j+1=${j+1} when i=$i")
        assert(! mt.contains(0))
        val posBits = mt.filter(_ > 0)
        val m = posBits.foldLeft(0) { (acc: Int, k: Int) =>
          acc + (if (k > 0) math.pow(2, k - 1).toInt else 0)
        }
        assert(m == i, s"failed for m=$m i=$i posBits=$posBits")
      }
    }
  }
  test("genKthBdd") {

    withNewBddHash {
      assert(genKthBdd(5, 0) == BddFalse)
      // 1 in binary is 00..00001 (32 bits) so genKthBdd which has 1 only in the least-significant position, thus the 1st minterm 00000
      assert(genKthBdd(5, 1) == And(Not(1), Not(2), Not(3), Not(4), Not(5)))
      assert(genKthBdd(5, 1) == And(-1, -2, -3, -4, -5))
      assert(genKthBdd(5, 1) == And(-5, -4, -3, -2, -1))
      assert(genKthBdd(5, 1) == Not(Or(1, 2, 3, 4, 5)))

      assert(genKthBdd(1,0) == BddFalse)
      assert(genKthBdd(1,1) == Bdd(-1))
      assert(genKthBdd(1,2) == Bdd(1))
      assert(genKthBdd(1,3) == BddTrue)

      assert(genKthBdd(1, 0) == BddFalse)
      assert(genKthBdd(1, 1) == Not(1))
      assert(genKthBdd(1, 2) == Bdd(1))
      assert(genKthBdd(1, 3) == BddTrue)

      assert(genKthBdd(2, 0xf) == BddTrue)
      assert(genKthBdd(3, 0xff) == BddTrue)
      assert(genKthBdd(4, 0xffff) == BddTrue)

    }
  }
  test("gen truth table range") {
    import Bdd._
    import adjuvant.Accumulators.withCounter
    withNewBddHash {
      (1 to 4).foreach { n =>
        assert(1 ==
                 withCounter(countF => {
                   assert(1 == withCounter(countT => {
                     (0L until 1L << (1L << n)).foreach { k =>
                       val bdd = genKthBdd(n, k)
                       if (bdd == BddTrue)
                         countT()
                       else if (bdd == BddFalse)
                         countF()
                     }
                   }), s"countT failed for n=$n")
                 }), s"countF failed for n=$n")
      }
    }
  }
}