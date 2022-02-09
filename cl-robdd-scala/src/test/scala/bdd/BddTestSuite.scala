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

import bdd.Bdd._
import org.scalatest.funsuite.AnyFunSuite

class BddTestSuite extends AnyFunSuite {

  test("printing") {
    withNewBddHash {
      BddTrue.toString
      BddFalse.toString
      val bdd1 = Bdd(3, BddTrue, BddFalse)
      val bdd2 = Bdd(2, BddFalse, BddTrue)
      val bdd3 = Bdd(1, bdd1, bdd2)
      bdd1.toString
      bdd2.toString
      bdd3.toString
    }
  }
  test("eq-ness") {
    withNewBddHash {
      val bdd1 = Bdd(3, BddTrue, BddFalse)
      val bdd2 = Bdd(2, BddFalse, BddTrue)
      val bdd3 = Bdd(1, bdd1, bdd2)

      Or(1, 2).toString
      And(1, 2).toString
      And(Or(1, Not(2)), Xor(3, AndNot(2, 3))).toString

      {
        val x = Not(And(bdd3, bdd2))
        val y = Or(Not(bdd3), Not(bdd2))
        x.toString
        y.toString
        assert(x eq y)
      }
    }
  }

  def genSamples(): Set[Bdd] = {
    val bdd1 = Bdd(3, BddTrue, BddFalse)
    val bdd2 = Bdd(2, BddFalse, BddTrue)
    val bdd3 = Bdd(1, bdd1, bdd2)
    val samples1 = Set[Bdd](BddTrue, BddFalse, bdd1, bdd2, bdd3)
    val samples2 = samples1 ++ samples1.map(b => Not(b))
    val samples = samples2 ++ samples2.flatMap(b1 => {
      samples2.flatMap(b2 => Set(And(b1, b2), Or(b2, b2), Xor(b1, b2), Xnor(b1, b2), AndNot(b1, b2)))
    })
    samples
  }

  test("size") {
    withNewBddHash {
      assert(1 == BddTrue.size())
      assert(1 == BddFalse.size())
      assert(3 == Bdd(1).size())
      assert(3 == Bdd(-1).size())

      val samples = genSamples()
      val bdd1 = Bdd(3, BddTrue, BddFalse)
      val bdd2 = Bdd(2, BddFalse, BddTrue)
      val bdd3 = Bdd(1, bdd1, bdd2)
      assert(BddFalse.size() == 1)
      assert(BddTrue.size() == 1)
      assert(bdd1.size() == 3)
      assert(bdd2.size() == 3)
      //bdd3.bddView(true,"Testing")
      assert(bdd3.size() == 5)
      for (bdd <- samples) {
        bdd match {
          case _: BddTerm => assert(bdd.size() == 1)
          case b: BddNode => assert(bdd.size() <= 1 + b.positive.size() + b.negative.size()) // maybe less because some nodes or terms might be shared
        }
      }
    }
  }

  test("subset superset") {
    withNewBddHash {
      val samples = genSamples()
      for {b1 <- samples} {
        for {b2 <- samples} {
          assert(And(b1, b2).subsetp(b1))
          assert(b1.supersetp(And(b1, b2)))
          assert(Or(b1, b2).supersetp(b1))
          assert(b1.subsetp(Or(b1, b2)))
        }
      }
    }
  }
  test("subset superset trivial") {
    assert(BddTrue.supersetp(BddFalse))
    assert(BddFalse.subsetp(BddTrue))
    assert(BddFalse.subsetp(BddTrue))
    assert(BddTrue.supersetp(BddFalse))
    assert(BddFalse.subsetp(BddFalse))
    assert(BddTrue.supersetp(BddTrue))
  }

  test("equivalentBySubset") {
    withNewBddHash {
      val samples = genSamples()
      for (bdd1 <- samples) {
        for (bdd2 <- samples) {
          if (bdd1 eq bdd2)
            assert(bdd1.equivalentBySubset(bdd2))
          else
            assert(!bdd1.equivalentBySubset(bdd2))
        }
      }
    }
  }

  test("printer") {
    withNewBddHash {
      Or(Bdd(1), Bdd(2))(Assignment(Set(1))).toString
      Or(Bdd(1), Bdd(2))(Assignment(Set(2))).toString
      Or(Bdd(1), Bdd(2))(Assignment(Set(1, 2))).toString
      Or(Bdd(1), Bdd(2))(Assignment(Set[Int]())).toString

      And(Bdd(1), Bdd(2))(Assignment(Set(1))).toString
      And(Bdd(1), Bdd(2))(Assignment(Set(2))).toString
      And(Bdd(1), Bdd(2))(Assignment(Set(1, 2))).toString
      And(Bdd(1), Bdd(2))(Assignment(Set[Int]())).toString

      val bdd = Or(Bdd(1), Not(Bdd(2)))
      Xnor(bdd, bdd).toString
    }
  }
}


