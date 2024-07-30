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
import bdd.Bdd._
import org.scalatest.funsuite.AnyFunSuite

class BinaryOperationTestSuite  extends AdjFunSuite {

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

  test("identities") {
    withNewBddHash {
      assert(Or(BddTrue, BddFalse) equals BddTrue)
      assert(Or(BddTrue, BddFalse) eq BddTrue)
      assert(Or(BddFalse, BddFalse) eq BddFalse)
      assert(Or(BddFalse, BddTrue) eq BddTrue)
      assert(Or(BddTrue, BddTrue) eq BddTrue)

      assert(Xor(BddTrue, BddFalse) eq BddTrue)
      assert(Xor(BddFalse, BddFalse) eq BddFalse)
      assert(Xor(BddFalse, BddTrue) eq BddTrue)
      assert(Xor(BddTrue, BddTrue) eq BddFalse)

      assert(Xnor(BddTrue, BddFalse) eq BddFalse)
      assert(Xnor(BddFalse, BddFalse) eq BddTrue)
      assert(Xnor(BddFalse, BddTrue) eq BddFalse)
      assert(Xnor(BddTrue, BddTrue) eq BddTrue)

      assert(And(BddTrue, BddFalse) eq BddFalse)
      assert(And(BddFalse, BddFalse) eq BddFalse)
      assert(And(BddFalse, BddTrue) eq BddFalse)
      assert(And(BddTrue, BddTrue) eq BddTrue)

      assert(AndNot(BddTrue, BddFalse) eq BddTrue)
      assert(AndNot(BddFalse, BddFalse) eq BddFalse)
      assert(AndNot(BddFalse, BddTrue) eq BddFalse)
      assert(AndNot(BddTrue, BddTrue) eq BddFalse)

      assert(Not(BddTrue) eq BddFalse)
      assert(Not(BddFalse) eq BddTrue)
    }
  }
  test("idempotence") {
    withNewBddHash {
      val samples = genSamples()
      for (bdd <- samples) {
        assert(And(bdd, bdd) eq bdd)
        assert(Or(bdd, bdd) eq bdd)
        assert(Xor(bdd, bdd) eq BddFalse)
        assert(AndNot(bdd, bdd) eq BddFalse)

        assert(And(bdd, BddTrue) eq bdd)
        assert(Or(bdd, BddTrue) eq BddTrue)
        assert(Xor(bdd, BddTrue) eq Not(bdd))
        assert(AndNot(bdd, BddTrue) eq BddFalse)
        assert(AndNot(BddTrue, bdd) eq Not(bdd))

        assert(And(bdd, BddFalse) eq BddFalse)
        assert(Or(bdd, BddFalse) eq bdd)
        assert(Xor(bdd, BddFalse) eq bdd)
        assert(AndNot(bdd, BddFalse) eq bdd)
        assert(AndNot(BddFalse, bdd) eq BddFalse)
      }
    }
  }
  test("xor") {
    withNewBddHash {
      val samples = genSamples()
      for (b1 <- samples) {
        for (b2 <- samples) {
          assert(Xor(b1, b2) eq AndNot(Or(b1, b2), And(b1, b2)))
          assert(Xor(b1, b2) eq Or(AndNot(b1, b2), AndNot(b2, b1)))
        }
      }
    }
  }
  test("xnor") {
    withNewBddHash {
      val samples = genSamples()
      for (b1 <- samples) {
        for (b2 <- samples) {
          assert(Xor(b1, b2) eq Not(Xnor(b1, b2)))
        }
      }
    }
  }
  test("and-not") {
    withNewBddHash {
      val samples = genSamples()
      for (b1 <- samples) {
        for (b2 <- samples) {
          assert(Or(Or(AndNot(b1, b2), AndNot(b2, b1)), And(b1, b2)) eq Or(b1, b2))
        }
      }
      val reference = AndNot(1, 2, 3, 4, 5, 6, 7, 8)
      assert(reference == And(1, -2, -3, -4, -5, -6, -7, -8))
      assert(reference == And(1, Not(Or(2, 3, 4, 5, 6, 7, 8))))
      assert(reference == And(1, And(-2, -3, -4, -5, -6, -7, -8)))
    }
  }

  test("varargs") {
    withNewBddHash {
      for {v1 <- 1 to 4
           _ = assert(And(v1) == Bdd(v1))
           _ = assert(Or(v1) == Bdd(v1))
           v2 <- -2 to 4
           if v2 != 0
           v3 <- 1 to 4
           if v3 != 0
           } {
        assert(And(v1, v2, v3) eq And(v1, And(v2, v3)))
        assert(Or(v1, v2, v3) eq Or(v1, Or(v2, v3)))
        assert(Xor(v1, v2, v3) eq Xor(v1, Xor(v2, v3)))
      }
    }
  }
  test("commutative") {
    withNewBddHash {
      val samples = genSamples()
      for (b1 <- samples) {
        for (b2 <- samples) {
          assert(Or(b1, b2) eq Or(b2, b1))
          assert(And(b1, b2) eq And(b2, b1))
          assert(Xor(b1, b2) eq Xor(b2, b1))
        }
      }
    }
  }
  test("associative") {
    withNewBddHash {
      val samples = genSamples()
      for (b1 <- samples) {
        for (b2 <- samples) {
          for (b3 <- samples) {
            assert(Or(Or(b1, b2), b3) eq Or(b1, Or(b2, b3)))
            assert(And(And(b1, b2), b3) eq And(b1, And(b2, b3)))
            assert(Xor(Xor(b1, b2), b3) eq Xor(b1, Xor(b2, b3)))
          }
        }
      }
    }
  }
  test("de morgan") {
    withNewBddHash {
      val samples = genSamples()
      for (b1 <- samples) {
        for (b2 <- samples) {
          assert(Not(Or(b1, b2)) eq And(Not(b1), Not(b2)))
          assert(Not(And(b1, b2)) eq Or(Not(b1), Not(b2)))
        }
      }
    }
  }

  test("inversion") {
    withNewBddHash {
      val samples = genSamples()
      for (b1 <- samples) {
        assert(Not(Not(b1)) eq b1)
      }
    }
  }
  test("varargs bdd") {
    withNewBddHash {
      assert(AndNot(1, 2) == And(1, Not(2)))
      assert(AndNot(1, 2, 3) == And(1, Not(2), Not(3)))
      assert(AndNot(1, 2, 3) == And(1, Not(Or(2, 3))))
      assert(AndNot(1, 2, 3, 4) == And(1, Not(Or(2, 3, 4))))
    }
  }

}
