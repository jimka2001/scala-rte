// Copyright (c) 2020 EPITA Research and Development Laboratory
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

package lbdd

import lbdd.LBdd._
import org.scalatest._
import lbdd.GraphViz._


class BinaryOperationsTestSuite extends FunSuite {

  def genSamples(): Set[LBdd] = {
    val b1 = LBdd(3, LBddTrue, LBddFalse)
    val b2 = LBdd(2, LBddFalse, LBddTrue)
    val b3 = LBdd(1, b1, b2)
    val samples1 = Set[LBdd](LBddTrue, LBddFalse, b1, b2, b3)
    val samples2 = samples1 ++ samples1.map(b => Not(b))
    val samples = samples2 ++ samples2.flatMap( b1 => {
      samples2.flatMap( b2 => Set(And(b1, b2), Or(b2, b2)))
    })
    samples
  }

  test("identities") {
    // Not identities
    assert(Not(LBddTrue) eq LBddFalse)
    assert(Not(LBddFalse) eq LBddTrue)

    // Or identities
    assert(Or(LBddTrue, LBddFalse) eq LBddTrue)
    assert(Or(LBddFalse, LBddTrue) eq LBddTrue)
    assert(Or(LBddFalse, LBddFalse) eq LBddFalse)
    assert(Or(LBddTrue, LBddTrue) eq LBddTrue)

    // And identities
    assert(And(LBddTrue, LBddTrue) eq LBddTrue)
    assert(And(LBddTrue, LBddFalse) eq LBddFalse)
    assert(And(LBddFalse, LBddFalse) eq LBddFalse)
    assert(And(LBddFalse, LBddTrue) eq LBddFalse)

    // AndNot identities
    assert(AndNot(LBddTrue, LBddFalse) eq LBddTrue)
    assert(AndNot(LBddTrue, LBddTrue) eq LBddFalse)
    assert(AndNot(LBddFalse, LBddTrue) eq LBddFalse)
    assert(AndNot(LBddFalse, LBddFalse) eq LBddFalse)
  }

  test("Xor and Xnor identities") {
    // Xor identities
    assert(Xor(LBddTrue, LBddFalse) eq LBddTrue)
    assert(Xor(LBddTrue, LBddTrue) eq LBddFalse)
    assert(Xor(LBddFalse, LBddFalse) eq LBddFalse)
    assert(Xor(LBddFalse, LBddTrue) eq LBddTrue)

    // Xnor identities
    assert(Xnor(LBddTrue, LBddFalse) eq LBddFalse)
    assert(Xnor(LBddTrue, LBddTrue) eq LBddTrue)
    assert(Xnor(LBddFalse, LBddTrue) eq LBddFalse)
    assert(Xnor(LBddFalse, LBddFalse) eq LBddTrue)
  }

  test("Xor samples") {
    val samples = genSamples()
    for (b1 <- samples) {
      for (b2 <- samples) {
        val b = Xor(b1, b2)
        val t1 = Or(And(b1, Not(b2)), And(Not(b1), b2))
        val t2 = And(Or(b1, b2), Or(Not(b1), Not(b2)))
        val t3 = And(Or(b1, b2), Not(And(b1, b2)))
        val t = Seq(t1, t2, t3)

        assert(t.forall(Evaluator.truthEval(b, _, 3)))

        assert(Evaluator.truthEval(b, Xor(Not(b1), b2), 3))
        assert(Evaluator.truthEval(b, Xor(b1, Not(b2)), 3))
        assert(Evaluator.truthEval(b, Xor(Not(b1), Not(b2)), 3))
        assert(Evaluator.truthEval(Xor(b, b2), b1, 3))
      }
    }
  }

  test("Xnor samples") {
    val samples = genSamples()
    for (b1 <- samples) {
      for (b2 <- samples) {
        val b = Xnor(b1, b2)
        val t1 = And(Or(b1, Not(b2)), Or(Not(b1), b2))
        val t2 = Or(And(b1, b2), And(Not(b1), Not(b2)))
        val t3 = Or(And(b1, b2), Not(Or(b1, b2)))
        val t4 = Xnor(b2, b1)
        val t = Seq(t1, t2, t3, t4)

        assert(t.forall(Evaluator.truthEval(b, _, 3)))
      }
    }
  }

  test("idempotence") {
    val samples = genSamples()
    for (b <- samples) {
      assert(And(b, b) eq b)
      assert(Or(b, b) eq b)
      assert(AndNot(b, b) eq LBddFalse)

      assert(And(b, LBddTrue) eq b)
      assert(Or(b, LBddTrue) eq LBddTrue)
      assert(AndNot(b, LBddTrue) eq LBddFalse)
      assert(AndNot(LBddTrue, b).toString == Not(b).toString)

      assert(And(b, LBddFalse) eq LBddFalse)
      assert(Or(b, LBddFalse) eq b)
      assert(AndNot(b, LBddFalse) eq b)
      assert(AndNot(LBddFalse, b) eq LBddFalse)
    }
  }

  test("AndNot") {
    val samples = genSamples()
    for (b1 <- samples) {
      for (b2 <- samples) {
        assert(Evaluator.truthEval(Or(Or(AndNot(b1, b2), AndNot(b2, b1)), And(b1, b2)), Or(b1, b2), 3))
      }
    }
  }

  test("varargs") {
    for { v1 <- 1 to 4
          _ = assert(And(v1).toString == LBdd(v1).toString)
          _ = assert(Or(v1).toString == LBdd(v1).toString)
          v2 <- -2 to 4
          if v2 != 0
          v3 <- 1 to 4
          if v3 != 0
        } {
      assert(Evaluator.truthEval(And(v1, v2, v3), And(v1, And(v2, v3)), 4))
      assert(Evaluator.truthEval(Or(v1, v2, v3), Or(v1, Or(v2, v3)), 4))
    }
  }

  test("commutative") {
    val samples = genSamples()
    for (b1 <- samples) {
      for (b2 <- samples) {
        assert(Or(b1, b2).toString == Or(b2, b1).toString)
        assert(And(b1, b2).toString == And(b2, b1).toString)
        assert(Xnor(b1, b2).toString == Xnor(b2, b1).toString)
      }
    }
  }

  test("associative") {
    val samples = genSamples()
    for (b1 <- samples) {
      for (b2 <- samples) {
        for (b3 <- samples) {
          assert(Evaluator.truthEval(Or(Or(b1, b2), b3), Or(b1, Or(b2, b3)), 3))
          assert(Evaluator.truthEval(And(And(b1, b2), b3), And(b1, And(b2, b3)), 3))
          assert(Evaluator.truthEval(Xnor(Xnor(b1, b2), b3), Xnor(b1, And(b2, b3)), 3))
        }
      }
    }
  }

}
