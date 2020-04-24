package lbdd

import org.scalatest._
import lbdd.GraphViz._


class EvaluatorTestSuite extends FunSuite {

  test("basic tests") {
    // Or truth table
    assert(Evaluator(Or(1, 2), Map(1 -> true, 2 -> true)))
    assert(Evaluator(Or(1, 2), Map(1 -> true, 2 -> false)))
    assert(Evaluator(Or(1, 2), Map(1 -> false, 2 -> true)))
    assert(! Evaluator(Or(1, 2), Map(1 -> false, 2 -> false)))

    // Commutation
    assert(Evaluator(Or(2, 1), Map(1 -> true, 2 -> true)))
    assert(Evaluator(Or(2, 1), Map(1 -> true, 2 -> false)))
    assert(Evaluator(Or(2, 1), Map(1 -> false, 2 -> true)))
    assert(! Evaluator(Or(2, 1), Map(1 -> false, 2 -> false)))

    // And truth table
    assert(Evaluator(And(1, 2), Map(1 -> true, 2 -> true)))
    assert(! Evaluator(And(1, 2), Map(1 -> true, 2 -> false)))
    assert(! Evaluator(And(1, 2), Map(1 -> false, 2 -> true)))
    assert(! Evaluator(And(1, 2), Map(1 -> false, 2 -> false)))

    // Commutation
    assert(Evaluator(And(2, 1), Map(1 -> true, 2 -> true)))
    assert(! Evaluator(And(2, 1), Map(1 -> true, 2 -> false)))
    assert(! Evaluator(And(2, 1), Map(1 -> false, 2 -> true)))
    assert(! Evaluator(And(2, 1), Map(1 -> false, 2 -> false)))

    // Not truth table
    assert(! Evaluator(Not(1), Map(1 -> true)))
    assert(Evaluator(Not(1), Map(1 -> false)))
  }


  val allValues: Set[Map[Int, Boolean]] =
    Set(
      Map(1 -> true, 2 -> true),
      Map(1 -> true, 2 -> false),
      Map(1 -> false, 2 -> true),
      Map(1 -> false, 2 -> false)
    )

  test("De Morgan's") {
    val b1 = Not(Or(1, 2))
    val b2 = And(Not(1), Not(2))

    for (l <- allValues) {
      if (Evaluator(b1, l) != Evaluator(b2, l)) {
        b1.bddView(true, "Not(Or(1, 2))")
        b2.bddView(true, "And(Not(1), Not(2))")
      }
      assert(Evaluator(b1, l) == Evaluator(b2, l))
    }
  }

}
