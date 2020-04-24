package lbdd

import org.scalatest._


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
  }

}
