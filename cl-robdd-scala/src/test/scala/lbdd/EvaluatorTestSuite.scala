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
        b1.bddView(true, "b1")
        b2.bddView(true, "b2")
        println(l)
      }
      assert(Evaluator(b1, l) == Evaluator(b2, l))
    }
  }

  val all3Values: Set[Map[Int, Boolean]] =
    Set(
      Map(1 -> true, 2 -> true, 3 -> true),
      Map(1 -> true, 2 -> false, 3 -> true),
      Map(1 -> true, 2 -> true, 3 -> false),
      Map(1 -> true, 2 -> false, 3 -> false),
      Map(1 -> false, 2 -> true, 2 -> true),
      Map(1 -> false, 2 -> false, 2 -> true),
      Map(1 -> false, 2 -> true, 3 -> false),
      Map(1 -> false, 2 -> false, 3 -> false)
    )

  test("permutations") {
    for (n <- 1 to 7) {
      val perms = Evaluator.mapPermutations(n)
      assert(perms.size == Math.pow(2, n))
      assert(perms.forall(p => p.size == n))
    }
  }

  test("De Morgan's and Associativity") {
    val b1 = Not(And(1, Or(2, 3)))
    val b2 = And(Not(And(1, 2)), Not(And(1, 3)))

    val or1 = Or(Not(1), Not(2))
    val or2 = Or(Not(1), Not(3))
    val b3 = And(or1, or2)

    for (l <- Evaluator.mapPermutations(3)) {
      if (Evaluator(b2, l) != Evaluator(b3, l)) {
//        or1.bddView(true, "Or(Not(1), Not(2))")
//        or2.bddView(true, "Or(Not(1), Not(3))")
//        b3.bddView(true, "And(Or(Not(1), Not(2)), Or(Not(1), Not(3)))")
        b2.bddView(true, "And(Not(And(1, 2)), Not(And(1, 3)))")
      }
      assert(Evaluator(b1, l) == Evaluator(b2, l))
      assert(Evaluator(b2, l) == Evaluator(b3, l))
    }
  }

  test("not canonical") {
    for (l <- Evaluator.mapPermutations(3)) {
      val b1 = And(Not(1), Not(2), Not(3))
      val b2 = LBdd(2)

      assert(Evaluator(Or(b1, b2), l) ==
        Evaluator(Or(Or(AndNot(b1, b2), AndNot(b2, b1)), And(b1, b2)), l))
    }
  }

}
