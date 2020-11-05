package lbdd

import lbdd.GraphViz._


object Evaluator {

  def apply(b: LBdd, assignment: Map[Int, Boolean]): Boolean = {
    b match {
      case LBddFalse => false
      case LBddTrue => true
      case node: LBddNode =>
        if (node.negation) apply(node, assignment)
        else apply(node, assignment)
    }
  }

  def apply(node: LBddNode, assignment: Map[Int, Boolean]): Boolean = {
    val value = assignment(node.label)
    if (value)
      if (node.negation)  ! apply(node.positive, assignment) & ! apply(node.middle, assignment)
      else apply(node.positive, assignment) || apply(node.middle, assignment)
    else
      if (node.negation) ! apply(node.negative, assignment) & ! apply(node.middle, assignment)
      else apply(node.negative, assignment) || apply(node.middle, assignment)
  }

  def apply(node: lazyNode, assignment: Map[Int, Boolean]): Boolean = {
    if (node.isEmpty) false
    else apply(node.get(), assignment)
  }

  def permutations(n : Int): List[List[Boolean]] = {
    val input = List(true, false)
    n match {
      case 1 => for (el <- input) yield List(el)
      case _ => for (el <- input; perm <- permutations(n - 1)) yield el :: perm
    }
  }

  def mapPermutations(n: Int): Set[Map[Int, Boolean]] = {
    permutations(n).map(l => (l.indices.map(_ + 1) zip l).toMap).toSet
  }

  def truthEval(b1: LBdd, b2: LBdd, n: Int): Boolean = {
    for (l <- mapPermutations(n)) {
      if (apply(b1, l) != apply(b2, l)) false
    }
    true
  }


  def truthTable(b: LBdd, n: Int): Unit = {
    for (i <- 1 to n) print("  " + i + "   | ")
    println(" Bdd ")
    for (l <- mapPermutations(n)) {
      for ((k, v) <- l) {
        if (v) print(v + "  | ")
        else print(v + " | ")
      }
      println(apply(b, l))
    }
    println()
  }
}


object main {
  def main(args: Array[String]): Unit = {
    //println(Evaluator(LBdd(1), Map(1 -> false)))

    val b1 = And(Not(1), Not(2), Not(3))
    val b2 = LBdd(2)

    b1.bddView(true, "And(Not(1), Not(2), Not(3))")
    b2.bddView(true, "x_2")
    Or(b1, b2).bddView(true, "Or(b1, b2)")
    Or(Or(AndNot(b1, b2), AndNot(b2, b1)), And(b1, b2)).bddView(true, "Or(Or(AndNot(b1, b2), AndNot(b2, b1)), And(b1, b2))")

    Evaluator.truthTable(Or(b1, b2), 3)
    Evaluator.truthTable(Or(Or(AndNot(b1, b2), AndNot(b2, b1)), And(b1, b2)), 3)


//    val b2 = And(2, Not(3))
//    val b1 = LBdd(3)
//    val b3 = And(Not(2), Not(3))
//
//    b1.bddView(true, "b1")
//    b2.bddView(true, "b2")
//    b3.bddView(true, "b3")
//
//    Or(Or(b1, b2), b3).bddView(true, "Or(Or(b1, b2), b3)")
//    Or(b1, Or(b2, b3)).bddView(true, "Or(b1, Or(b2, b3))")
  }
}
