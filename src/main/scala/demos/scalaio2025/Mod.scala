package demos.scalaio2025

import adjuvant.Adjuvant.randCase

abstract class Expr

case class Plus(a: Expr, b: Expr) extends Expr

case class Times(a: Expr, b: Expr) extends Expr

case class Const(n: Int) extends Expr


object Expr {
  val random = new scala.util.Random

  def eval(e: Expr, p: Int): Int = {
    e match {
      case Plus(a, b) => (eval(a, p) + eval(b, p)) % p
      case Times(a, b) => (eval(a, p) * eval(b, p)) % p
      case Const(n) => n % p
    }
  }

  def naiveRand(depth: Int, p: Int): Expr = {
    if (depth == 0)
      Const(random.nextInt(p))
    else
      randCase(() => Const(random.nextInt(p)),
               Seq((0.333, () => Plus(naiveRand(depth - 1, p), naiveRand(depth - 1, p))),
                   (0.333, () => Times(naiveRand(depth - 1, p), naiveRand(depth - 1, p)))))
  }

  def balancedRand(depth: Int, p: Int): Expr = {
    val nodeCount = random.between(1 << depth, 1 << (depth + 1))
    val population = scala.util.Random.shuffle((0 until nodeCount).to(List))

    abstract class Node
    case class InternalNode(n: Int, left: Node, right: Node) extends Node
    case class LeafNode() extends Node

    def insert(node: Node, a: Int): Node = {
      node match {
        case InternalNode(n, left, right) if a < n => InternalNode(n, insert(left, a), right)
        case InternalNode(n, left, right) => InternalNode(n, left, insert(right, a))
        case LeafNode() => InternalNode(a, LeafNode(), LeafNode())
      }
    }

    def nodeToExpr(node: Node): Expr = {
      node match {
        case InternalNode(_, left, right) =>
          randCase(() => Plus(nodeToExpr(left), nodeToExpr(right)),
                   Seq((0.5, () => Times(nodeToExpr(left), nodeToExpr(right)))))
        case LeafNode() => Const(random.nextInt(p))
      }
    }

    val default: Node = LeafNode()
    nodeToExpr(population.foldLeft(default)(insert))
  }

}

object ModDemo {

  def allProducts(p:Int):Seq[(Int,Double)] = {
    val size = (p * p).toDouble

    (for {a <- 0 to p - 1
          b <- 0 to p - 1} yield (a, b))
      .foldLeft(Map[Int, Int]()) {
        case (acc, (a, b)) =>
          val prod = a * b % p
          acc + (prod -> (1 + acc.getOrElse(prod, 0)))
      }
      .to(List)
      .map { case (prod, count) => (prod, 100 * count / size) }
      .sortBy(_._2)
      .reverse
  }

  // print a histogram of percentages of all the values
  // of a*b mod p
  def main(argv: Array[String]): Unit = {
    val p = 2

    println(allProducts(p))
  }
}


object TestExpr {

  import Expr.{eval, balancedRand, naiveRand}
  import adjuvant.Adjuvant.returnPercentages

  def main(argv: Array[String]): Unit = {
    val depth = 6
    val p = 5
    val m = 100000
    println("Balanced", returnPercentages(m, () => eval(balancedRand(depth, p), p)))
    println("Naive   ", returnPercentages(m, () => eval(naiveRand(depth, p), p)))
    println("All     ", ModDemo.allProducts(p))
  }
}

