package demos.scalaio2025

import adjuvant.Adjuvant.weightedCase

abstract class Expr

case class Plus(a: Expr, b: Expr) extends Expr

case class Times(a: Expr, b: Expr) extends Expr

case class Const(n: Int) extends Expr


object Expr {
  val random = new scala.util.Random

  // evaluate an Expr tree of + and * mod p
  def eval(e: Expr, p: Int): Int = {
    e match {
      case Plus(a, b) => (eval(a, p) + eval(b, p)) % p
      case Times(a, b) => (eval(a, p) * eval(b, p)) % p
      case Const(n) => n % p
    }
  }

  // generate a randomly selected Expr tre.
  // a tree of depth-d-or-less is either a leaf
  // or a node whose two children are each
  // trees of depth-(d-1)-or-less
  def naiveRandByDepth(depth: Int, p: Int): Expr = {
    if (depth == 0)
      Const(random.nextInt(p))
    else
      weightedCase(
               (33, () => Plus(naiveRandByDepth(depth - 1, p), naiveRandByDepth(depth - 1, p))),
               (33, () => Times(naiveRandByDepth(depth - 1, p), naiveRandByDepth(depth - 1, p))),
               (34, () => Const(random.nextInt(p))))
  }

  def naiveRandBySize(leaves: Int, p: Int): Expr = {
        assert(leaves > 0)
    if (leaves == 1)
      Const(random.nextInt(p))
    else {
      val leftSize = random.between(1, leaves)
      val left = naiveRandBySize(leftSize, p)
      val right = naiveRandBySize(leaves - leftSize, p)
      // println(s"$leaves = $leftSize + ${leaves - leftSize}")
      if (random.nextInt(2) == 0)
        Plus(left,right)
      else
        Times(left,right)
    }
  }

  // generate a randomly selected Expr tree of depth approximately d.
  // this is done by generating a somewhat-balanced tree, by shuffling
  // the list 0, 1, 2, ... 2^(depth+1)-1,
  // then building a binary search tree by inserting this list iterative
  // in shuffled order into a tree starting with the empty tree,
  // finally building a Expr tree the same share as the binary search tree.
  def balancedRandByDepth(depth: Int, p: Int): Expr = {
    val nodeCount = random.between(1 << depth, 1 << (depth + 1))
    balancedRandBySize(nodeCount, p)
  }

  def balancedRandBySize(nodeCount:Int, p:Int):Expr = {
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
          weightedCase[Expr](// () => Times(nodeToExpr(left), nodeToExpr(right)),
                   (50, () => Plus(nodeToExpr(left), nodeToExpr(right))),
                   (50, () => Times(nodeToExpr(left), nodeToExpr(right))))
        case LeafNode() => Const(random.nextInt(p))
      }
    }

    val default: Node = LeafNode()
    nodeToExpr(population.foldLeft(default)(insert))
  }
}

object ModDemo {

  // build histogram of all products of a*b mod p
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
      .sortBy(pair => (pair._2, pair._1))
      .reverse
  }

  // print a histogram of percentages of all the values
  // of a*b mod p
  def main(argv: Array[String]): Unit = {
    val p = 2

    println(allProducts(p))
  }
}

object TestExprBySize {
  import Expr.{eval, balancedRandBySize, naiveRandBySize}
  import adjuvant.Adjuvant.returnPercentages

  def main(argv: Array[String]): Unit = {
    val size = 1 << 6
    val p = 5
    val m = 100
    println("Balanced", returnPercentages(m, () => eval(balancedRandBySize(size, p), p)))
    println("Naive   ", returnPercentages(m, () => eval(naiveRandBySize(size, p), p)))
    println("All     ", ModDemo.allProducts(p))
  }
}

object TestExprByDepth {

  import Expr.{eval, balancedRandByDepth, naiveRandByDepth}
  import adjuvant.Adjuvant.returnPercentages

  def main(argv: Array[String]): Unit = {
    val depth = 6
    val p = 5
    val m = 100000
    println("Balanced", returnPercentages(m, () => eval(balancedRandByDepth(depth, p), p)))
    println("Naive   ", returnPercentages(m, () => eval(naiveRandByDepth(depth, p), p)))
    println("All     ", ModDemo.allProducts(p))
  }
}
