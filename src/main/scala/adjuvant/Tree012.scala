package adjuvant

import scala.util.Random

sealed abstract class Tree012[A]

case class Tree012Leaf[A]() extends Tree012[A]

case class Tree012Unary[A](value:A,
                           child: Tree012[A]) extends Tree012[A]

case class Tree012Binary[A](value:A,
                            left: Tree012[A],
                            right: Tree012[A]) extends Tree012[A]

object Tree012 {
  def insert[A <: Ordered[A]](tree:Tree012[A], probability:Float, value:A):Tree012[A] = {
    tree match {
      case Tree012Leaf =>
        val random = new scala.util.Random
        if ( random.between(0.0, 1.0) < probability)
          Tree012Binary[A](value, tree, tree)
        else
          Tree012Unary[A](value, tree)
      case Tree012Unary(parent, child) =>
        Tree012Unary(parent, insert(child, probability, value))
      case Tree012Binary(parent, left, right) =>
        if (value < parent)
          Tree012Binary(parent, insert(left, probability, value), right)
        else
          Tree012Binary(parent, left, insert(right, probability, value))
    }
  }

  def build[A](probability:Float, population:Seq[A]):Tree012[A] = {
    val tree:Tree012[A] = Tree012Leaf[A]()
    population.foldLeft(tree){
      (accTree:Tree012[A], item:A) => insert(accTree, probability, item)
    }
  }
  val random = new scala.util.Random
  def rand(probability_binary:Float, depth:Int):Tree012[Int] = {
    assert(depth >= 0)
    assert(0.0 < probability_binary && probability_binary < 1.0)

    val k = math.pow(2, depth).round.toInt
    val num_leaves = k + random.nextInt(k)

    build(probability_binary, Random.shuffle((0 to num_leaves).to(List)))
  }
}
