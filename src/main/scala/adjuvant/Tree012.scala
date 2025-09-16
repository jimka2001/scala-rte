package adjuvant

import scala.util.Random

sealed abstract class Tree012

case class Tree012Leaf() extends Tree012

case class Tree012Unary(value:Int,
                           child: Tree012) extends Tree012

case class Tree012Binary(value:Int,
                            left: Tree012,
                            right: Tree012) extends Tree012

object Tree012 {
  def insert(tree:Tree012, probability:Float, value:Int):Tree012 = {
    tree match {
      case Tree012Leaf() =>
        val random = new scala.util.Random
        if ( random.between(0.0, 1.0) < probability)
          Tree012Binary(value, tree, tree)
        else
          Tree012Unary(value, tree)
      case Tree012Unary(parent, child) =>
        Tree012Unary(parent, insert(child, probability, value))
      case Tree012Binary(parent, left, right) =>
        if (value < parent)
          Tree012Binary(parent, insert(left, probability, value), right)
        else
          Tree012Binary(parent, left, insert(right, probability, value))
    }
  }

  def build(probability:Float, population:Seq[Int]):Tree012 = {
    val tree:Tree012 = Tree012Leaf()
    population.foldLeft(tree){
      (accTree:Tree012, item:Int) => insert(accTree, probability, item)
    }
  }
  val random = new scala.util.Random
  def rand(probability_binary:Float, depth:Int):Tree012 = {
    assert(depth >= 0)
    assert(0.0 < probability_binary && probability_binary < 1.0)

    val k = math.pow(2, depth).round.toInt
    val num_leaves = k + random.nextInt(k)

    build(probability_binary, Random.shuffle((0 to num_leaves).to(List)))
  }
}
