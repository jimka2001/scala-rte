package demos.scalaio2025
import adjuvant.Adjuvant.randCase
abstract class Expr

case class Plus(a:Expr, b:Expr) extends Expr
case class Times(a:Expr, b:Expr) extends Expr
case class Const(n:Int) extends Expr



object Expr {
  val random = new scala.util.Random

  def eval(e: Expr, p: Int): Int = {
    e match {
      case Plus(a, b) => (eval(a, p) + eval(b, p)) % p
      case Times(a, b) => (eval(a, p) * eval(b, p)) % p
      case Const(n) => n % p
    }
  }

  def naiveRand(depth: Int, p:Int): Expr = {
    if (depth == 0)
      Const(random.nextInt(p))
    else
      randCase(() => Const(random.nextInt(p)),
               Seq((0.333, () => Plus(naiveRand(depth-1, p), naiveRand(depth-1, p))),
                   (0.333, () => Times(naiveRand(depth-1, p), naiveRand(depth-1, p)))))
  }

  def balancedRand(depth:Int, p:Int): Expr = {
    val nodeCount = random.between(1 << depth, 1 << (depth+1))
    val population = scala.util.Random.shuffle((0 until nodeCount).to(List))

    abstract class Node
    case class InternalNode(n:Int, left:Node, right:Node) extends Node
    case class LeafNode() extends Node

    def insert(node:Node, a:Int):Node = {
      node match {
        case InternalNode(n, left, right) if a < n => InternalNode(n, insert(left, a), right)
        case InternalNode(n, left, right) => InternalNode(n, left, insert(right, a))
        case LeafNode() => InternalNode(a, LeafNode(), LeafNode())
      }
    }
    def nodeToExpr(node:Node): Expr = {
      node match {
        case InternalNode(_, left, right) =>
          randCase(() => Plus(nodeToExpr(left), nodeToExpr(right)),
            Seq((0.5, () => Times(nodeToExpr(left), nodeToExpr(right)))))
        case LeafNode() => Const(random.nextInt(p))
      }
    }
    val default:Node = LeafNode()
    nodeToExpr(population.foldLeft(default)(insert))
  }

}

object TestExpr {
  import Expr.{eval,balancedRand, naiveRand}
  import adjuvant.Adjuvant.measureFrequencies
  def main(argv:Array[String]):Unit = {
    val p = 7
    println(measureFrequencies(10000,() => eval(balancedRand(3, p), p)).to(List).sortBy(_._1))
    println(measureFrequencies(10000,() => eval(naiveRand(3,p),p)).to(List).sortBy(_._1))

  }
}

object Mod {
  def mul_mod(a:Int, b:Int, p:Int):Int = {
    (a * b) % p
  }

  def frequencies[A](population:Seq[A]):(Map[A,Int],Map[Int,Set[A]]) = {
    val fr = for{
      (a,as) <- population.groupBy(identity)
    } yield (a, as.length)

    val revmap = fr.foldLeft(Map[Int,Set[A]]())((acc,item) => {
      val (a:A,n:Int) = item
      acc.get(n) match {
        case None => acc + (n -> Set(a))
        case Some(s) => acc + (n -> (s ++ Set(a)))
        }
    })

    (fr, revmap)
  }

}

object ModDemo {

  def main(argv:Array[String]):Unit = {
    val p = 15
    def mul(a:Int, b:Int):Int = Mod.mul_mod(a,b,p)

    val products = for{a <- 0 to p-1
                       b <- 0 to p-1} yield mul(a,b)
    println(products)
    val (fr, revmap) = Mod.frequencies(products)

    println(revmap)
    println(fr)

  }
}