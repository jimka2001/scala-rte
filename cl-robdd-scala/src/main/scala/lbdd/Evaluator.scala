package lbdd


object Evaluator {

  def apply(b: LBdd, l: Map[Int, Boolean]): Boolean = {
    b match {
      case LBddFalse => false
      case LBddTrue => true
      case node: LBddNode => apply(node, l)
    }
  }

  def apply(node: LBddNode, l: Map[Int, Boolean]): Boolean = {
    val value = l(node.label)
    if (value)
      apply(node.positive, l) || apply(node.middle, l)
    else
      apply(node.negative, l) || apply(node.middle, l)
  }

  def apply(node: lazyNode, l: Map[Int, Boolean]): Boolean = {
    if (node.isEmpty)
      false
    else
      apply(node.get(), l)
  }

}


object main {
  def main(args: Array[String]): Unit = {
    println(Evaluator(LBdd(1), Map(1 -> false)))
  }
}
