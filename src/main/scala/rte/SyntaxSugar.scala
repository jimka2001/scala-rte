package rte
import rte.Cat.createCat
import rte.Or.createOr

object Plus{
  def apply(rte:Rte):Rte = {
    Cat(rte,Star(rte))
  }
}


object Permute{
  def apply(operands: Rte*):Rte = {
    val perms = for {perm <- operands.permutations
                     } yield createCat(perm)
    createOr(perms.toSeq)
  }
}

object AndNot{
  def apply(operands:Rte*):Rte = {
    assert(operands.length > 0, "not supported 0-ary AndNot")
    And(operands.head, Not(createOr(operands.tail)))
  }
}

object Xor {
  def apply(a:Rte, b:Rte):Rte = Or(And(a,Not(b)),
                                   And(Not(a),b))
}

object Optional{
  def apply(a:Rte):Rte = Or(a, EmptyWord)
}

object Exponent{
  def apply(a:Rte, n:Short): Rte = {
    n match {
      case 0 => EmptyWord
      case 1 => a
      case i if i > 1 => Cat(Seq.fill(n)(a.canonicalize))
      case i if i < 0 => throw new Error("^ operator does not work with negative numbers: $n")
    }
  }
}
