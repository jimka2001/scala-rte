package rte

class RteSyntaxSugar

object Plus extends RteSyntaxSugar {
  def apply(rte:Rte):Rte = {
    Cat(rte,Star(rte))
  }
}


object Permute extends RteSyntaxSugar{
  import rte.Or.createOr
  import rte.Cat.createCat

  def apply(operands: Rte*):Rte = {
    val perms = for {perm <- operands.permutations
                     } yield createCat(perm)
    createOr(perms.toSeq)
  }
}

object AndNot extends RteSyntaxSugar {
  import rte.Or.createOr

  def apply(operands:Rte*):Rte = {
    assert(operands.length > 0, "not supported 0-ary AndNot")
    And(operands.head, Not(createOr(operands.tail)))
  }
}

object Xor extends RteSyntaxSugar {
  def apply(a:Rte, b:Rte):Rte = Or(And(a,Not(b)),
                                   And(Not(a),b))
}

object Optional extends RteSyntaxSugar {
  def apply(a:Rte):Rte = Or(a, EmptySeq)
}

object Exponent extends RteSyntaxSugar {
  def apply(a:Rte, n:Short): Rte = {
    n match {
      case 0 => EmptySeq
      case 1 => a
      case i if i > 1 => Cat(Seq.fill(n)(a.canonicalize))
      case i if i < 0 => throw new Error("^ operator does not work with negative numbers: $n")
    }
  }
}

object Member extends RteSyntaxSugar {
  import genus.SMember
  def apply(xs: Any*): Rte = Singleton(SMember(xs: _*))
}

object Satisfies extends RteSyntaxSugar {
  import genus.SSatisfies

  def apply(f: Any=>Boolean, text:String): Rte = Singleton(SSatisfies(f,text))
}

object Eql extends RteSyntaxSugar {
  import genus.SEql

  def apply(x: Any): Rte = Singleton(SEql(x))
}

object Atomic extends RteSyntaxSugar {
  import genus.SAtomic
  def apply(ct: Class[_]): Rte =
    Singleton(SAtomic(ct))
}
