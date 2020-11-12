// Copyright (c) 2020 EPITA Research and Development Laboratory
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


package lbdd


sealed abstract class BinaryOperations {
  def apply(): LBdd
  def apply(b: LBdd): LBdd
  def apply(b1: LBdd, b2: LBdd): LBdd
  val commutative: Boolean

  def apply(args: List[LBdd]): LBdd = {
    args match {
      case Nil => apply()
      case b :: Nil => apply(b)
      case _ => args.reduceLeft {
        (acc, b) => apply(acc, b)         // TODO : Better ?
      }
    }
  }

  def apply(args: LBdd*): LBdd = apply(args.toList)
}


object Or extends BinaryOperations {
  val commutative = true
  def apply(): LBdd = LBddFalse
  def apply(b: LBdd): LBdd = b


  def orOp(b1: LBddNode, b2: LBddNode): LBdd = {
    if (b1.label == b2.label)
      LBdd(b1.label, apply(b1.positive, b2.positive),
                     apply(b1.middle, b2.middle),
                     apply(b1.negative, b2.negative),
                     b1.negation)
    else if (b1.label < b2.label)
      LBdd(b1.label, b1.positive,
                     apply(l = b1.middle, b = b2),
                     b1.negative,
                     b1.negation)
    else
      LBdd(b2.label, b2.positive,
                     apply(b = b1, l = b2.middle),
                     b2.negative,
                     b2.negation)
  }


  def apply(b1: LBdd, b2: LBdd): LBdd = {
    (b1, b2) match {
      case (_, _) if b1 eq b2 => b1
      case (LBddTrue, _) => LBddTrue
      case (LBddFalse, _) => b2
      case (_, LBddTrue) => LBddTrue
      case (_, LBddFalse) => b1
      case (b1: LBddNode, b2: LBddNode) => orOp(b1, b2)
    }
  }

  def apply(l1: lazyNode, l2: lazyNode): lazyNode = {
    (l1, l2) match {
      case (None, None) => None
      case (None, _) => l2
      case (_, None) => l1
      case (_, _) => lazify(apply(l1.get(), l2.get()))
    }
  }

  def apply(b: LBdd, l: lazyNode): lazyNode = {
    (b, l) match {
      case (_, None) => lazify(b)
      case (LBddTrue, _) => f_true
      case (LBddFalse, _) => l
      case (b: LBddNode, l: lazyNode) => lazify(apply(b, l.get()))
    }
  }
}


object And extends BinaryOperations {
  val commutative = true
  def apply(): LBdd = LBddTrue
  def apply(b: LBdd): LBdd = b

  def andOp(b1: LBddNode, b2: LBddNode): LBdd = {
    if (b1.label == b2.label)
      LBdd(b1.label, apply(Or(b1.positive, b1.middle),
                           Or(b2.positive, b2.middle)),
                     None,
                     apply(Or(b1.negative, b1.middle),
                           Or(b2.negative, b2.middle)),
                     b1.negation)
    else if (b1.label < b2.label)
      LBdd(b1.label, apply(b1.positive, b2),
                     apply(l = b1.middle, b = b2),
                     apply(b1.negative, b2),
                     b1.negation)
    else
      LBdd(b2.label, apply(b1, b2.positive),
                     apply(b = b1, l = b2.middle),
                     apply(b1, b2.negative),
                     b2.negation)
  }

  def apply(b: LBdd, l: lazyNode): lazyNode = {
    (b, l) match {
      case (_, None) => None
      case (LBddFalse, _) => None
      case (LBddTrue, _) => l
      case (b: LBddNode, l: lazyNode) => lazify(apply(b, l.get()))
    }
  }

  def apply(l1: lazyNode, l2: lazyNode): LBdd = {
    (l1, l2) match {
      case (None, None) => LBddFalse     // TODO : ERROR ??
      case (None, _) => l2.get()
      case (_, None) => l1.get()
      case (_, _) => apply(l1.get(), l2.get())
    }
  }

  def apply(b1: LBdd, b2: LBdd): LBdd = {
    (b1, b2) match {
      case (_, _) if b1 eq b2 => b1
      case (LBddTrue, _) => b2
      case (_, LBddTrue) => b1
      case (LBddFalse, _) => LBddFalse
      case (_, LBddFalse) => LBddFalse
      case (b1: LBddNode, b2: LBddNode) => andOp(b1, b2)
    }
  }
}


//object Not {
//  def apply(): LBdd = LBddFalse
//
//  // TODO : Do not unlazify ! Change return type
//  def apply(l: lazyNode): LBdd = {
//    unlazify(lazyNode(l.f, !l.negation))
//  }
//
//  def apply(b: LBdd): LBdd = {
//    b match {
//      case LBddFalse => LBddTrue
//      case LBddTrue => LBddFalse
//      case b: LBddNode => LBdd(b.label, b.positive, b.middle, b.negative, !b.negation)
//    }
//  }
//}

object Not {
  def apply(): LBdd = LBddFalse

  def apply(l: lazyNode): LBdd = {
    l match {
      case None => LBddFalse
      case f => apply(f.get())
    }
  }

  // TODO : Do not understand why this works
  def apply(b: LBdd): LBdd = {
    b match {
      case LBddFalse => LBddTrue
      case LBddTrue => LBddFalse
      case b: LBddNode =>
        LBdd(b.label, apply(Or(b.positive, b.middle)),
                      None,
                      apply(Or(b.negative, b.middle)))
    }
  }
}


object AndNot extends BinaryOperations {
  val commutative = false
  def apply():LBdd = sys.error("0-ary AndNot not implemented")
  def apply(b: LBdd): LBdd = sys.error("unary AndNot not implemented")

  def andNotOp(b1: LBddNode, b2: LBddNode): LBdd = {
    if (b1.label == b2.label)
      LBdd(b1.label, apply(b1.positive, b2.positive),
                     apply(b1.middle, b2.middle),
                     apply(b1.negative, b2.negative),
                     b1.negation)
    else if (b1.label < b2.label)
      LBdd(b1.label, apply(Or(b1.positive, b1.middle), b2),
                     None,
                     apply(Or(b1.negative, b1.middle), b2),
                     b1.negation)
    else
      LBdd(b2.label, apply(b1, Or(b2.positive, b2.middle)),
                     None,
                     apply(b1, Or(b2.negative, b2.middle)),
                     b2.negation)
  }

  def apply(l1: lazyNode, l2: lazyNode): lazyNode = {
    (l1, l2) match {
      case (None, None) => None
      case (None, _) => None         // TODO : Not sure, None ?
      case (_, None) => None
      case (_, _) => lazify(apply(l1.get(), l2.get()))
    }
  }

  def apply(l: lazyNode, b: LBdd): LBdd = {
    (l, b) match {
      case (None, _) => LBddFalse       // TODO : Not sure...
      case (_, LBddTrue) => LBddFalse
      case (_, _) if l eq f_false => LBddFalse
      case (_, LBddFalse) if l eq f_true => LBddTrue
      case (_, _) => apply(l.get(), b)
    }
  }

  def apply(b: LBdd, l: lazyNode): LBdd = {
    (b, l) match {
      case (_, None) => LBddFalse       // TODO : ...
      case (LBddFalse, _) => LBddFalse
      case (_, _) if l eq f_true => LBddFalse
      case (LBddTrue, _) if l eq f_false => LBddTrue
      case (_, _) => apply(b, l.get())
    }
  }

  def apply(b1: LBdd, b2: LBdd): LBdd = {
    (b1, b2) match {
      case (_, _) if b1 eq b2 => LBddFalse
      case (LBddFalse, _) => LBddFalse
      case (_, LBddTrue) => LBddFalse
      case (LBddTrue, LBddFalse) => LBddTrue
      case (b1: LBddNode, LBddFalse) => b1
      case (LBddTrue, b2: LBddNode) => Not(b2)
      case (b1: LBddNode, b2: LBddNode) => andNotOp(b1, b2)
    }
  }
}


object Xor extends BinaryOperations {
  val commutative = true
  def apply():LBdd = LBddFalse
  def apply(b: LBdd): LBdd = b

  def apply(b1: LBdd, b2: LBdd): LBdd = {

    // TODO : lazify

    Or(AndNot(b1, b2), AndNot(b2, b1))
  }
}


object Xnor extends BinaryOperations {
  val commutative = true
  def apply(): LBdd = LBddTrue
  def apply(b: LBdd): LBdd = b

  def apply(b1: LBdd, b2: LBdd): LBdd = {

    // TODO : lazify

    Or(And(b1, b2), And(Not(b1), Not(b2)))
  }
}
