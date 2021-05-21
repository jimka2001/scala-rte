// Copyright (c) 2021 EPITA Research and Development Laboratory
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
//

package rte

case class Or(operands:Seq[Rte]) extends Rte {
  override def toLaTeX: String = "(" + operands.map(_.toLaTeX).mkString("\\vee ") + ")"

  override def toString: String = operands.map(_.toString).mkString("Or(", ",", ")")

  def nullable: Boolean = operands.exists {
    _.nullable
  }

  def firstTypes: Set[genus.SimpleTypeD] = operands.toSet.flatMap((r: Rte) => r.firstTypes)

  override def canonicalizeOnce: Rte = {
    //println("canonicalizing Or:  " + operands)
    val betterOperands = operands
      .distinct
      .flatMap{
        case EmptySet => Seq()
        case Or(Seq(rs @ _*)) => rs.map(_.canonicalizeOnce)
        case r => Seq(r.canonicalizeOnce)
      }

    lazy val singletons: List[genus.SimpleTypeD] = betterOperands.flatMap {
      case Singleton(td) => List(td)
      case _ => List.empty
    }.toList
    lazy val maybeSub = singletons.find { sub =>
      singletons.exists { sup =>
        sub != sup && sub.subtypep(sup).contains(true)
      }
    }
    lazy val maybePlus = betterOperands.find(Rte.isPlus)
    lazy val existsNullable = betterOperands.exists(_.nullable)

    // Or(Not(<[= 0]>),(<[= 1]>)*) -> Some(Not(<[= 0]>))
    lazy val dominantNotSingleton = betterOperands.find{
      case Not(Singleton(td1)) if betterOperands.exists{
        case Star(Singleton(td2)) => td1.disjoint(td2).contains(true)
        case _ => false
      } => true
      case _ => false
    }
    lazy val maybeCatxyz = betterOperands.find(Rte.catxyzp) // (:cat X Y Z (:* (:cat X Y Z)))
    // Or() --> EmptySet
    if (betterOperands.isEmpty)
      EmptySet
    else if (betterOperands.sizeIs == 1) {
      // Or(x) --> x
      betterOperands.head
    }
    else if (betterOperands.contains(Rte.sigmaStar))
      Rte.sigmaStar
    else if (betterOperands.exists { r1 => Rte.isStar(r1) && betterOperands.exists { r2 => Star(r2) == r1 }
    })
    // (:or A B (:* B) C)
    // --> (:or A (:* B) C)
      Or.createOr(betterOperands.flatMap {
        case r2 if betterOperands.exists { r1 => Rte.isStar(r1) && Star(r2) == r1 } => Seq()
        case rt => Seq(rt)
      })
    // (:or A :epsilon B (:cat X (:* X)) C)
    //   --> (:or A :epsilon B (:* X) C )
    // (:or :epsilon (:cat X (:* X)))
    //   --> (:or :epsilon (:* X))
    else if (existsNullable && maybePlus.nonEmpty)
      Or.createOr(betterOperands.map {
        case Cat(Seq(x, z@Star(y))) if x == y => z
        case Cat(Seq(z@Star(x), y)) if x == y => z
        case rt => rt
      })
    // (:or A :epsilon B (:cat X Y Z (:* (:cat X Y Z))) C)
    //   --> (:or A :epsilon B (:* (:cat X Y Z)) C )
    // (:or :epsilon (:cat X Y Z (:* (:cat X Y Z))))
    //   --> (:or :epsilon (:* (:cat X Y Z)))
    else if (existsNullable && maybeCatxyz.nonEmpty)
      Or.createOr(betterOperands.map{
        case c@Cat(Seq(rs@_*)) if maybeCatxyz.contains(c) => rs.last
        case rt => rt
      })

    // (:or A :epsilon B (:* X) C)
    //   --> (:or A B (:* X) C)
    else if (betterOperands.contains(EmptyWord) && betterOperands.exists(r => r != EmptyWord && r.nullable))
      Or.createOr(betterOperands.filterNot(_ == EmptyWord))
    else if (maybeSub.nonEmpty)
      Or.createOr(betterOperands.filterNot(_ == Singleton(maybeSub.get)))
    else if (betterOperands.contains(Rte.sigmaSigmaStarSigma) && betterOperands.exists{
      case Not(Singleton(_)) => true
      case _ => false})
      Or.createOr(betterOperands.filter(_!= Rte.sigmaSigmaStarSigma))
    else if (betterOperands.exists{
      // Or(A,Not(A),X) -> Sigma
      case Not(rt) if betterOperands.contains(rt) => true
      case _ => false
    })
      Rte.sigmaStar
    else if (betterOperands.exists{
      // Or(A,Not(B),X) -> Sigma if B is subtype of A
      case Not(Singleton(sub)) if betterOperands.exists{
        case Singleton(sup) if sub.subtypep(sup).contains(true) => true
        case _ => false
      } => true
      case _ => false
    })
      Rte.sigmaStar
    else if ( betterOperands.size == 2 && dominantNotSingleton.nonEmpty)
    // Or(Not(<[= 0]>),(<[= 1]>)*) did not equal Not(<[= 0]>)
      dominantNotSingleton.get
    else
      Or.createOr(betterOperands)
  }
  def derivativeDown(wrt:genus.SimpleTypeD):Rte = Or.createOr(operands.map(rt => rt.derivative(Some(wrt))))
}

object Or {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new Or(operands)
  def createOr(operands: Seq[Rte]):Rte = {
    operands match {
      case Seq() => EmptySet
      case Seq(rt) => rt
      case _ => Or(operands)
    }
  }
}
