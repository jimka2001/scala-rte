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
    val betterOperands = operands
      .distinct
      .map(_.canonicalizeOnce)
      .distinct
      .filterNot(_ == EmptySet)
    val singletons: List[genus.SimpleTypeD] = betterOperands.flatMap {
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


    // predicate to match form like this Cat(X, Y, Z, Star( Cat(X, Y, Z)))
    def catxyzp(rt:Rte):Boolean = rt match {
      case Cat(Seq(rs1@_*)) =>{
        // at least length 2
        if (rs1.sizeIs >= 2) {
          val rightmost = rs1.last
          val leading = rs1.dropRight(1)
          rightmost match {
            case Star(Cat(Seq(rs2@_*))) => leading == rs2
            case _ => false
          }
        }
        else
          false
      }
      case _ => false
    }
    // Or(Not(<[= 0]>),(<[= 1]>)*) -> Some(Not(<[= 0]>))
    lazy val dominantNotSingleton = betterOperands.find{
      case Not(Singleton(td1)) if betterOperands.exists{
        case Star(Singleton(td2)) => td1.disjoint(td2).contains(true)
        case _ => false
      } => true
      case _ => false
    }
    lazy val maybeCatxyz = betterOperands.find(catxyzp) // (:cat X Y Z (:* (:cat X Y Z)))
    // Or() --> EmptySet
    if (betterOperands.isEmpty)
      EmptySet
    else if (betterOperands.sizeIs == 1) {
      // Or(x) --> x
      betterOperands.head
    }
    else if (betterOperands.contains(Rte.sigmaStar))
      Rte.sigmaStar
    else if (betterOperands.exists(Rte.isOr)) {
      // Or(a,Or(x,y),b) --> Or(a,x,y,b)
      val orops = betterOperands.flatMap {
        case Or(Seq(rs@_*)) => rs
        case r => Seq(r)
      }
      Or(orops)
    }
    else if (betterOperands.exists { r1 => Rte.isStar(r1) && betterOperands.exists { r2 => Star(r2) == r1 }
    })
    // (:or A B (:* B) C)
    // --> (:or A (:* B) C)
      Or(betterOperands.flatMap {
        case r2 if betterOperands.exists { r1 => Rte.isStar(r1) && Star(r2) == r1 } => Seq()
        case rt => Seq(rt)
      })
    // (:or A :epsilon B (:cat X (:* X)) C)
    //   --> (:or A :epsilon B (:* X) C )
    // (:or :epsilon (:cat X (:* X)))
    //   --> (:or :epsilon (:* X))
    else if (existsNullable && maybePlus.nonEmpty)
      Or(betterOperands.map {
        case Cat(Seq(x, z@Star(y))) if x == y => z
        case Cat(Seq(z@Star(x), y)) if x == y => z
        case rt => rt
      })
    // (:or A :epsilon B (:cat X Y Z (:* (:cat X Y Z))) C)
    //   --> (:or A :epsilon B (:* (:cat X Y Z)) C )
    // (:or :epsilon (:cat X Y Z (:* (:cat X Y Z))))
    //   --> (:or :epsilon (:* (:cat X Y Z)))
    else if (existsNullable && maybeCatxyz.nonEmpty)
      Or(betterOperands.map{
        case c@Cat(Seq(rs@_*)) if maybeCatxyz.contains(c) => rs.last
        case rt => rt
      })

    // (:or A :epsilon B (:* X) C)
    //   --> (:or A B (:* X) C)
    else if (betterOperands.contains(EmptyWord) && betterOperands.exists(r => r != EmptyWord && r.nullable))
      Or(betterOperands.filterNot(_ == EmptyWord))
    else if (maybeSub.nonEmpty)
      Or(betterOperands.filterNot(_ == Singleton(maybeSub.get)))
    else if (betterOperands.contains(Rte.sigmaSigmaStarSigma) && betterOperands.exists{
      case Not(Singleton(_)) => true
      case _ => false})
      Or(betterOperands.filter(_!= Rte.sigmaSigmaStarSigma))
    else if (dominantNotSingleton.nonEmpty)
    // Or(Not(<[= 0]>),(<[= 1]>)*) did not equal Not(<[= 0]>)
      dominantNotSingleton.get
    else
      Or(betterOperands)
  }
  def derivativeDown(wrt:genus.SimpleTypeD):Rte = Or(operands.map(rt => rt.canonicalize.derivative(Some(wrt))))
}

object Or {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new Or(operands)
}
