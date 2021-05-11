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

case class And(operands:Seq[Rte]) extends Rte{
  import genus.SimpleTypeD
  override def toLaTeX:String = operands.map(_.toLaTeX).mkString("(", "\\wedge ", ")")
  override def toString:String = operands.map(_.toString).mkString("And(", ",", ")")
  def nullable:Boolean = operands.forall{_.nullable}
  def firstTypes:Set[SimpleTypeD] = operands.toSet.flatMap((r:Rte) => r.firstTypes)
  override def canonicalizeOnce:Rte = {

    val betterOperands = operands
      .distinct
      .map(_.canonicalizeOnce)
      .distinct
    val matchesOnlySingletons:Boolean = betterOperands.contains(Sigma) || betterOperands.exists(Rte.isSingleton)
    val singletons:List[genus.SimpleTypeD] = betterOperands.flatMap{
      case Singleton(td) => List(td)
      case Not(Singleton(td)) if matchesOnlySingletons => List(genus.SNot(td))
      case _ => List.empty
    }.toList
    lazy val singletonIntersection = genus.SAnd(singletons:_*)
    lazy val canonicalizedSingletons = singletonIntersection.canonicalize()
    lazy val singletonsInhabited = canonicalizedSingletons.inhabited
    lazy val maybeSuper:Option[Rte] = singletons.find{sup =>
      singletons.exists { sub =>
        sub != sup && sub.subtypep(sup).contains(true)
      }}.flatMap{
      case genus.SNot(td) => Some(Not(Singleton(td)))
      case td => Some(Singleton(td))
    }

    if (betterOperands.isEmpty)
      Rte.sigmaStar
    else if (betterOperands.sizeIs == 1)
      betterOperands.head
    else if(betterOperands.contains(EmptySet))
      EmptySet
    else if (betterOperands.contains(EmptyWord) && matchesOnlySingletons)
      EmptySet
    else if ( betterOperands.contains(EmptyWord)) {
      if ( betterOperands.forall(_.nullable) )
        EmptyWord
      else
        EmptySet
    }
    else if (betterOperands.contains(Rte.sigmaStar))
      And(betterOperands.filterNot(_ == Rte.sigmaStar))
    else if (betterOperands.exists(Rte.isAnd)) {
      val andops = betterOperands.flatMap{
        case And(Seq(rs @ _*)) => rs
        case r => Seq(r)
      }
      And(andops)
    } else if (betterOperands.exists(Rte.isOr)) {
      // And(A,B,Or(X,Y,Z),C,D)
      // --> Or(And(A,B,   X,   C, C)),
      //        And(A,B,   Y,   C, C)),
      //        And(A,B,   Z,   C, C)))
      betterOperands.find(Rte.isOr) match {
        case Some(x@Or(Seq(rs @ _*))) =>
          Or(rs.map{r => And(Rte.searchReplace(betterOperands,x,r))})
      }
    } else if (betterOperands.exists(r1 => betterOperands.contains(Not(r1))))
      EmptySet
    else if (singletons.exists(td => td.inhabited.contains(false)))
      EmptySet
    else if (betterOperands.contains(Sigma) && singletons.exists(td => td.inhabited.contains(true)))
      And(betterOperands.filterNot(_ == Sigma))
    else if (betterOperands.contains(Rte.sigmaStar) && singletons.exists(td => td.inhabited.contains(true)))
      And(betterOperands.filterNot(_ == Rte.sigmaStar))
    else if ( singletons.tails.exists{
      case t1::ts => ts.exists{t2 => t1.disjoint(t2).contains(true)}
      case _ => false
    })
      EmptySet
    else if ( maybeSuper.nonEmpty)
      And(betterOperands.filterNot(maybeSuper.contains(_)))
    else if ((betterOperands.contains(Sigma)
      || singletons.exists(td => td.inhabited.contains(true)))
      && betterOperands.exists(Rte.isCat)
      && betterOperands.exists{
        case c@Cat(Seq(_*)) => c.minLength > 1
        case _ => false
      }
    )
      EmptySet
    else if (matchesOnlySingletons && singletonsInhabited.contains(false))
      EmptySet
    else if (canonicalizedSingletons == genus.SEmpty)
      EmptySet
    else if (matchesOnlySingletons && singletons.exists(genus.Types.memberp))
      Singleton(canonicalizedSingletons)
    else
      And(betterOperands)
  }
  def derivativeDown(wrt:genus.SimpleTypeD):Rte = And(operands.map(rt => rt.canonicalize.derivative(Some(wrt))))
}

object And {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new And(operands)
}

object AndSanity {
  def main(argv:Array[String]):Unit = {
    print(And(Singleton(genus.SEql(0)),Sigma).canonicalize)
  }
}