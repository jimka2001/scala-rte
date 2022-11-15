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

import adjuvant.Adjuvant.{findSimplifier, searchReplace, uniquify}
import genus._

abstract class Combination(val operands:Seq[Rte]) extends Rte {
  val zero:Rte
  val one:Rte
  def create(operands: Seq[Rte]):Rte
  def createTypeD(operands: Seq[SimpleTypeD]):SimpleTypeD
  def createDual(operands: Seq[Rte]):Rte
  def sameCombination(c:Combination):Boolean
  def dualCombination(d:Combination):Boolean
  def firstTypes: Set[SimpleTypeD] = operands.toSet.flatMap((r: Rte) => r.firstTypes)
  def annihilator(a:SimpleTypeD,b:SimpleTypeD):Option[Boolean]
  def orInvert(x:Boolean):Boolean
  def setOperation(a:Seq[Any],b:Seq[Any]):Seq[Any]
  def setDualOperation(a:Seq[Any],b:Seq[Any]):Seq[Any]

  def conversionC3():Rte = {
    // Or(... Sigma* ....) -> Sigma*
    // And(... EmptySet ....) -> EmptySet
    if (operands.contains(zero))
      zero
    else
      this
  }

  def conversionC4():Rte = {
    create(uniquify(operands))
  }

  def conversionC5():Rte = {
    create(Rte.sortAlphabetically(operands))
  }

  def conversionC6():Rte = {
    // remove Sigma* and flatten And(And(...)...)
    // remove EmptySet and flatten Or(Or(...)...)
    create(operands.flatMap{
      case r if r == one => Seq()
      case c:Combination if sameCombination(c) => c.operands
      case r => Seq(r)
    })
  }

  def conversionC7():Rte = {
    // (:or A B (:* B) C)
    // --> (:or A (:* B) C)
    // (:and A B (:* B) C)
    // --> (:and A B C)
    val stars = operands.collect{
      case rt@Star(_) => rt
    }
    if (stars.isEmpty)
      this
    else
      create(operands.flatMap{
        case r1 if Rte.isOr(this) && stars.contains(Star(r1))  => Seq()
        case Star(r1) if Rte.isAnd(this) && operands.contains(r1) => Seq()
        case r => Seq(r)
      })
  }

  def conversionC11():Rte = {
    // And(...,x,Not(x)...) -> EmptySet
    // Or(...,x,Not(x),...) -> Sigma *
    if (operands.exists(r1 => operands.contains(Not(r1))))
      zero
    else
      this
  }

  def conversionC14():Rte = {
    // Or(A,Not(B),X) -> Sigma* if B is subtype of A
    // And(A,Not(B),X) -> EmptySet if A is subtype of B
    val nots = operands.collect{
      case Not(Singleton(sub)) => sub
    }
    lazy val singletons = operands.collect{
      case Singleton(sup) => sup
    }

    nots.find{sub => singletons.exists(sup => annihilator(sup,sub).contains(true))} match {
      case None => this
      case Some(_) => zero
    }
  }


  def conversionC12():Rte = {
    lazy val cats: Seq[Cat] = operands.collect{
      case c:Cat if c.operands.count{o => ! o.nullable} > 1 => c
    }
    val notSing: Seq[Not] = operands.collect{
      case n@Not(Singleton(_)) => n
    }
    // sigmaSigmaStarSigma = Cat(Sigma, Sigma, sigmaStar)
    // Or(   A, B, ... Cat(Sigma,Sigma,Sigma*) ... Not(Singleton(X)) ...)
    //   --> Or( A, B, ... Not(Singleton(X))
    // This is correct because Cat(Σ,Σ,(Σ)*) is the set of all sequences of length 2 or more
    //    and Not(Singleton(X)) includes the set of all sequences of length 2 or more
    // Similarly, any Cat(...) which contains at least two non-nullables is either the
    //    empty set, or a set of sequences each of length 2 or more.
    //    And Not(Singleton(X)) contains all all sequences of length 2 or more.
    // So se can remove all such sequences.
    if (notSing.isEmpty || cats.isEmpty)
      this
    else
      this match {
        case _:Or => create(operands.filterNot(cats.contains) )
        case _:And => create(operands.filterNot(notSing.contains) )
        case _ => throw new Exception(s"expecting And or Or, not $this")
    }
  }

  def conversionC15():Rte = {
    // simplify to maximum of one SMember(...) and maximum of one Not(SMember(...))
    // Or(<{1,2,3,4}>,<{4,5,6,7}>,Not(<{10,11,12,13}>,Not(<{12,13,14,15}>)))
    //   --> Or(<{1,2,3,4,6,7}>,Not(<{12,13}>))
    //
    // And(<{1,2,3,4}>,<{4,5,6,7}>,Not(<{10,11,12,13}>,Not(<{12,13,14,15}>)))
    //   --> And(<{3,4}>,Not(<{10,11,12,13,14,15}>))
    val members = operands.collect{
      case s@Singleton(_:SMemberImpl) => s
    }
    val notMembers = operands.collect{
      case s@Not(Singleton(_:SMemberImpl)) => s
    }
    if (members.size <= 1 && notMembers.size <= 1)
      this
    else{
      val newMember:Rte = members
        .collect{case Singleton(mi:SMemberImpl) => mi.xs }
        .reduceOption(setOperation) match {
        case None => one
        case Some(pairs:Seq[_]) => {
          val memberOperands = pairs.collect{
            case (p1:SimpleTypeD,p2:Any) => (p1,p2)
            case pair => throw new NotImplementedError(s"invalid data: this=$this, pair=$pair")
          }
          Singleton(RandomType.createMemberFromPairs(memberOperands))
        }
      }
      val newNotMember:Rte = notMembers
        .collect{case Not(Singleton(mi:SMemberImpl)) => mi.xs }
        .reduceOption(setDualOperation) match {
        case None => one
        case Some(pairs:Seq[_]) => {
          val notMemberOperands = pairs.collect{
            case (p1:SimpleTypeD, p2:Any) => (p1,p2)
            case pair => throw new NotImplementedError(s"invalid data: this=$this, pair=$pair")
          }
          Not(Singleton(RandomType.createMemberFromPairs(notMemberOperands)))
        }
      }
      // careful to put the SMember back in the place of the first
      //   this is accomplished by replacing every SMember/SEql with the one we derive here
      //   and then use uniquify to remove duplicates without changing order.
      create(uniquify(operands.map{
        case Singleton(_:SMemberImpl) => newMember
        case Not(Singleton(_:SMemberImpl)) => newNotMember
        case r => r
      }))
    }
  }

  def conversionC16():Rte = {
    // WARNING, this function assumes there are no repeated elements
    //     according to ==
    //     If there are repeated elements, both will be removed.

    // remove And superclasses
    // remove Or subclasses

    // Must be careful, e.g. if Or(A,B) with A a subset of B and B a subset of A
    //    but A != B, then don't remove both.
    val ss = operands.collect{
      case Singleton(td) => td
    }
    val redundant = ss.toList.tails.flatMap { tail =>
      if (tail.size < 2)
        List()
      else {
        val td1 = tail.head
        tail.tail.flatMap { td2 =>
          if (annihilator(td1, td2).contains(true)) {
            List(td2)
          } else if (annihilator(td2, td1).contains(true)) {
            List(td1)
          } else
            List()
        }
      }
    }.toList
    val filtered = operands.flatMap{
      // And(super,sub,...) --> And(sub,...)
      // Or(super,sub,...) --> Or(super,...)
      case Singleton(sup) if redundant.contains(sup) => Seq()
      case r => Seq(r)
    }
    create(filtered)
  }

  def conversionD16b():Rte

  def conversionC17():Rte = {
    // And({1,2,3},Singleton(X),Not(Singleton(Y)))
    //  {...} selecting elements, x, for which SAnd(X,SNot(Y)).typep(x) is true

    // Or({1,2,3},Singleton(X),Not(Singleton(Y)))
    //  {...} deleting elements, x, for which SOr(X,SNot(Y)).typep(x) is true

    operands.collectFirst {
      case s@Singleton(_: SMemberImpl) => s
    } match {
      case Some(s@Singleton(m: SMemberImpl)) =>
        val singletonRtes = searchReplace(operands, s, Seq()).collect {
          case s:Singleton => s
          case n@Not(Singleton(_)) => n
        }
        val looser = singletonRtes.collect {
          case Singleton(td) => td
          case Not(Singleton(td)) => SNot(td)
        }
        looser match {
          case Seq() => this
          case tds =>
            val td = createTypeD(tds)
            val rte = Singleton(RandomType.createMemberFromPairs(m.xs.filter{case (_:SimpleTypeD,a:Any) => orInvert(td.typep(a))}))
            create(searchReplace(operands, s, rte))
        }
      case _ => this
    }
  }

  def conversionC21():Rte = {
    // if A and B are disjoint then
    //  And(A,B,X) --> EmptySet
    //  Or(Not(A),Not(B),X) --> sigma*
    val singletons = this match {
      case _:And => operands.collect{case Singleton(td) => td}.toList
      case _:Or => operands.collect{case Not(Singleton(td)) => td}.toList
    }
    if ( singletons.tails.exists{
      case t1::ts => ts.exists{t2 => t1.disjoint(t2).contains(true)}
      case _ => false
    })
      zero
    else
      this
  }

  def conversionC99():Rte = {
    create(operands.map(_.canonicalizeOnce))
  }

  def conversionC1():Rte = create(operands)

  override def canonicalizeOnce: Rte = {
    findSimplifier(tag="combination",target=this,verbose=false,List[(String,() => Rte)](
      "super" -> (() => { super.canonicalizeOnce })
      ))
  }

  def derivativeDown(wrt:SimpleTypeD, factors:List[SimpleTypeD], disjoints:List[SimpleTypeD]):Rte = {
    //   This optimization may avoid having to compute a costly derivative
    //   and then throw away the result.  Once we discover that rt.derivative() returns zero
    //   then we don't have to take any more derivatives.
    val derivs = operands.foldLeft(List(this.one)) {
      case (acc, rt) =>
        if (acc == List(this.zero))
          acc // if previous iteration found zero, just keep returning it
        else {
          val d = rt.derivative(Some(wrt), factors, disjoints)
          if (d == this.zero)
            List(this.zero)
          else
            d :: acc
        }
    }
    create(derivs)
  }
  override def search(test: Rte => Boolean): Option[Rte] = {
    // this magic incantation of std lib calls does the following
    //   calls the search(test) method on each element of operands
    //   until such a call returns Some(something), then
    //   halts the iteration, returning the option returned by search(test).
    //   If search fails to return Some(something), None is returned.
    operands
      .iterator
      .map(_.search(test))
      .collectFirst{case Some(x) => x}
      .orElse(super.search(test))
  }
}


