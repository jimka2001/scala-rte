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
import adjuvant.Adjuvant.{uniquify,findSimplifier,searchReplace}
import genus._

case class Or(operands:Seq[Rte]) extends Rte {
  def create(operands: Seq[Rte]):Rte = {
    Or.createOr(operands)
  }

  override def toLaTeX: String = "(" + operands.map(_.toLaTeX).mkString("\\vee ") + ")"

  override def toString: String = operands.map(_.toString).mkString("Or(", ",", ")")

  def nullable: Boolean = operands.exists {
    _.nullable
  }

  def firstTypes: Set[SimpleTypeD] = operands.toSet.flatMap((r: Rte) => r.firstTypes)
  def toSimpleTypeD:SimpleTypeD = SOr.createOr(operands.map(_.toSimpleTypeD))

  def inhabited:Option[Boolean] = {
    if (operands.exists(_.inhabited.contains(true)))
      Some(true)
    else if (operands.exists(_.inhabited.isEmpty))
      None
    else
      Some(false)
  }

  def conversion3():Rte = {
    // Or(... Sigma* ....) -> Sigma*
    if (operands.contains(Rte.sigmaStar))
      Rte.sigmaStar
    else
      this
  }

  def conversion4():Rte = {
    create(uniquify(operands))
  }

  def conversion5():Rte = {
    create(Rte.sortAlphabetically(operands))
  }

  def conversion6():Rte = {
    // remove EmptySet and flatten Or(Or(...)...)
    create(operands.flatMap{
      case EmptySet => Seq()
      case Or(Seq(rs @ _*)) => rs
      case r => Seq(r)
    })
  }

  def conversion7():Rte = {
    // (:or A B (:* B) C)
    // --> (:or A (:* B) C)
    val stars = operands.collect{
      case rt@Star(_) => rt
    }
    if (stars.isEmpty)
      this
    else
      create(operands.flatMap{
        case r1 if stars.contains(Star(r1)) => Seq()
        case r => Seq(r)
    })
  }

  def conversion8(existsNullable : => Boolean):Rte = {
    val maybePlus = operands.find(Rte.isPlus)

    // (:or A :epsilon B (:cat X (:* X)) C)
    //   --> (:or A :epsilon B (:* X) C )
    // (:or :epsilon (:cat X (:* X)))
    //   --> (:or :epsilon (:* X))
    // (:or (:* Y) (:cat X (:* X)))
    //   --> (:or (:* Y) (:* X))

    if (existsNullable && maybePlus.nonEmpty)
      create(operands.map {
        case Cat(Seq(x, z@Star(y))) if x == y => z
        case Cat(Seq(z@Star(x), y)) if x == y => z
        case rt => rt
      })
    else
      this
  }

  def conversion9(existsNullable: =>Boolean):Rte = {
    lazy val maybeCatxyz = operands.find(Rte.catxyzp) // (:cat X Y Z (:* (:cat X Y Z)))

    // (:or A :epsilon B (:cat X Y Z (:* (:cat X Y Z))) C)
    //   --> (:or A :epsilon B (:* (:cat X Y Z)) C )
    // (:or :epsilon (:cat X Y Z (:* (:cat X Y Z))))
    //   --> (:or :epsilon (:* (:cat X Y Z)))
    if (existsNullable && maybeCatxyz.nonEmpty)
      create(operands.map{
        case c@Cat(Seq(rs@_*)) if maybeCatxyz.contains(c) => rs.last
        case rt => rt
      })
    else
      this
  }

  def conversion10():Rte = {
    // (:or A :epsilon B (:* X) C)
    //   --> (:or A B (:* X) C)
    if (operands.contains(EmptyWord) && operands.exists(r => r != EmptyWord && r.nullable))
      create(operands.filterNot(_ == EmptyWord))
    else
      this
  }

  def conversion11b():Rte = {
    // if Sigma is in the operands, then filter out all singletons
    // Or(Singleton(A),Sigma,...) -> Or(Sigma,...)
    if (operands.contains(Sigma))
      create(operands.flatMap{
        case Singleton(_) => Seq()
        case td => Seq(td)
      })
    else
      this
  }

  def conversion11():Rte = {
    // filter out singletons which are a subclass of other singletons
    lazy val singletons: Seq[SimpleTypeD] = operands.collect {
      case Singleton(td) => td
    }

    val maybeSub = singletons.find { sub =>
      singletons.exists { sup =>
        sub != sup && sub.subtypep(sup).contains(true)
      }
    }
    maybeSub match {
      case None => this
      case Some(td) => create(operands.filterNot(_ == Singleton(td)))
    }
  }

  def conversion12():Rte = {
    // TODO, this is a curious conversion.  I think it should
    //    be generalized, but not sure how.

    // sigmaSigmaStarSigma = Cat(Sigma, Sigma, sigmaStar)
    // Or(   A, B, ... Cat(Sigma,Sigma,Sigma*) ... Not(Singleton(X)) ...)
    //   --> Or( A, B, ... Not(Singleton(X))
    // This is correct because Cat(Σ,Σ,(Σ)*) is the set of all sequences of length 2 or more
    //    and Not(Singleton(X)) includes the set of all sequences of length 2 or more
    if (operands.contains(Rte.sigmaSigmaStarSigma) && operands.exists{
      case Not(Singleton(_)) => true
      case _ => false})
      create(operands.filter(_!= Rte.sigmaSigmaStarSigma))
    else
      this
  }

  def conversion13():Rte = {
    // Or(A,Not(A),X) -> SigmaStar
    operands.collectFirst{
      case Not(rt) if operands.contains(rt) => rt
    } match {
      case None => this
      case Some(_) => Rte.sigmaStar
    }
  }

  def conversion14():Rte = {
    // Or(A,Not(B),X) -> Sigma if B is subtype of A

    val subs = operands.collect{
      case Not(Singleton(sub)) => sub
    }
    lazy val sups = operands.collect{
      case Singleton(sup) => sup
    }

    subs.find{sub => sups.exists(sup => sub.subtypep(sup).contains(true))} match {
      case None => this
      case Some(_) => Rte.sigmaStar
    }
  }

  def conversion15():Rte = {
    // Or(Not(A),B*,C) = Or(Not(A),C) if A and B  disjoint,
    //   i.e. remove all B* where b is disjoint from A

    val as = operands.collect{
      case Not(Singleton(td)) => td
    }
    val bs: Seq[Star] = operands.collect{
      // set of b's which are disjoint with at least one a in as
      case s@Star(Singleton(b)) if as.exists(a => a.disjoint(b).contains(true)) => s
    }

    bs match {
      case Seq() => this
      case _ => create(operands.diff(bs))
    }
  }

  def conversion16():Rte = {
    // Or(<{1,2,3}>,<{a,b,c}>,Not(<{4,5,6}>))

    val members = operands.collect{
      case s@Singleton(_:SMemberImpl) => s
    }
    val as = members.flatMap{
      case Singleton(m:SMemberImpl) => m.xs
      case x => throw new Exception(s"unexpected value $x")
    }
    if (members.isEmpty)
      this
    else {
      // careful to put the SMember back in the place of the first
      //   this is accomplished by replacing every SMember/SEql with the one we derive here
      //   and then use uniquify to remove duplicates without changing order.
      val s = Singleton(Types.createMember(uniquify(as)))
      create(uniquify(operands.map{
        case Singleton(_:SMemberImpl) => s
        case r => r
      }))
    }
  }

  def conversion17():Rte = {
    // Or(<{1,2,3}>,Not(<{3,4,5,6}>))
    // remove redundant element from SMember within Or
    // because of conversion16 we know there is at most one SMember/SEql
    // Note that we would like to form a dfa of the Rte by removing
    //     <{1,2,3}> and then testing membership of Seq(1), Seq(2), and Seq(3)
    //     of that dfa.   However, creating a dfa means we must find all the derivatives
    //     and canonicalize them.   This forms an infinite loop.    If someone
    //     can figure out how to avoid this infinite loop, we could make this
    //     conversion17 more effective.
    //     As an approximation, rather than checking membership of the rte,
    //     we instead find all the Singleton() and Not(Singleton(...))
    //     and just form a SimpleTypeD and check that.
    val member = operands.collectFirst {
      case s@Singleton(_: SMemberImpl) => s
    }
    member match {
      case None => this
      case Some(s@Singleton(m: SMemberImpl)) =>
        val looser = create(searchReplace(operands,s,Seq()))
        val td = looser.toSimpleTypeD
        val keep = m.xs.filter { x => ! td.typep(x)}
        val rte = Singleton(Types.createMember(keep))
        create(searchReplace(operands, s, rte))
      case _ => throw new Exception("scala compiler is not smart enough to know this line is unreachable")
    }
  }

  def conversion99():Rte =
    create(operands.map(_.canonicalizeOnce))

  def conversion1():Rte = create(operands)

  override def canonicalizeOnce: Rte = {
    lazy val existsNullable = operands.exists(_.nullable)
    findSimplifier(this,List[() => Rte](
      () => { conversion1() },
      () => { conversion3() },
      () => { conversion4() },
      () => { conversion6() },
      () => { conversion7() },
      () => { conversion8(existsNullable)},
      () => { conversion9(existsNullable)},
      () => { conversion10()},
      () => { conversion11b()},
      () => { conversion11()},
      () => { conversion12()},
      () => { conversion13()},
      () => { conversion14()},
      () => { conversion15()},
      () => { conversion16()},
      () => { conversion17()},
      () => { conversion99() },
      () => { conversion5() },
      () => { super.canonicalizeOnce }
      ))
  }

  def derivativeDown(wrt:SimpleTypeD):Rte = create(operands.map(rt => rt.derivative(Some(wrt))))
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
