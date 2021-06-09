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

import adjuvant.Adjuvant._
import scala.annotation.tailrec
import genus._

case class And(operands:Seq[Rte]) extends Rte{
  def create(operands: Seq[Rte]):Rte = {
    And.createAnd(operands)
  }
  override def toLaTeX:String = operands.map(_.toLaTeX).mkString("(", "\\wedge ", ")")
  override def toString:String = operands.map(_.toString).mkString("And(", ",", ")")
  def nullable:Boolean = operands.forall{_.nullable} // TODO should be lazy
  def firstTypes:Set[SimpleTypeD] = operands.toSet.flatMap((r:Rte) => r.firstTypes) // TODO should be lazy

  def conversion3():Rte = {
    // And(... EmptySet ....) -> EmptySet
    if (operands.contains(EmptySet))
      EmptySet
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
    // remove Sigma* and flatten And(And(...)...)
    create(operands.flatMap{
      case Rte.sigmaStar => Seq()
      case And(Seq(rs @ _*)) => rs
      case r => Seq(r)
    })
  }

  def conversion7():Rte = {
    if (operands.contains(EmptyWord) && matchesOnlySingletons)
      EmptySet
    else
      this
  }

  def conversion8():Rte = {
    // if operands contains EmptyWord, then the intersection is either EmptyWord or EmptySet
    if (! operands.contains(EmptyWord))
      this
    else if ( operands.forall(_.nullable) )
      EmptyWord
    else
      EmptySet
  }

  def conversion9():Rte = {
    if (matchesOnlySingletons && operands.exists(Rte.isStar))
    // if x matches only singleton then And(x,y*) -> And(x,y)
      create(operands.map {
        case Star(rt) => rt
        case rt => rt
      })
    else
      this
  }

  def conversion10():Rte = {
    if (operands.exists(Rte.isOr))
    // And(A,B,Or(X,Y,Z),C,D)
    // --> Or(And(A,B,   X,   C, C)),
    //        And(A,B,   Y,   C, C)),
    //        And(A,B,   Z,   C, C)))
      operands.find(Rte.isOr) match {
        case Some(x@Or(Seq(rs@_*))) =>
          Or.createOr(rs.map { r => And.createAnd(searchReplace(operands, x, r)) })
      }
    else
      this
  }
  def conversion11():Rte = {
    // And(...,x,Not(x)...)
    if (operands.exists(r1 => operands.contains(Not(r1))))
      EmptySet
    else
      this
  }
  def conversion12():Rte = {
    if (singletons.exists(td => td.inhabited.contains(false)))
      EmptySet
    else
      this
  }
  def conversion13():Rte = {
    if (operands.contains(Sigma)
      && operands.exists(Rte.isSingleton)
      && singletons.exists(td => td.inhabited.contains(true)))
      create(operands.filterNot(_ == Sigma))
    else
      this
  }
  def conversion14():Rte = {
    // TODO this test should be removed.
    assert(! operands.contains(Rte.sigmaStar))
    this
  }
  def conversion15():Rte = {
    if ( singletons.tails.exists{
      case t1::ts => ts.exists{t2 => t1.disjoint(t2).contains(true)}
      case _ => false
    })
      EmptySet
    else
      this
  }
  def conversion16():Rte = {
    // remove superclasses
    //  and remove Not(disjoint) if

    val ss = operands.collect{
      case Singleton(td) => td
    }
    val filtered = operands.map{
      // And(super,sub) --> And(Sigma,sub)
      case Singleton(sup) if ss.exists(sub => sub != sup && sup.supertypep(sub).contains(true)) => Sigma
      // And(x,Not(y)) --> And(x,Sigma) if x,y disjoint
      case Not(Singleton(td)) if ss.exists(d => td.disjoint(d).contains(true)) => Sigma
      case r => r
    }
    create(filtered)
  }
  def conversion17a():Rte = {
    // if And(...) has more than one Cat(...) which has no nullable operand,
    //    then the number of non-nullables must be the same, else EmptySet.
    //    We also replace the several Cat(...) (having no nullables)
    //    with a single Cat(...) with intersections of operands.
    //    And(Cat(a,b,c),Cat(x,y,z) ...)
    //    --> And(Cat(And(a,x),And(b,y),And(c,z),...)

    // TODO this could be extended to intersect the leading non-nullable
    //   operands of Cat(...)s which contain a nullable.
    //   E.g., And(Cat(a,b*),Cat(x,y*))
    //     ->  And(Cat(And(a,x),b*),
    //             Cat(And(a,x),y*))
    //   But I'm not sure how to avoid infinite loop.
    val cats: Seq[Seq[Rte]] = operands.collect{
      case Cat(tds) if tds.forall(td => !td.nullable) => tds
    }
    if (cats.isEmpty)
      this
    else if (cats.tail.forall{tds => tds.size == cats.head.size}) {
      @tailrec
      def invert[A](sss:Seq[Seq[A]],acc:List[Seq[A]]):List[Seq[A]] = {
        if (sss.head.isEmpty)
          acc.reverse
        else
          invert(sss.map{_.tail},sss.map{_.head}::acc)
      }
      val catOperands = invert(cats,List()).map{create}
      val cat = Cat.createCat(catOperands)
      create(uniquify(operands.map{
        case Cat(tds) if cats.contains(tds) => cat
        case rte => rte
      }))
    } else
      EmptySet
  }
  def conversion17b():Rte = {
    // after 17a we know that if there are multiple Cats(...) without a nullable,
    //   then all such Cats(...) without a nullable have same number of operands.
    //   So assure that all other Cats have no more non-nullable operands.
    val cats = operands.collect {
      case c: Cat => c
    }
    val numNonNullable = cats.collectFirst {
      //case c:Cat if c.operands.forall(td => !td.nullable) => c.operands.size
      case Cat(tds) if tds.forall(td => !td.nullable) => tds.size
    }
    val nums = cats.iterator.collect {
      case Cat(tds) => tds.count(!_.nullable)
    }
    numNonNullable match {
      case None => this
      case Some(m) if nums.forall(_ <= m) => this
      case _ => EmptySet
    }
  }
  def conversion17():Rte = {
    // if And(...) contains a Cat(...) with at least 2 non-nullable components,
    //    this the Cat matches only sequences of length 2 or more.
    // If And(...) contains a singleton, then it matches only sequences
    //    of length 1, perhaps an empty set of such sequences if the singleton type
    //    is empty.
    // If both are true, then the And() matches EmptySet
    lazy val tds = operands.collect{
      case Singleton(td) => td
    }
    lazy val cats:Seq[Cat] = operands.collect{
      case c:Cat => c
    }

    if ((operands.contains(Sigma) || tds.nonEmpty)
      // do we have more than 1 non-nullable argument of some Cat(...)
      && cats.collectFirst {
      case Cat(tds) if tds.iterator.filter(!_.nullable).take(2).size > 1 => true
    }.nonEmpty)
      EmptySet
    else
      this
  }
  def conversion17c():Rte = {
    // if And(...) contains a Cat with no nullables, (or explicit Sigma or Singleton)
    //  then remove the nullables from ever other Cat with that many non-nullables,

    // find a Cat(...) with no nullables, there should be at most one because
    //    conversion17a as run.
    operands.collectFirst {
      case Cat(tds) if tds.forall(!_.nullable) => tds.size
      case Singleton(_) => 1
      case Sigma => 1
    } match {
      case None => this
      case Some(m) =>
        // find Cat(...) with at least m non-nullables, and remove nullables
        create(operands.map {
          case Cat(tds) if tds.count(!_.nullable) >= m =>
            Cat.createCat(tds.filterNot(_.nullable))
          case rte => rte
        })
    }
  }
  def conversion18():Rte = {
    operands.collectFirst{
      case Singleton(td) if td.inhabited.contains(false) => td
    } match {
      case None => this
      case _ => EmptySet
    }
  }
  def conversion19():Rte = {
    val canonicalizedSingletons: SimpleTypeD = SAnd.createAnd(singletons).canonicalize()

    if (canonicalizedSingletons == SEmpty)
      EmptySet
    else
      this
  }

  def conversion21():Rte = {
    // And({1,2,3},Singleton(X),Singleton(Y))
    //  {...} selecting elements, x, for which SAnd(X,Y).typep(x) is true
    import Types.createMember
    operands.collectFirst {
      case Singleton(m: SMemberImpl) => m.xs
    } match {
      case None => this
      case Some(xs) =>
        val lessStrict: SimpleTypeD = SAnd.createAnd(operands.flatMap {
          case Singleton(td) => Seq(td)
          case Not(Singleton(td)) => Seq(SNot(td))
          case _ => Seq()
        })
        val newMember: Rte = Singleton(createMember(xs.filter(lessStrict.typep)))
        create(uniquify(operands.map {
          case _: Singleton => newMember
          case Not(_: Singleton) => newMember
          case r => r
        }))
    }
  }
  def conversion99():Rte = {
    create(operands.map(_.canonicalizeOnce))
  }
  def conversion1():Rte = create(operands)

  lazy val matchesOnlySingletons:Boolean = operands.contains(Sigma) || operands.exists(Rte.isSingleton)

  lazy val singletons:List[SimpleTypeD] = operands.flatMap{
    case Singleton(td) => List(td)
    case Not(Singleton(td)) if matchesOnlySingletons => List(SNot(td))
    case _ => List.empty
  }.toList

  override def canonicalizeOnce:Rte = {
    //println("canonicalizing And: " + operands)

    findSimplifier(this,List[() => Rte](
      () => { conversion1() },
      () => { conversion3() },
      () => { conversion4() },
      () => { conversion5() },
      () => { conversion6() },
      () => { conversion7() },
      () => { conversion8() },
      () => { conversion9() },
      () => { conversion10() },
      () => { conversion11() },
      () => { conversion12() },
      () => { conversion13() },
      () => { conversion14() },
      () => { conversion15() },
      () => { conversion16() },
      () => { conversion17() },
      () => { conversion17a() },
      () => { conversion17b() },
      () => { conversion17c() },
      () => { conversion18() },
      () => { conversion19() },
      () => { conversion21() },
      () => { conversion99() },
      ))
  }
  def derivativeDown(wrt:SimpleTypeD):Rte = And.createAnd(operands.map(rt => rt.derivative(Some(wrt))))
}

object And {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new And(operands)

  def createAnd(operands: Seq[Rte]): Rte = {
    operands match {
      case Seq() => Rte.sigmaStar
      case Seq(rt) => rt
      case _ => And(operands)
    }
  }
}

object AndSanity {
  def main(argv:Array[String]):Unit = {
    var c1:Rte = And(Singleton(STop),Not(Singleton(SMember(4,5,6))))

    for{ i <- 0 to 10}{
      c1 = c1.canonicalizeOnce
      println(s"$i: $c1")
    }
  }
}