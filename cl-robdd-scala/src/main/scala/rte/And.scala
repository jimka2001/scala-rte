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

case class And(override val operands:Seq[Rte]) extends Combination(operands) {
  override def toMachineReadable: String = operands.map(_.toMachineReadable).mkString("Or(", ",", ")")

  val zero:Rte = EmptySet
  val one:Rte = Rte.sigmaStar
  def sameCombination(c:Combination):Boolean = {
    c match {
      case _:And => true
      case _ => false
    }
  }
  def dualCombination(c:Combination):Boolean = {
    c match {
      case _:Or => true
      case _ => false
    }
  }
  override def annihilator(a:SimpleTypeD,b:SimpleTypeD):Option[Boolean] = {
    a.subtypep(b)
  }
  def create(operands: Seq[Rte]):Rte = {
    And.createAnd(operands)
  }
  def createTypeD(operands: Seq[SimpleTypeD]):SimpleTypeD = SAnd.createAnd(operands)
  def createDual(operands: Seq[Rte]):Rte = {
    Or.createOr(operands)
  }
  def orInvert(x:Boolean):Boolean = x
  def setOperation(a:Seq[Any],b:Seq[Any]):Seq[Any] = a.filter(x => b.contains(x)) // intersection
  def setDualOperation(a:Seq[Any],b:Seq[Any]):Seq[Any] = a ++ b.diff(a) // union

  override def toLaTeX:String = operands.map(_.toLaTeX).mkString("(", "\\wedge ", ")")
  override def toString:String = operands.map(_.toString).mkString("And(", ",", ")")
  def nullable:Boolean = operands.forall{_.nullable} // TODO should be lazy

  def conversion7():Rte = {
    if (operands.contains(EmptyWord) && matchesOnlySingletons)
      EmptySet
    else
      this
  }

  def conversionA8():Rte = {
    // if operands contains EmptyWord, then the intersection is either EmptyWord or EmptySet
    if (! operands.contains(EmptyWord))
      this
    else if ( operands.forall(_.nullable) )
      EmptyWord
    else
      EmptySet
  }

  def conversionA9():Rte = {
    if (matchesOnlySingletons && operands.exists(Rte.isStar))
    // if x matches only singleton then And(x,y*) -> And(x,y)
      create(operands.map {
        case Star(rt) => rt
        case rt => rt
      })
    else
      this
  }

  def conversionA10():Rte = {
    if (operands.exists(Rte.isOr))
    // And(A,B,Or(X,Y,Z),C,D)
    // --> Or(And(A,B,   X,   C, D)),
    //        And(A,B,   Y,   C, D)),
    //        And(A,B,   Z,   C, D)))
      operands.find(Rte.isOr) match {
        case Some(x@Or(Seq(rs@_*))) =>
          Or.createOr(rs.map { r => And.createAnd(searchReplace(operands, x, r)) })
      }
    else
      this
  }

  def conversionA18():Rte = {
    operands.collectFirst{
      case Singleton(td) if td.inhabited.contains(false) => td
    } match {
      case None => this
      case _ => EmptySet
    }
  }

  def conversionA13():Rte = {
    if (operands.contains(Sigma)
      && operands.exists(Rte.isSingleton)
      && singletons.exists(td => td.inhabited.contains(true)))
      create(operands.filterNot(_ == Sigma))
    else
      this
  }

  def conversionD16b():Rte = {
    val ss = operands.collect{
      case Singleton(td) => td
    }
    val filtered = operands.flatMap{
      // And(A,x,Not(y)) --> And(A, x)        if x,y disjoint
      case Not(Singleton(td)) if ss.exists(d => td.disjoint(d).contains(true)) => Seq()
      case r => Seq(r)
    }
    create(filtered)
  }

  def conversionA17():Rte = {
    // if And(...) contains a Cat(...) with at least 2 non-nullable components,
    //    this  Cat matches only sequences of length 2 or more.
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

  def conversionA17a():Rte = {
    // if And(...) has more than one Cat(...) which has no nullable operand,
    //    then the number of non-nullables must be the same, else EmptySet.

    val cats: Seq[Seq[Rte]] = operands.collect{
      case Cat(tds) if tds.forall(td => !td.nullable) => tds
    }
    if (cats.isEmpty)
      this
    else if (cats.tail.forall{tds => tds.size == cats.head.size}) {
      this
    } else
      EmptySet
  }

  def conversionA17a2():Rte = {
    //    We  replace the several Cat(...) (having no nullables)
    //    with a single Cat(...) with intersections of operands.
    //    And(Cat(a,b,c),Cat(x,y,z) ...)
    //    --> And(Cat(And(a,x),And(b,y),And(c,z),...)

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
      this
  }

  def conversionA17b():Rte = {
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

  def conversionA17c():Rte = {
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

  def conversionA19():Rte = {
    val canonicalizedSingletons: SimpleTypeD = SAnd.createAnd(singletons).canonicalize()

    if (canonicalizedSingletons == SEmpty)
      EmptySet
    else
      this
  }

  lazy val matchesOnlySingletons:Boolean = operands.contains(Sigma) || operands.exists(Rte.isSingleton)

  lazy val singletons:List[SimpleTypeD] = operands.flatMap{
    case Singleton(td) => List(td)
    case Not(Singleton(td)) if matchesOnlySingletons => List(SNot(td))
    case _ => List.empty
  }.toList

  override def canonicalizeOnce:Rte = {
    //println("canonicalizing And: " + operands)

    findSimplifier(tag="and", this, verbose=false,List[(String,() => Rte)](
      "1" -> (() => { conversionC1() }),
      "3" -> (() => { conversionC3() }),
      "4" -> (() => { conversionC4() }),
      "6" -> ( () => { conversionC6() }),
      "7" -> (() => { conversion7() }),
      "C7" -> (() => { conversionC7() }),
      "8" -> (() => { conversionA8() }),
      "9" -> (() => { conversionA9() }),
      "10" -> (() => { conversionA10() }),
      "C11" -> (() => { conversionC11() }),
      "C14" -> (() => { conversionC14() }),
      "18" -> ( () => { conversionA18() }),
      "C12" -> ( () => { conversionC12() }),
      "13" -> (() => { conversionA13() }),
      "21" -> ( () => { conversionC21() }),
      "C15" -> ( () => { conversionC15() }),
      "C16" -> ( () => { conversionC16() }),
      "C16b" -> (() => { conversionD16b() }),
      "17" -> (() => { conversionA17() }),
      "17a" -> (() => { conversionA17a() }),
      "17a2" -> (() => { conversionA17a2() }),
      "17b" -> (() => { conversionA17b() }),
      "17c" -> (() => { conversionA17c() }),
      "A19" -> (() => { conversionA19() }),
      "C17" -> (() => { conversionC17() }),
      "99" -> (() => { conversionC99() }),
      "5" -> (() => { conversionC5() }),
      "super" -> (() => { super.canonicalizeOnce})
      ))
  }
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