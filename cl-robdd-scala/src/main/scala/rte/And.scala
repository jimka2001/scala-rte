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

case class And(operands:Seq[Rte]) extends Rte{
  import genus.SimpleTypeD
  def create(operands: Seq[Rte]):Rte = {
    And.createAnd(operands)
  }
  override def toLaTeX:String = operands.map(_.toLaTeX).mkString("(", "\\wedge ", ")")
  override def toString:String = operands.map(_.toString).mkString("And(", ",", ")")
  def nullable:Boolean = operands.forall{_.nullable} // TODO should be lazy
  def firstTypes:Set[SimpleTypeD] = operands.toSet.flatMap((r:Rte) => r.firstTypes) // TODO should be lazy

  def conversion1():Rte = {
    // And() -> Sigma
    if (operands.isEmpty)
      Sigma
    else
      this
  }
  def conversion2():Rte = {
    if (operands.sizeIs == 1)
      operands.head
    else
      this
  }

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
    if (matchesOnlySingletons && operands.exists(Rte.isStar)) {
      // if x matches only singleton then And(x,y*) -> And(x,y)
      create(operands.map{
        case Star(rt) => rt
        case rt => rt
      })
    } else
      this
  }
  def conversion10():Rte = {
    if (operands.exists(Rte.isOr)) {
      // And(A,B,Or(X,Y,Z),C,D)
      // --> Or(And(A,B,   X,   C, C)),
      //        And(A,B,   Y,   C, C)),
      //        And(A,B,   Z,   C, C)))
      val distrib = operands.find(Rte.isOr) match {
        case Some(x@Or(Seq(rs @ _*))) =>
          Or.createOr(rs.map{r => And.createAnd(searchReplace(operands,x,r))})

      }
      distrib
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
  def conversion17():Rte = {
    // if And(...) contains a Cat(...) with at least 2 non-nullable components,
    //    this the Cat matches only sequences of length 2 or more.
    // If And(...) contains a singleton, then it matches only sequences
    //    of length 1, perhaps an empty set of such sequences if the singleton type
    //    is empty.
    // If both are true, then the And() matches EmptySet
    lazy val ss = operands.collect{
      case Singleton(td) => td
    }
    lazy val cs:Seq[Cat] = operands.collect{
      case c:Cat => c
    }
    if ((operands.contains(Sigma) || ss.nonEmpty)
      // TODO, we don't really need to count higher than 2
      && cs.exists(c => c.operands.count(! _.nullable) > 1 )
    )
      EmptySet
    else
      this
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
    if (canonicalizedSingletons == genus.SEmpty)
      EmptySet
    else
      this
  }
  def conversion20():Rte = {
    // e.g. And( Cat(Sigma,Star(Sigma)), Sigma,...) --> And(Sigma,...)
    if (operands.contains(Rte.sigmaSigmaStar) && matchesOnlySingletons)
      create(searchReplace(operands,Rte.sigmaSigmaStar,Seq()))
    else
      this
  }
  def conversion21():Rte = {
    val members = operands.collectFirst{
      case Singleton( m:genus.SMemberImpl) => m
    }
    members match {
      case None => this
      case Some(m) =>
        val as:Seq[Any] = m.xs
        val dfa = this.toDfa(true)
        val filtered = as.filter{a => dfa.simulate(Seq(a)).contains(true)}
        Singleton(genus.Types.createMember(filtered : _*))
    }
  }
  def conversion99():Rte = {
    create(operands.map(_.canonicalizeOnce))
  }
  lazy val matchesOnlySingletons:Boolean = operands.contains(Sigma) || operands.exists(Rte.isSingleton)

  lazy val singletons:List[genus.SimpleTypeD] = operands.flatMap{
    case Singleton(td) => List(td)
    case Not(Singleton(td)) if matchesOnlySingletons => List(genus.SNot(td))
    case _ => List.empty
  }.toList
  //lazy val singletonIntersection = genus.SAnd.createAnd(singletons)
  lazy val canonicalizedSingletons = genus.SAnd.createAnd(singletons).canonicalize()

  override def canonicalizeOnce:Rte = {
    //println("canonicalizing And: " + operands)

    findSimplifier(this,List[() => Rte](
      () => { conversion1() },
      () => { conversion2() },
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
      () => { conversion18() },
      () => { conversion19() },
      () => { conversion20() },
      () => { conversion21() },
      () => { conversion99() },
      ))
  }
  def derivativeDown(wrt:genus.SimpleTypeD):Rte = And.createAnd(operands.map(rt => rt.derivative(Some(wrt))))
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
    print(And(Singleton(genus.SEql(0)),Sigma).canonicalize)
  }
}