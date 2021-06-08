// Copyright (c) 2020,21 EPITA Research and Development Laboratory
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

package genus

import Types._
import NormalForm._
import adjuvant.Adjuvant.{conj, uniquify}

/** An intersection type, which is the intersection of zero or more types.
 *
 * @param tds var-arg, zero or more types
 */
case class SAnd(override val tds: SimpleTypeD*) extends SCombination { // SAnd  SNot
  override def toString: String = tds.map(_.toString).mkString("[SAnd ", ",", "]")

  override def create(tds:Seq[SimpleTypeD]):SimpleTypeD = SAnd.createAnd(tds)
  override def createDual(tds:Seq[SimpleTypeD]):SimpleTypeD = SOr.createOr(tds)
  override val unit:SimpleTypeD = STop
  override val zero:SimpleTypeD = SEmpty
  override def annihilator(a:SimpleTypeD,b:SimpleTypeD):Option[Boolean] = {
    b.supertypep(a)
  }
  override def sameCombination(td:SimpleTypeD):Boolean = andp(td)
  override def dualCombination(td:SimpleTypeD):Boolean = orp(td)
  def combinator[A](a:Seq[A],b:Seq[A]):Seq[A] = {
    a.intersect(b)
  }
  def dualCombinator[A](a:Seq[A],b:Seq[A]):Seq[A] = {
    uniquify(a ++ b)
  }
  def filter[A](seq:Seq[A],f:A=>Boolean):Seq[A] = seq.filter(f)

  override def typep(a: Any): Boolean = {
    tds.forall(_.typep(a))
  }

  // SAnd(tds: SimpleTypeD*)
  override protected def inhabitedDown: Option[Boolean] = {
    lazy val dnf = canonicalize(nf = Some(Dnf))
    lazy val cnf = canonicalize(nf = Some(Cnf))
    lazy val inhabitedDnf = dnf.inhabited
    lazy val inhabitedCnf = cnf.inhabited

    if (tds.exists(_.inhabited.contains(false))) {
      // if one of an intersection is empty, then the intersection is empty
      Some(false)
    } else if (tds.forall(atomicp)) {
      // if we have all atomic types (e.g. traits and abstract)
      //  and none are disjoint with another, then we assume
      //  the intersection is inhabited.
      //  however, if some pair is disjoint, the the intersection is
      //  empty, thus not inhabited
      Some(tds.toSet.subsets(2).map(_.toList).forall {
        case List(t1: SimpleTypeD, t2: SimpleTypeD) =>
          t1.disjoint(t2).contains(false)
      })
    } else if (dnf != this && inhabitedDnf.nonEmpty) {
      inhabitedDnf
    } else if (cnf != this && inhabitedCnf.nonEmpty) {
      inhabitedCnf
    } else {
      // we don't know anything about whether the intersection is empty
      super.inhabitedDown
    }
  }

  // SAnd(tds: SimpleTypeD*)
  override protected def disjointDown(t2: SimpleTypeD): Option[Boolean] = {
    lazy val inhabited_t2 = t2.inhabited.contains(true)
    lazy val inhabited_this = this.inhabited.contains(true)

    // (disjoint? (and A B C) D)   where B disjoint with D
    if (tds.exists(_.disjoint(t2).getOrElse(false)))
      Some(true)

    // (disjoint? (and A B C) B)
    else if (tds.contains(t2)
             && inhabited_t2
             && inhabited_this)
      Some(false)

    // (disjoint? (and B C) A)
    // (disjoint? (and String (not (member a b c 1 2 3))) java.lang.Comparable)
    else if (inhabited_t2
             && inhabited_this
             && tds.exists(t1 =>
                             t1.subtypep(t2).contains(true)
                             || t2.subtypep(t1).contains(true)))
      Some(false)

    else
      super.disjointDown(t2)
  }

  // SAnd(tds: SimpleTypeD*)
  override protected def subtypepDown(t: SimpleTypeD): Option[Boolean] = {
    if (tds.isEmpty)
      STop.subtypep(t)
    else if (tds.exists(_.subtypep(t).contains(true)))
      Some(true)
    else if (t.inhabited.contains(true)
             && inhabited.contains(true)
             && tds.forall(_.disjoint(t).contains(true))) {
      Some(false)
    } else
      super.subtypepDown(t)
  }
  def conversionA1():SimpleTypeD = {

    // SAnd(SMember(42,43,44), A, B, C)
    //  ==> SMember(42,44)
    tds.find(memberp) match {
      case Some(SMember(xs@_*)) =>
        createMember(xs.filter(typep))
      case Some(SEql(a)) =>
        createMember(Seq(a).filter(typep))
      case _ => this
    }
  }

  def conversionA3():SimpleTypeD = {
    // discover a disjoint pair
    if (tds.tails.exists(ts => ts.nonEmpty && ts.tail.exists(b => b.disjoint(ts.head).contains(true))))
      SEmpty
    else
      this
  }
  // SAnd(tds: SimpleTypeD*)
  override def canonicalizeOnce(nf: Option[NormalForm] = None): SimpleTypeD = {
    findSimplifier(List[() => SimpleTypeD](
      () => { conversionA1() },
      () => { conversionA3() },
      () => { super.canonicalizeOnce(nf)}
      ))
  }

  // SAnd(tds: SimpleTypeD*)
  override def computeDnf(): SimpleTypeD = {
    // convert SAnd( x1, x2, SOr(y1,y2,y3), x3, x4)
    //    --> td = SOr(y1,y2,y3)
    //    --> orArgs = (y1,y2,y3)
    //    --> others = (x1, x2, x3, x4)
    // --> SOr(SAnd(x1,x2,x3,x4,  y1),
    //         SAnd(x1,x2,x3,x4,  y2),
    //         SAnd(x1,x2,x3,x4,  y3),
    //     )
    tds.find(orp) match {
      case Some(td@SOr(orArgs@_*)) =>
        val others = tds.filterNot(_ == td)
        SOr.createOr(orArgs.map { x => SAnd.createAnd(conj(others, x)) })
      case None => this
      case x => throw new Error(s"this should not occur: " + x)
    }
  }
}

object SAnd {
  def createAnd(tds: Seq[SimpleTypeD]): SimpleTypeD = {
    tds match {
      case Seq() => STop
      case Seq(td) => td
      case _ => SAnd(tds: _*)
    }
  }
}