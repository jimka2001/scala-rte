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

/** An intersection type, which is the intersection of zero or more types.
 *
 * @param tds var-arg, zero or more types
 */
case class SAnd(override val tds: SimpleTypeD*) extends SCombination { // SAnd  SNot
  override def toString: String = tds.map(_.toString).mkString("[And ", ",", "]")

  override def create(tds:SimpleTypeD*):SimpleTypeD = SAnd(tds: _*)
  override val unit:SimpleTypeD = STop
  override val zero:SimpleTypeD = SEmpty
  override def annihilator(a:SimpleTypeD,b:SimpleTypeD):Option[Boolean] = {
    b.supertypep(a)
  }
  override def sameCombination(td:SimpleTypeD):Boolean = {
    td match {
      case SAnd(_@_*) => true
      case _ => false
    }
  }
  override def typep(a: Any): Boolean = {
    tds.forall(_.typep(a))
  }

  // SAnd(tds: Type*)
  override def inhabitedDown: Option[Boolean] = {
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

  // SAnd(tds: Type*)
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

  // SAnd(tds: Type*)
  override def subtypep(t: SimpleTypeD): Option[Boolean] = {
    if (tds.isEmpty)
      STop.subtypep(t)
    else if (tds.exists(_.subtypep(t).contains(true)))
      Some(true)
    else if (t.inhabited.contains(true)
             && inhabited.contains(true)
             && tds.forall(_.disjoint(t).contains(true))) {
      Some(false)
    } else
      super.subtypep(t)
  }

  // SAnd(tds: SimpleTypeD*)
  override def canonicalizeOnce(nf: Option[NormalForm] = None): SimpleTypeD = {
    findSimplifier(List[() => SimpleTypeD](
      () => { // SAnd(SMember(42,43,44), A, B, C)
        //  ==> SMember(42,44)
        tds.find(memberp) match {
          case Some(SMember(xs@_*)) =>
            createMember(xs.filter(typep): _*)
          case Some(SEql(a)) =>
            createMember(Seq(a).filter(typep): _*)
          case _ => this
        }
      },
      () => {
        // (and Long (not (member 1 2)) (not (member 3 4)))
        //  --> (and Long (not (member 1 2 3 4)))
        // (and Double (not (member 1.0 2.0 "a" "b"))) --> (and Double (not (member 1.0 2.0)))

        val notMembers = tds.filter {
          case SNot(SMember(_*)) => true
          case SNot(SEql(_)) => true
          case _ => false
        }
        val others: Seq[SimpleTypeD] = tds.filter {
          case SNot(SEql(_)) => false
          case SNot(SMember(_*)) => false
          case _ => true
        }
        if (notMembers.isEmpty)
          this
        else {
          // the intersection type omitting the (not (member ...)
          val lessStrict = SAnd(others: _*)
          val newMembers = notMembers.flatMap {
            case SNot(SMember(xs@_*)) => xs
            case SNot(e@SEql(_)) => e.xs
          }
          val newElements: Seq[Any] = newMembers.filter(x => lessStrict.typep(x))
          val newNotMember = SNot(SMember(newElements: _*).canonicalize())

          SAnd((others ++ Seq(newNotMember)).sortWith(cmpTypeDesignators): _*)
        }
      },
      () => {
        // TODO this checks n^2 times, need to change to n^2 / 2
        if (tds.exists(a => tds.exists(b => a.disjoint(b).contains(true))))
          SEmpty
        else
          this
      },
      () => {
        // (and A B C) --> (and A C) if  A is subtype of B
        tds.find(sub => tds.exists { sup =>
          ((sub != sup)
           && sub.subtypep(sup).contains(true))
        }) match {
          case None => this
          case Some(sub) =>
            // throw away all proper superclasses of sub, i.e., keep everything that is not a superclass
            // of sub and also keep sub itself.   keep false and dont-know
            val keep = tds.filter(sup => sup == sub || !sub.subtypep(sup).contains(true))
            SAnd(keep: _*)
        }
      },
      () => { super.canonicalizeOnce(nf)}
      ))
  }

  // SAnd(tds: Type*)
  override def computeDnf(): SimpleTypeD = {
    tds.find(orp) match {
      case Some(td@SOr(orArgs@_*)) =>
        val others = tds.filterNot(_ == td)
        SOr(orArgs.map { x => SAnd(conj(x, others): _*) }: _*)
      case None => this
      case _ => ???
    }
  }
}
