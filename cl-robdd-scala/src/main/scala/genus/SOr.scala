// Copyright (c) 2020 EPITA Research and Development Laboratory
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

/** A union type, which is the union of zero or more types.
 *
 * @param tds var-arg, zero or more types
 */
case class SOr(tds: SimpleTypeD*) extends SimpleTypeD {
  override def toString:String = tds.map(_.toString).mkString("[Or ", ",", "]")

  override def typep(a: Any): Boolean = {
    tds.exists(_.typep(a))
  }

  override def inhabited: Option[Boolean] = {
    val i = memoize((s: SimpleTypeD) => s.inhabited)
    if (tds.exists(i(_).contains(true)))
      Some(true)
    else if (tds.forall(i(_).contains(false)))
      Some(false)
    else
      super.inhabited
  }

  // UnionType(tds: Type*)
  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = {
    val d = memoize((s: SimpleTypeD) => s.disjoint(t))
    if (tds.forall(d(_).contains(true)))
      Some(true)
    else if (tds.exists(d(_).contains(false)))
      Some(false)
    else
      super.disjointDown(t)
  }

  // UnionType(tds: Type*)
  override def subtypep(t: SimpleTypeD): Option[Boolean] = {
    val s = memoize((s: SimpleTypeD) => s.subtypep(t))
    if (tds.forall(s(_).contains(true)))
      Some(true)
    else if (tds.exists(s(_).contains(false)))
      Some(false)
    else
      super.subtypep(t)
  }

  // UnionType(tds: Type*)
  override def canonicalizeOnce(nf:Option[NormalForm]=None): SimpleTypeD = {
    findSimplifier(List[() => SimpleTypeD](
      () => {
        if (tds.contains(SEmpty)) {
          SOr(tds.filter {
            _ != SEmpty
          }: _*)
        }
        else if (tds.contains(STop)){
          STop
        }
        else
          this
      },
      () => {
        tds match {
          case Seq() => SEmpty
          case Seq(x) => x
          case xs => SOr(xs.distinct : _*)
        }
      },
      () => {
        // (or (member 1 2 3) (member 2 3 4 5)) --> (member 1 2 3 4 5)
        // (or String (member 1 2 "3") (member 2 3 4 "5")) --> (or String (member 1 2 4))

        val members = tds.filter(memberp)
        val eqls = tds.filter(eqlp)
        if (members.size + eqls.size <= 1)
          this
        else {
          val others = tds.filterNot(td => memberp(td) || eqlp(td))
          val stricter = SOr(others : _*)
          val content = (members ++ eqls).flatMap{
            case SMember(xs @ _*) => xs.filterNot(stricter.typep)
            case SEql(x) => Seq(x).filterNot(stricter.typep)
            case _ => Seq()
          }
          SOr(others ++ Seq(SMember(content : _*).canonicalize()) :_*)
        }
      },
      () => {
        // (or (or A B) (or C D)) --> (or A B C D)
        if (!tds.exists {
          case SOr(_*) => true
          case _ => false
        })
          this
        else
          SOr(tds.flatMap{
            case SOr(xs @ _*) => xs
            case x => Seq(x)
          } : _*)
      },
      () => {
        // (or A (not A)) --> Top
        if (tds.exists(td => tds.contains(SNot(td))))
          STop
        else
          this
      },
      () => {
        // AXBC + !X = ABC + !X
        tds.find{
          case SNot(x) => tds.exists{
            case SAnd(td2s @ _*) => td2s.contains(x)
            case _ => false
          }
          case _ => false
        } match {
          case Some(SNot(x)) => SOr(tds.map{
            case SAnd(td2s @ _*) => SAnd(td2s.filter(_ != x) : _*)
            case a => a
          } :_*)
          case _ => this
        }
      },
      () => {
        // A + A!B -> A + B
        // A + A!BX + Y = (A + BX + Y)
        // A + ABX + Y = (A + Y)

        // TODO need to look at these relations if A < B or B < A,
        //  I'm not yet sure what will happen, but I think there are some simplifications to be made
        val ao = tds.find(a =>
                            tds.exists {
                              case SAnd(td2s@_*) =>
                                td2s.exists {
                                  case SNot(b) if a == b => true
                                  case b if a == b => true
                                  case _ => false
                                }
                              case _ => false
                            })
        ao match {
          case None => this
          case Some(a) => // remove NotType(a) from every intersection, and if a is in intersection, replace with EmptyType
            SOr(
              tds.flatMap {
                case SAnd(tds@_*) if tds.contains(a) => Seq()
                case SAnd(tds@_*) if tds.contains(SNot(a)) => Seq(SAnd(tds.filterNot(_ == SNot(a)): _*))
                case b => Seq(b)
              }: _*)
        }
      },
      () => {
        // (or A (not B)) --> Top   if B is subtype of A
        tds.find{a => tds.exists{
          case SNot(b) if b.subtypep(a).contains(true) => true
          case _ => false
        }} match {
          case None => this
          case Some(_) => STop
        }
      },
      () => {
        SOr(tds.map((t: SimpleTypeD) => t.canonicalize(nf=nf)).sortWith(cmpTypeDesignators): _*).maybeDnf(nf).maybeCnf(nf)
      }
    ))
  }

  // UnionType(tds: Type*)
  override def toCnf: SimpleTypeD = {
    tds.find(andp) match {
      case Some(td@SAnd(andArgs@_*)) =>
        val others = tds.filterNot(_ == td)
        SAnd(andArgs.map { x => SOr(conj(x, others): _*) }: _*)
      case None => this
      case _ => ???
    }
  }


  // UnionType(tds: Type*)
  override def cmp(td:SimpleTypeD):Boolean = {
    if (this == td)
      false
    else td match {
      // this <= td ?
      case SOr(tds @ _*) =>
        compareSequence(this.tds,tds)
      case _ => super.cmp(td)
    }
  }
}