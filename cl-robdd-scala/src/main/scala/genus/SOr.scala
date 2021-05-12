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
import adjuvant.Adjuvant.conj

/** A union type, which is the union of zero or more types.
 *
 * @param tds var-arg, zero or more types
 */
case class SOr(override val tds: SimpleTypeD*) extends SCombination {
  override def toString:String = tds.map(_.toString).mkString("[Or ", ",", "]")

  override def create(tds:SimpleTypeD*):SimpleTypeD = SOr(tds: _*)
  override val unit:SimpleTypeD = SEmpty
  override val zero:SimpleTypeD = STop
  override def annihilator(a:SimpleTypeD,b:SimpleTypeD):Option[Boolean] = {
    b.subtypep(a)
  }
  override def sameCombination(td:SimpleTypeD):Boolean = {
    td match {
      case SOr(_@_*) => true
      case _ => false
    }
  }
  override def typep(a: Any): Boolean = {
    tds.exists(_.typep(a))
  }

  override protected def inhabitedDown: Option[Boolean] = {
    import adjuvant.Adjuvant._

    val i = memoize((s: SimpleTypeD) => s.inhabited)
    if (tds.exists(i(_).contains(true)))
      Some(true)
    else if (tds.forall(i(_).contains(false)))
      Some(false)
    else
      super.inhabitedDown
  }

  // SOr(tds: SimpleTypeD*)
  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = {
    import adjuvant.Adjuvant._

    val d = memoize((s: SimpleTypeD) => s.disjoint(t))
    if (tds.forall(d(_).contains(true)))
      Some(true)
    else if (tds.exists(d(_).contains(false)))
      Some(false)
    else
      super.disjointDown(t)
  }

  // SOr(tds: SimpleTypeD*)
  override protected def subtypepDown(t: SimpleTypeD): Option[Boolean] = {
    import adjuvant.Adjuvant._

    val s = memoize((s: SimpleTypeD) => s.subtypep(t))
    if (tds.isEmpty) {
      SEmpty.subtypep(t)
    } else if (t.canonicalize() == STop)
      Some(true)
    else if (tds.forall(s(_).contains(true)))
      Some(true)
    else if (tds.exists(s(_).contains(false))) {
      Some(false)
    } else
      super.subtypepDown(t)
  }

  // SOr(tds: SimpleTypeD*)
  override def canonicalizeOnce(nf:Option[NormalForm]=None): SimpleTypeD = {
    findSimplifier(List[() => SimpleTypeD](
      () => {
        // (or (member 1 2 3) (member 2 3 4 5)) --> (member 1 2 3 4 5)
        // (or String (member 1 2 "3") (member 2 3 4 "5")) --> (or String (member 1 2 4))

        val members = tds.filter(memberp)
        if (members.size <= 1)
          this
        else {
          val others = tds.filterNot(memberp)
          val stricter = SOr(others : _*)
          val content = members.flatMap{
            case m:SMemberImpl => m.xs.filterNot(stricter.typep)
            case _ => Seq()
          }
          SOr(others ++ Seq(createMember(content : _*)) :_*)
        }
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
          case Some(a) => // remove SNot(a) from every intersection, and if a is in intersection, replace with SEmpty
            SOr(
              tds.flatMap {
                case SAnd(tds@_*) if tds.contains(a) => Seq()
                case SAnd(tds@_*) if tds.contains(SNot(a)) => Seq(SAnd(tds.filterNot(_ == SNot(a)): _*))
                case b => Seq(b)
              }: _*)
        }
      },
      () => { super.canonicalizeOnce(nf)}
      ))
  }

  // SOr(tds: SimpleTypeD*)
  override def computeCnf(): SimpleTypeD = {
    // convert SOr( x1, x2, SAnd(y1,y2,y3), x3, x4)
    //    --> td = SAnd(y1,y2,y3)
    //    --> andArgs = (y1,y2,y3)
    //    --> others = (x1, x2, x3, x4)
    // --> SAnd(SOr(x1,x2,x3,x4,  y1),
    //          SOr(x1,x2,x3,x4,  y2),
    //          SOr(x1,x2,x3,x4,  y3),
    //     )
    tds.find(andp) match {
      case Some(td@SAnd(andArgs@_*)) =>
        val others = tds.filterNot(_ == td)
        SAnd(andArgs.map { x => SOr(conj(others,x): _*) }: _*)
      case None => this
      case x => throw new Error(s"this should not occur: " + x)
    }
  }
}
