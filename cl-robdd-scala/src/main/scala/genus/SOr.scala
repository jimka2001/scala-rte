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
import adjuvant.Adjuvant.{findSimplifier, uniquify}

/** A union type, which is the union of zero or more types.
 *
 * @param tds var-arg, zero or more types
 */
case class SOr(override val tds: SimpleTypeD*) extends SCombination {
  override def toString:String = tds.map(_.toString).mkString("[SOr ", ",", "]")

  override def create(tds:Seq[SimpleTypeD]):SimpleTypeD = SOr.createOr(tds)
  override def createDual(tds:Seq[SimpleTypeD]):SimpleTypeD = SAnd.createAnd(tds)
  override val unit:SimpleTypeD = SEmpty
  override val zero:SimpleTypeD = STop
  override def annihilator(a:SimpleTypeD,b:SimpleTypeD):Option[Boolean] = {
    b.subtypep(a)
  }
  override def sameCombination(td:SimpleTypeD):Boolean = orp(td)
  override def dualCombination(td:SimpleTypeD):Boolean = andp(td)
  def combinator[A](a:Seq[A],b:Seq[A]):Seq[A] = {
    uniquify(a ++ b)
  }
  def dualCombinator[A](a:Seq[A],b:Seq[A]):Seq[A] = {
    a.intersect(b)
  }
  def comboFilter[A](seq:Seq[A], f:A=>Boolean):Seq[A] = seq.filterNot(f)

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

  def conversionD1():SimpleTypeD = {
    // dual of SAnd:conversionD1

    // SOr(SNot(SMember(42,43,44,"a","b")), String)
    //  ==> SNot(SMember(42,43,44))
    tds.collectFirst{case SNot(m:SMemberImpl) => m} match {
      case Some(m:SMemberImpl) =>
        SNot(createMember(m.xs.filterNot(typep)))
      case _ => this
    }
  }

  def conversionD3():SimpleTypeD = {
    // discover a disjoint pair embedded in SNot
    // I.e., find SNot(x), SNot(y) where x and y are disjoint
    //   if found, return STop
    val nots = tds.collect{case SNot(td) => td}

    if (nots.tails.exists(ts => ts.nonEmpty && nots.tail.exists(b => b.disjoint(ts.head).contains(true))))
      STop
    else
      this
  }

  // SOr(tds: SimpleTypeD*)
  override def canonicalizeOnce(nf:Option[NormalForm]=None): SimpleTypeD = {
    findSimplifier(tag="SOr",this,verbose=false,List[(String,() => SimpleTypeD)](
      ("super",   () => { super.canonicalizeOnce(nf)})
      ))
  }

  // SOr(tds: SimpleTypeD*)
  override def computeCnf(): SimpleTypeD = {
    // convert SOr( x1, x2, SAnd(y1,y2,y3), x3, x4)
    //    --> td = SAnd(y1,y2,y3)
    //    --> andArgs = (y1,y2,y3)
    //    --> others = (x1, x2, x3, x4)
    // --> SAnd(SOr(x1,x2, y1, x3,x4),
    //          SOr(x1,x2, y2, x3,x4),
    //          SOr(x1,x2, y3, x3,x4)
    //     )
    computeNf()
  }
}

object SOr {
  def createOr(tds:Seq[SimpleTypeD]):SimpleTypeD = {
    tds match {
      case Seq() => SEmpty
      case Seq(td) => td
      case _ => SOr(tds: _*)
    }
  }
}