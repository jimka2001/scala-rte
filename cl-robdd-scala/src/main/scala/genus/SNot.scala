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

/** A negation of a type.
 *
 * @param s the type we want to get the complement
 */
case class SNot(s: SimpleTypeD) extends SimpleTypeD {
  override def toString:String = "[Not " + s.toString + "]"

  override def typep(a: Any): Boolean = {
    !s.typep(a)
  }

  override def inhabitedDown: Option[Boolean] = {
    val nothing = classOf[Nothing]
    val any = classOf[AnyRef]
    s match {
      case STop => Some(false)
      case SEmpty => Some(true)
      case SAtomic(`nothing`) => Some(true)
      case SAtomic(`any`) => Some(false)
      case SAtomic(_) => Some(true)
      case SMember(_ @ _*) => Some(true)
      case SEql(_) => Some(true)
      case SNot(x) => x.inhabited
      case _ => None
    }
  }

  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = {
    if (t.subtypep(s).getOrElse(false))
      Some(true)
    else super.disjointDown(t)
  }

  // NotType(s: Type)
  override def subtypep(t: SimpleTypeD): Option[Boolean] = {
    lazy val os = t match {
      case SNot(b) => b.subtypep(s)
      case _ => None
    }
    if (s.inhabited.contains(true)
        && s.subtypep(t).contains(true))
      Some(false)
    else if (os.nonEmpty)
      os
    else
      super.subtypep(t)

  }

  // NotType(s: Type)
  override def canonicalizeOnce(nf:Option[NormalForm]=None): SimpleTypeD = {
    s match {
      case SNot(s1) => s1.canonicalizeOnce(nf=nf)
      case STop => SEmpty
      case SEmpty => STop
      case s2: SimpleTypeD => SNot(s2.canonicalizeOnce(nf=nf)).maybeDnf(nf).maybeCnf(nf)
    }
  }

  // NotType(s: Type)
  override def computeDnf(): SimpleTypeD = {
    s match {
      case SAnd(xs @ _*) =>
        SOr(xs.map(x => SNot(x)) : _*)
      case SOr(xs @ _*) =>
        SAnd(xs.map(x => SNot(x)) : _*)
      case _ => this
    }
  }
  // NotType(s: Type)
  override def computeCnf(): SimpleTypeD = {
    toDnf
  }
  // NotType(s: Type)
  override def cmp(td:SimpleTypeD):Boolean = {
    if( this == td)
      false
    else td match {
      case SNot(td) => cmpTypeDesignators(s, td)
      case _ => super.cmp(td)
    }
  }
}
