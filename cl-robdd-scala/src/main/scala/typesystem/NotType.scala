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

package typesystem
import Types._
import NormalForm._

/** A negation of a type.
 *
 * @param s the type we want to get the complement
 */
case class NotType(s: Type) extends Type {
  override def toString = "[Not " + s.toString + "]"

  override def typep(a: Any): Boolean = {
    !s.typep(a)
  }

  override def inhabited: Option[Boolean] = {
    val nothing = classOf[Nothing]
    val any = classOf[AnyRef]
    s match {
      case TopType => Some(false)
      case EmptyType => Some(true)
      case AtomicType(`nothing`) => Some(true)
      case AtomicType(`any`) => Some(false)
      case AtomicType(_) => Some(true)
      case MemberType(_) => Some(true)
      case EqlType(_) => Some(true)
      case NotType(x) => x.inhabited
      case _ => None
    }
  }

  override protected def disjointDown(t: Type): Option[Boolean] = {
    if (t.subtypep(s).getOrElse(false))
      Some(true)
    else super.disjointDown(t)
  }

  // NotType(s: Type)
  override def subtypep(t: Type): Option[Boolean] = {
    lazy val os = t match {
      case NotType(b) => b.subtypep(s)
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
  override def canonicalizeOnce(nf:Option[NormalForm]=None): Type = {
    s match {
      case NotType(s1) => s1.canonicalizeOnce(nf=nf)
      case TopType => EmptyType
      case EmptyType => TopType
      case s2: Type => NotType(s2.canonicalizeOnce(nf=nf)).maybeDnf(nf).maybeCnf(nf)
    }
  }

  // NotType(s: Type)
  override def toDnf: Type = {
    s match {
      case IntersectionType(xs @ _*) =>
        UnionType(xs.map(x => NotType(x)) : _*)
      case UnionType(xs @ _*) =>
        IntersectionType(xs.map(x => NotType(x)) : _*)
      case _ => this
    }
  }
  // NotType(s: Type)
  override def toCnf: Type = {
    toDnf
  }
  // NotType(s: Type)
  override def cmp(td:Type) = {
    if( this == td)
      false
    else td match {
      case NotType(td) => cmpTypeDesignators(s,td)
      case _ => super.cmp(td)
    }
  }
}
