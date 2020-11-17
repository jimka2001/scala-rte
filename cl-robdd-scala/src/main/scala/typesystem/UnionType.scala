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

/** A union type, which is the union of zero or more types.
 *
 * @param tds var-arg, zero or more types
 */
case class UnionType(tds: Type*) extends Type {
  override def toString = tds.map(_.toString).mkString("[Or ", ",", "]")

  override def typep(a: Any): Boolean = {
    tds.exists(_.typep(a))
  }

  override def inhabited: Option[Boolean] = {
    val i = memoize((s: Type) => s.inhabited)
    if (tds.exists(i(_).contains(true)))
      Some(true)
    else if (tds.forall(i(_).contains(false)))
      Some(false)
    else
      super.inhabited
  }

  // UnionType(U: Type*)
  override protected def disjointDown(t: Type): Option[Boolean] = {
    val d = memoize((s: Type) => s.disjoint(t))
    if (tds.forall(d(_).contains(true)))
      Some(true)
    else if (tds.exists(d(_).contains(false)))
      Some(false)
    else
      super.disjointDown(t)
  }

  // UnionType(U: Type*)
  override def subtypep(t: Type): Option[Boolean] = {
    val s = memoize((s: Type) => s.subtypep(t))
    if (tds.forall(s(_).contains(true)))
      Some(true)
    else if (tds.exists(s(_).contains(false)))
      Some(false)
    else
      super.subtypep(t)
  }
}
