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

/** A union type, which is the union of zero or more types.
 *
 * @param tds var-arg, zero or more types
 */
case class UnionType(tds: Type*) extends Type {
  override def toString:String = tds.map(_.toString).mkString("[Or ", ",", "]")

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

  // UnionType(tds: Type*)
  override protected def disjointDown(t: Type): Option[Boolean] = {
    val d = memoize((s: Type) => s.disjoint(t))
    if (tds.forall(d(_).contains(true)))
      Some(true)
    else if (tds.exists(d(_).contains(false)))
      Some(false)
    else
      super.disjointDown(t)
  }

  // UnionType(tds: Type*)
  override def subtypep(t: Type): Option[Boolean] = {
    val s = memoize((s: Type) => s.subtypep(t))
    if (tds.forall(s(_).contains(true)))
      Some(true)
    else if (tds.exists(s(_).contains(false)))
      Some(false)
    else
      super.subtypep(t)
  }

  // UnionType(tds: Type*)
  override def canonicalizeOnce(dnf:Boolean=false): Type = {
    findSimplifier(List[() => Type](
      () => {
        if (tds.contains(EmptyType)) {
          UnionType(tds.filter {
            _ != EmptyType
          }: _*)
        }
        else if (tds.contains(TopType)){
          TopType
        }
        else
          this
      },
      () => {
        tds match {
          case Seq() => EmptyType
          case Seq(x) => x
          case xs => UnionType(xs.distinct : _*)
        }
      },
      () => {
        // (or (member 1 2 3) (member 2 3 4 5)) --> (member 1 2 3 4 5)
        // (or String (member 1 2 "3") (member 2 3 4 "5")) --> (or String (member 1 2 4))
        def memberp(t:Type):Boolean = t match {
          case MemberType( _*) => true
          case _ => false
        }
        val members = tds.filter(memberp)
        if (members.size <= 1)
          this
        else {
          val others = tds.filterNot(memberp)
          val stricter = UnionType(others : _*)
          val content = members.flatMap{
            case MemberType(xs @ _*) => xs.filterNot(stricter.typep)
            case _ => Seq()
          }
          UnionType(others ++ Seq(MemberType(content : _*)) :_*)
        }
      },
      () => {
        // (or (or A B) (or C D)) --> (or A B C D)
        if (!tds.exists {
          case UnionType(_*) => true
          case _ => false
        })
          this
        else
          UnionType(tds.flatMap{
            case UnionType(xs @ _*) => xs
            case x => Seq(x)
          } : _*)
      },
      () => {
        UnionType(tds.map((t: Type) => t.canonicalize(dnf=dnf)): _*)
      }
    ))
  }
}
