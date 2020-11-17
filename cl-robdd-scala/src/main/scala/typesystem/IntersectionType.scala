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

/** An intersection type, which is the intersection of zero or more types.
 *
 * @param tds var-arg, zero or more types
 */
case class IntersectionType(tds: Type*) extends Type {
  override def toString = tds.map(_.toString).mkString("[And ", ",", "]")

  override def typep(a: Any): Boolean = {
    tds.forall(_.typep(a))
  }

  override def inhabited: Option[Boolean] = {
    if (tds.exists(_.inhabited.contains(false))) {
      // if one of an intersection is empty, then the intersection is empty
      Some(false)
    } else {
      // we don't know anything about whether the intersection is empty
      super.inhabited
    }
  }

  override protected def disjointDown(t: Type): Option[Boolean] = {
    if (tds.exists(_.disjoint(t).getOrElse(false)))
      Some(true)
    else super.disjointDown(t)
  }

  override def subtypep(t: Type): Option[Boolean] = {
    if (tds.exists(_.subtypep(t).contains(true)))
      Some(true)
    else
      super.subtypep(t)
  }

  // IntersectionType(U: Type*)
  override def canonicalizeOnce: Type = {
    findSimplifier(List[() => Type](
      () => {
        if (tds.contains(EmptyType))
          EmptyType
        else
          this
      },
      () => {
        IntersectionType.apply(tds.distinct: _*)
      },
      () => {
        if (tds.isEmpty)
          TopType
        else if (tds.tail.isEmpty)
          tds.head
        else
          this
      },
      () => { // IntersectionType(EqlType(42), A, B, C)
        //  ==> EqlType(42)
        //  or EmptyType
        tds.find {
          case EqlType(_) => true
          case _ => false
        } match {
          case Some(e@EqlType(x)) =>
            if (typep(x))
              e
            else
              EmptyType
          case _ => this
        }
      },
      () => { // IntersectionType(MemberType(42,43,44), A, B, C)
        //  ==> MemberType(42,44)
        tds.find {
          case MemberType(_*) => true
          case _ => false
        } match {
          case Some(MemberType(xs@_*)) =>
            MemberType(xs.filter(typep): _*)
          case _ => this
        }
      },
      () => { // IntersectionType(A,TopType,B) ==> IntersectionType(A,B)
        if (tds.contains(TopType))
          IntersectionType(tds.filterNot(_ == TopType): _*)
        else
          this
      },
      () => {
        // (and Long (not (member 1 2)) (not (member 3 4)))
        //  --> (and Long (not (member 1 2 3 4)))
        // (and Double (not (member 1.0 2.0 "a" "b"))) --> (and Double (not (member 1.0 2.0)))
        // (and Double (not (= "a"))) --> (and Double  (not (member)))

        val notMembers = tds.filter {
          case NotType(MemberType(_*)) => true
          case _ => false
        }
        val notEqls = tds.filter {
          case NotType(EqlType(_)) => true
          case _ => false
        }
        val others: Seq[Type] = tds.filter {
          case NotType(EqlType(_)) => false
          case NotType(MemberType(_*)) => false
          case _ => true
        }
        if (notEqls.isEmpty && notMembers.isEmpty)
          this
        else {
          // the intersection type omitting the (not (=...) and (not (member ...)
          val lessStrict = IntersectionType(others: _*)
          val newEqls = notEqls.map { case NotType(EqlType(x)) => x }
          val newMembers = notMembers.flatMap { case NotType(MemberType(xs@_*)) => xs }
          val newElements: Seq[Any] = (newEqls ++ newMembers).filter(x => lessStrict.typep(x))
          val newNotMember = NotType(MemberType(newElements: _*))

          IntersectionType(others ++ Seq(newNotMember): _*)
        }
      },
      () => {
        // (and A (and B C) D) --> (and A B C D)
        val ands = tds.find {
          case IntersectionType(_*) => true
          case _ => false
        }
        if (ands.isEmpty)
          this
        else {
          IntersectionType(tds.flatMap {
            case IntersectionType(xs@_*) => xs
            case x => Seq(x)
          }: _*)
        }
      },
      () => {
        IntersectionType(tds.map((t: Type) => t.canonicalize): _*)
      }
      ))

  }
}
