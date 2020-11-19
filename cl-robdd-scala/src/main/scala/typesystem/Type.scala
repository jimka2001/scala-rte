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


/** Trait representing types that have to be stored in the HashMap in the
 * LBdd representation.
 */
trait TerminalType

/** A general type of our type system. */
abstract class Type {

  def ||(t:Type):Type = UnionType(this,t).canonicalize(dnf=true)
  def &&(t:Type):Type = IntersectionType(this,t).canonicalize(dnf=true)
  def unary_! : Type = NotType(this).canonicalize(dnf=true)
  def -(t:Type):Type = IntersectionType(this,NotType(t)).canonicalize(dnf=true)
  def ^^(t:Type):Type = UnionType(IntersectionType(this,NotType(t)),
                                  IntersectionType(NotType(this),t)).canonicalize(dnf=true)

  /** Returns whether a given object belongs to this type.
   * It is a set membership test.
   *
   * @param a the object we want to check the type
   * @return a Boolean which is true is a is of this type
   */
  def typep(a: Any): Boolean

  /** Returns whether a given type and this type are disjoint.
   * This might be undecidable. The disjointDown method is used to avoid
   * infinite loops.
   *
   * @param td the type we want to check whether it is disjoint to this type
   * @return an optional Boolean which is true if t and this type are disjoint
   */
  def disjoint(td: Type): Option[Boolean] = {
    val d1 = disjointDown(td)
    lazy val d2 = td.disjointDown(this)
    lazy val c1 = this.canonicalize()
    lazy val c2 = td.canonicalize()
    lazy val dc12 = c1.disjointDown(c2)
    lazy val dc21 = c2.disjointDown(c1)

    if (d1.nonEmpty)
      d1
    else if (d2.nonEmpty)
      d2
    else if (c1 == c2)
      None
    else if (dc12.nonEmpty)
      dc12
    else {
      dc21
      // TODO we can also check whether c1.subtypep(c2) and c1.inhabited
      //    careful not to cause an infinite loop
    }
  }

  def inhabited: Option[Boolean] = None

  protected def disjointDown(t: Type): Option[Boolean] = {
    if (inhabited.contains(false)) // an empty type is disjoint to every other type
      Some(true)
    else
      None
  }

  /** Returns whether this type is a recognizable subtype of another given type.
   * It is a subset test. This might be undecidable.
   *
   * @param t the type we want to check whether this type is included in
   * @return an optional Boolean which is true if this type is a subtype of t
   */
  def subtypep(t: Type): Option[Boolean] = {
    if ( (t.getClass eq this.getClass)
         && (t == this))
      Some(true)
    else
      (inhabited,t.inhabited) match {
        case (None,_) => None
        case (_,None) => None
        case (Some(false),Some(false)) => Some(true) // empty set is a subset of the empty set
        case (_,Some(false)) => Some(false) // no inhabited type is a subtype of the empty type
        case _ => None
      }
  }

  def fixedPoint[T](v:T, f:T=>T, goodEnough:(T,T)=>Boolean):T = {
    def fixed(v:T,history:List[T]):T = {
      val v2 = f(v)
      if (goodEnough(v, v2))
        v
      else if (history.contains(v2)) {
        history.zipWithIndex.foreach{case (td,i) => println(s"$i: $td")}
        throw new Exception("Failed: fixedPoint encountered the same value twice: " + v2)
      }
      else
        fixed(v2, v::history)
    }
    fixed(v,List[T]())
  }

  def findSimplifier(simplifiers:List[() => Type]):Type = {
    simplifiers match {
      case Nil => this
      case s::ss =>
        val t2 = s()
        if ( this == t2)
          findSimplifier(ss)
        else
          t2
    }
  }

  def toDnf:Type = this
  def maybeDnf(dnf:Boolean=false):Type = {
    if (dnf)
      toDnf
    else
      this
  }
  def canonicalizeOnce(dnf:Boolean=false):Type = this
  def canonicalize(dnf:Boolean=false):Type = {
    fixedPoint(this,
               (t:Type)=>t.canonicalizeOnce(dnf=dnf),
               (a:Type,b:Type) => a.getClass == b.getClass && a==b)
  }

  /** Returns whether this type is a recognizable supertype of another given type.
   * It is a superset test. This might be undecidable.
   *
   * @param t the type we want to check the inclusion in this type
   * @return an optional Boolean which is true if this type is a supertype of t
   */
  def supertypep(t: Type): Option[Boolean] = t.subtypep(this)

  def cmp(t:Type):Boolean = {
    throw new Exception(s"cannot cmpare type designators ${this.getClass} vs ${t.getClass}")
  }
}
