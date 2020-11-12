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

import lbdd._

import scala.util.DynamicVariable


object TypeSystemWithLBdd {

  type TYPE_HASH = scala.collection.mutable.Map[AtomicType, Int]
  val maybeHash = new DynamicVariable[Option[TYPE_HASH]](None)
  var numAllocations = new DynamicVariable[Int](0)

  def newHash(): TYPE_HASH = {
    import org.jboss.util.collection._

    import scala.collection.JavaConverters._
    new WeakValueHashMap[AtomicType, Int].asScala
  }

  def withNewTypeHash[A](code: => A): A = {
    numAllocations.withValue(0) {
      maybeHash.withValue(Some(newHash())) {
        code
      }
    }
  }


  /** Creates a LBdd representing the atomic type at given as argument.
    * Unfortunately, this method has side effects. The integers (used as labels)
    * representing the AtomicTypes are stored in a HashMap.
    *
    * @param at the atomic type from which we want to create a LBdd
    * @return the created LBdd
    */
  protected def typeAsLBdd(at: AtomicType): LBdd = {
    maybeHash.value match {
      case None => sys.error("Called outside dynamic extent of withNewTypeHash(...)")
      case Some(hash) =>
        hash.get(at) match {
          case Some(k) => LBdd(k)
          case None =>
            numAllocations.value += 1
            hash(at) = numAllocations.value
            LBdd(numAllocations.value)
        }
    }
  }


  /** Creates a LBdd representing the type t given as argument.
    *
    * @param t the type from which we want to create a LBdd
    * @return the created LBdd
    */
  def typeAsLBdd(t: Type): LBdd = {
    t match {
      case x if x == EmptyType => LBddFalse
      case x if x == SuperType => LBddTrue
      case x: AtomicType => typeAsLBdd(x)
      case x: UnionType => Or(x.U.map(typeAsLBdd).toList)
      case x: IntersectionType => And(x.U.map(typeAsLBdd).toList)
      case x: NotType => Not(typeAsLBdd(x.T))
        // TODO : Exhaustive pattern matching
      case x: MemberType => ???
      case x: EqlType => ???
      case x: CustomType => ???
      case _ => ???
    }
  }


  /** Returns whether b1 is disjoint from b2.
    * This might be undecidable.
    *
    * @param b1 LBdd representing the first type.
    * @param b2 LBdd representing the second type.
    * @return an optional Boolean which is true if b1 and b2 are disjoint.
    */
  def disjoint(b1: LBdd, b2: LBdd): Option[Boolean] = {
    if (And(b1, b2) == LBddFalse || And(b2, b1) == LBddFalse) Some(true)
    else None
  }


  /** Returns whether b is inhabited (non-empty).
    * This might be undecidable.
    *
    * @param b LBdd representing the type.
    * @return an optional Boolean which is true if b is not empty.
    */
  def inhabited(b: LBdd): Option[Boolean] = {
    b match {
      case LBddFalse => Some(false)
      case LBddTrue => Some(true)
      case _ => None
    }
  }


  /** Returns whether b1 is a recognizable subtype of b2.
    * It is a subtype test. This might be undecidable.
    *
    * @param b1 LBdd representing the first type
    * @param b2 LBdd representing the second type
    * @return an optional Boolean which is true is b1 is a subtype of b2
    */
  def subtypep(b1: LBdd, b2: LBdd): Option[Boolean] = {
    if (And(b1, b2) == b2) Some(true)
    else if (And(b2, b1) == b2) Some(true)
    else if (disjoint(b1, b2).getOrElse(false)) Some(false)
    else None
  }
}
