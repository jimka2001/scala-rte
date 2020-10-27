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

import scala.math.abs
import scala.math.sqrt


/** A general type of our type system. */
sealed abstract class Type {
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
    * @param t the type we want to check whether it is disjoint to this type
    * @return an optional Boolean which is true if t and this type are disjoint
    */
  def disjoint(t: Type): Option[Boolean] = {
    disjointDown(t) match {
      case None => t.disjointDown(this)
      case x => x
    }
  }

  def disjointDown(t: Type): Option[Boolean]

  /** Returns whether this type is a recognizable subtype of another given type.
    * It is a subset test. This might be undecidable.
    *
    * @param t the type we want to check whether this type is included in
    * @return an optional Boolean which is true if this type is a subtype of t
    */
  def subtypep(t: Type): Option[Boolean]

  /** Returns whether this type is a recognizable supertype of another given type.
    * It is a superset test. This might be undecidable.
    *
    * @param t the type we want to check the inclusion in this type
    * @return an optional Boolean which is true if this type is a supertype of t
    */
  def supertypep(t: Type): Option[Boolean] = t.subtypep(this)
}


/** The empty type, subtype of all types. */
object EmptyType extends Type {
  override def typep(a: Any): Boolean = false

  override def disjointDown(t: Type): Option[Boolean] = Some(true)

  override def subtypep(t: Type): Option[Boolean] = Some(true)
}


/** The super type, super type of all types. */
object SuperType extends Type {
  override def typep(a: Any): Boolean = true

  override def disjointDown(t: Type): Option[Boolean] = {
    t match {
      case EmptyType => Some(true)
      case _ => Some(false)
    }
  }

  override def subtypep(t: Type): Option[Boolean] = {
    if (t == SuperType) Some(true)
    else Some(false)
  }
}


/** The atoms of our type system: a simple type built from a native Scala/Java type.
  *
  * @param T the class of a Scala or Java type this class will wrap (call it with `classOf[native_type]`)
  */
case class AtomicType(T: Class[_]) extends Type {
  override def typep(a: Any): Boolean = {
    // TODO : Java Int vs Scala Int
    // TODO : Typep of a List[Something]
    println("type of a in typep = " + a.getClass)
    T.isInstance(a)
  }

  override def disjointDown(t: Type): Option[Boolean] = {
    t match {
      case EmptyType => Some(true)
      case SuperType => Some(false)
      case _ if t == this => Some(false)
      case tp: AtomicType
        if this.T.isAssignableFrom(tp.T) || tp.T.isAssignableFrom(this.T) => Some(false)
      case _ => None
    }
  }

  override def subtypep(t: Type): Option[Boolean] = {
    t match {
      case tp: AtomicType if this.T.isAssignableFrom(tp.T) => Some(true)
      // TODO : Other cases ? Interfaces ?
      case _ => None
    }
  }
}


/** The AtomicType object, implementing an apply method in order to
  * deal with EmptyType and SuperType construction.
  */
object AtomicType {
  def apply(T: Class[_]): Type = {
    if (T == classOf[Nothing]) EmptyType
    else if (T == classOf[Any]) SuperType
    else new AtomicType(T)
  }
}


/** A union type, which is the union of zero or more types.
  *
  * @param U var-arg, zero or more types
  */
case class UnionType(U: Type*) extends Type {
  override def typep(a: Any): Boolean = {
    U.exists(_.typep(a))
  }

  override def disjointDown(t: Type): Option[Boolean] = {
    if (U.forall(_.disjoint(t).nonEmpty))
      Some(U.forall(_.disjoint(t).get))
    else None
  }

  override def subtypep(t: Type): Option[Boolean] = {
    // TODO : Only a single pass ?
    if (U.forall(_.subtypep(t).getOrElse(false))) Some(true)
    else if (U.exists(! _.subtypep(t).getOrElse(true))) Some(false)
    else None
  }
}


/** An intersection type, which is the intersection of zero or more types.
  *
  * @param U var-arg, zero or more types
  */
case class IntersectionType(U: Type*) extends Type {
  override def typep(a: Any): Boolean = {
    U.forall(_.typep(a))
  }

  override def disjointDown(t: Type): Option[Boolean] = {
    if (U.exists(_.disjoint(t).getOrElse(false)))
      Some(true)
    else None
  }

  override def subtypep(t: Type): Option[Boolean] = {
    if (U.exists(_.subtypep(t).getOrElse(false))) Some(true)
    else None
  }
}


/** A negation of a type.
  *
  * @param T the type we want to get the complement
  */
case class NotType(T: Type) extends Type {
  override def typep(a: Any): Boolean = {
    ! T.typep(a)
  }

  override def disjointDown(t: Type): Option[Boolean] = {
    if (t.subtypep(T).getOrElse(false))
      Some(true)
    else None
  }

  override def subtypep(t: Type): Option[Boolean] = ???
}


/** The member type is an exhaustive type, all object composing it are
  * given at construction time.
  *
  * @param M var-arg, the members of the type
  */
case class MemberType(M: Any*) extends Type {
  override def typep(a: Any): Boolean = M.contains(a)

  override def disjointDown(t: Type): Option[Boolean] = {
    if (M.exists(t.typep)) Some(false)
    else Some(true)
  }

  override def subtypep(t: Type): Option[Boolean] = ???
}


/** The equal type, a type which is equal to a given object.
  *
  * @param a the object defining the type
  */
case class EqlType(a: Any) extends Type {
  override def typep(b: Any): Boolean = {
    a == b
  }

  override def disjointDown(t: Type): Option[Boolean] = {
    if (t.typep(a)) Some(false)
    else Some(true)
  }

  override def subtypep(t: Type): Option[Boolean] = {
    if (t == this) Some(true)
    else if (t == EmptyType) Some(true)
    else Some(false)
  }
}


/** A custom type, build by a function defined by the user.
  *
  * @param f a function taking an object Any and returning a Boolean defining the type
  */
case class CustomType(f: Any => Boolean) extends Type {
  override def typep(a: Any): Boolean = f(a)

  override def disjointDown(t: Type): Option[Boolean] = ???

  override def subtypep(t: Type): Option[Boolean] = ???
}



object TypeSystem {

  val anyType = AtomicType(classOf[Any])
  val nothingType = AtomicType(classOf[Nothing])

  val intType = AtomicType(classOf[Int])
  val intJavaType = AtomicType(classOf[java.lang.Integer])
  val doubleJavaType = AtomicType(classOf[java.lang.Double])
  val stringType = AtomicType(classOf[String])
  val listIntType = AtomicType(classOf[List[Int]])
  val booleanJavaType = AtomicType(classOf[java.lang.Boolean])
  val unitRuntimeType = AtomicType(classOf[scala.runtime.BoxedUnit])
  val charJavaType = AtomicType(classOf[java.lang.Character])
  val anyRefType = AtomicType(classOf[AnyRef])
  val numericType = AtomicType(classOf[java.lang.Number])

  val atomicTypesSeq: Seq[Type] =
    Seq(intType, intJavaType, doubleJavaType, stringType, listIntType, booleanJavaType, unitRuntimeType,
      charJavaType, anyRefType, numericType)


  def isEven(x: Any): Boolean = {
    x match {
      case y: Int => y % 2 == 0
      case _ => false
    }
  }
  val evenType = CustomType(isEven)

  def isOdd(x: Any): Boolean = {
    x match {
      case y: Int => abs(y % 2) == 1
      case _ => false
    }
  }
  val oddType = CustomType(isOdd)

  def isPrime(x: Any): Boolean = {
    @scala.annotation.tailrec
    def go(k: Int, y: Int): Boolean = {
      if (k > sqrt(y)) true
      else if (y % k == 0) false
      else go(k + 1, y)
    }
    x match {
      case y: Int => go(2, y)
      case _ => false
    }
  }
  val primeType = CustomType(isPrime)


  def main(args: Array[String]): Unit = {
    val a = 2
    val t = AtomicType(classOf[Int])

    println("type of a = " + a.getClass)
    println("class of Int = " + classOf[Int])

    println(t.typep(a))
  }
}
