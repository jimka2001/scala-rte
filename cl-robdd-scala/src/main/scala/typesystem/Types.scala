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

import java.lang

import scala.math.abs
import scala.math.sqrt
import scala.runtime.BoxedUnit

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

  /** Returns whether this type is a recognizable supertype of another given type.
    * It is a superset test. This might be undecidable.
    *
    * @param t the type we want to check the inclusion in this type
    * @return an optional Boolean which is true if this type is a supertype of t
    */
  def supertypep(t: Type): Option[Boolean] = t.subtypep(this)
}


/** Trait representing types that have to be stored in the HashMap in the
  * LBdd representation.
  */
sealed trait TerminalType


/** The empty type, subtype of all types. */
object EmptyType extends Type {
  override def typep(a: Any): Boolean = false
  override def inhabited:Some[Boolean] = Some(false)
  override protected def disjointDown(t: Type): Option[Boolean] = Some(true)
  override def subtypep(t: Type): Option[Boolean] = Some(true)
}


/** The super type, super type of all types. */
object TopType extends Type {
  override def typep(a: Any): Boolean = true
  override def inhabited:Some[Boolean] = Some(true)

  override protected def disjointDown(t: Type): Option[Boolean] = {
    t match {
      case EmptyType => Some(true)
      case _ => Some(false)
    }
  }

  override def subtypep(t: Type): Option[Boolean] = {
    if (t == TopType) Some(true)
    else Some(false)
  }
}


/** The atoms of our type system: a simple type built from a native Scala/Java type.
  *
  * @param ct the class of a Scala or Java type this class will wrap (call it with `classOf[native_type]`)
  */
case class AtomicType(ct: Class[_]) extends Type with TerminalType {
  override def typep(a: Any): Boolean = {
    ct.isInstance(a)
  }
  override def inhabited:Some[Boolean] = {
    if (ct.isAssignableFrom(classOf[Nothing]))
      Some(false)
    else
      Some(true)
  }
  override protected def disjointDown(t: Type): Option[Boolean] = {
    import java.lang.reflect.Modifier
    def isFinal(cl:Class[_]):Boolean = {
      Modifier.isFinal(cl.getModifiers())
    }
    def isInterface(cl:Class[_]):Boolean = {
      Modifier.isInterface(cl.getModifiers())
    }

    t match {
      case EmptyType => Some(true)
      case TopType => Some(false)
      case AtomicType(tp) =>
        if (tp == ct)
           Some(false)
        else if (ct.isAssignableFrom(tp) || tp.isAssignableFrom(ct))
          Some(false)
        else if (isFinal(ct) || isFinal(tp)) // if either is final
          Some(true)
        else if (isInterface(ct) || isInterface(tp) ) // if either is an interface
          Some(false)
        else // neither is final, and neither is an interface, and neither is a subclass of the other, so disjoint.
          Some(true)
      case _ => super.disjointDown(t)
    }
  }

  override def subtypep(s: Type): Option[Boolean] = {
    s match {
      // super.isAssignableFrom(sub) means sub is subtype of super
      case AtomicType(tp) =>
        Some(tp.isAssignableFrom(ct))

      case MemberType(_ @ _*) =>
        Some(false) // no member type exhausts all the values of an Atomic Type

      case EqlType(_) =>
        Some(false)

      case NotType(_) =>
        super.subtypep(s)

      case UnionType(tp @ _*) =>
        if (tp.exists(x => subtypep(x).contains(true)))
          // A < A union X,
          // and A < B => A < B union X
          // if this happens to be a subtype of one of the components of the union type, then it is
          //   a subtype of the union type, but if this fails to be a subtype of every component
          //   we can not reach any conclusion
          Some(true)
        else if (tp.forall(x => disjoint(x).contains(true)))
          Some(false)
        else
          super.subtypep(s)

      case IntersectionType(tp @ _*) =>
        if(tp.forall(x => subtypep(x).contains(true)))
          Some(true)
        else if (tp.forall(x => disjoint(x).contains(true)))
          Some(false)
        else
          super.subtypep(s)

      case CustomType(_) =>
        super.subtypep(s)
    }
  }
}


/** The AtomicType object, implementing an apply method in order to
  * deal with EmptyType and TopType construction.
  */
object AtomicType {
  def apply(ct: Class[_]): Type = {
    if (ct == classOf[Nothing]) EmptyType
    else if (ct == classOf[Any]) TopType
    else new AtomicType(ct)
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
  override def inhabited:Option[Boolean] = {
    // TODO : Only a single pass ?
    if (U.exists(_.inhabited.contains(true)))
      Some(true)
    else if (U.forall(_.inhabited.contains(false)))
      Some(false)
    else
      super.inhabited
  }
  // UnionType(U: Type*)
  override protected def disjointDown(t: Type): Option[Boolean] = {
    // TODO : Only a single pass ?
    if (U.forall(_.disjoint(t).contains(true)))
      Some(true)
    else if (U.exists(_.disjoint(t).contains(false)))
      Some(false)
    else
      super.disjointDown(t)
  }

  // UnionType(U: Type*)
  override def subtypep(t: Type): Option[Boolean] = {
    // TODO : Only a single pass ?
    if (U.forall(_.subtypep(t).contains(true)))
      Some(true)
    else if (U.exists(_.subtypep(t).contains(false)))
      Some(false)
    else
      super.subtypep(t)
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
  override def inhabited: Option[Boolean] = {
    if(U.exists(false == _.inhabited.getOrElse(true))) {
      // if one of an intersection is empty, then the intersection is empty
      Some(false)
    } else {
      // we don't know anything about whether the intersection is empty
      None
    }
  }
  override protected def disjointDown(t: Type): Option[Boolean] = {
    if (U.exists(_.disjoint(t).getOrElse(false)))
      Some(true)
    else super.disjointDown(t)
  }

  override def subtypep(t: Type): Option[Boolean] = {
    if (U.exists(_.subtypep(t).getOrElse(false))) Some(true)
    else None
  }
}


/** A negation of a type.
  *
  * @param s the type we want to get the complement
  */
case class NotType(s: Type) extends Type {
  override def typep(a: Any): Boolean = {
    ! s.typep(a)
  }

  override def inhabited:Option[Boolean] = {
    val nothing = classOf[Nothing]
    val any = classOf[AnyRef]
    s match {
      case NotType(x) => x.inhabited
      case AtomicType(`nothing`) => Some(true)
      case AtomicType(`any`) => Some(false)
      case AtomicType(_) => Some(true)
      case TopType => Some(false)
      case EmptyType => Some(true)
      case MemberType(_) => Some(true)
      case EqlType(_) => Some(true)
      case _ => None
    }
  }
  override protected def disjointDown(t: Type): Option[Boolean] = {
    if (t.subtypep(s).getOrElse(false))
      Some(true)
    else super.disjointDown(t)
  }

  override def subtypep(t: Type): Option[Boolean] = {
    this.s.disjoint(t)
  }
}


/** The member type is an exhaustive type, all object composing it are
  * given at construction time.
  *
  * @param M var-arg, the members of the type
  */
case class MemberType(M: Any*) extends Type with TerminalType {
  override def typep(a: Any): Boolean = M.contains(a)
  override def inhabited:Option[Boolean] = Some(true)
  override protected def disjointDown(t: Type): Option[Boolean] = {
    if (M.exists(t.typep)) Some(false)
    else Some(true)
  }

  override def subtypep(t: Type): Option[Boolean] = {
    if (M.forall(t.typep)) Some(true)
    else Some(false)
  }
}


/** The equal type, a type which is equal to a given object.
  *
  * @param a the object defining the type
  */
case class EqlType(a: Any) extends Type with TerminalType {
  override def typep(b: Any): Boolean = {
    a == b
  }

  override def inhabited:Option[Boolean] = Some(true)

  override protected def disjointDown(t: Type): Option[Boolean] = {
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
case class CustomType(f: Any => Boolean) extends Type with TerminalType {
  override def typep(a: Any): Boolean = f(a)

  override protected def disjointDown(t: Type): Option[Boolean] = super.disjointDown(t)
  override def inhabited:Option[Boolean] = super.inhabited
  override def subtypep(t: Type): Option[Boolean] = super.subtypep(t)
}


object Types {

  val Any: Class[Any] = classOf[Any]
  val Nothing: Class[Nothing] = classOf[Nothing]
  val Int: Class[Int] = classOf[Int]
  val Integer: Class[Integer] = classOf[lang.Integer]
  val Double: Class[lang.Double] = classOf[lang.Double]
  val String: Class[String] = classOf[lang.String]
  val ListAny: Class[List[Any]] = classOf[List[Any]]
  val Boolean: Class[lang.Boolean] = classOf[lang.Boolean]
  val Unit: Class[BoxedUnit] = classOf[scala.runtime.BoxedUnit]
  val Char: Class[Character] = classOf[lang.Character]
  val AnyRef: Class[AnyRef] = classOf[AnyRef]
  val AnyVal: Class[AnyVal] = classOf[AnyVal]
  val Numeric: Class[Number] = classOf[lang.Number]

  val anyType = AtomicType(Any)
  val nothingType = AtomicType(Nothing)

  val intType = AtomicType(Int)
  val intJavaType = AtomicType(Integer)
  val doubleJavaType = AtomicType(Double)
  val stringType = AtomicType(String)
  val listAnyType = AtomicType(ListAny)
  val booleanJavaType = AtomicType(Boolean)
  val unitRuntimeType = AtomicType(Unit)
  val charJavaType = AtomicType(Char)
  val anyRefType = AtomicType(AnyRef)
  val numericType = AtomicType(Numeric)

  val atomicTypesSeq: Seq[Type] =
    Seq(intType, intJavaType, doubleJavaType, stringType, listAnyType, booleanJavaType, unitRuntimeType,
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
