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
  // The memoize method is inspired by
  //  https://clojuredocs.org/clojure.core/memoize
  // Returning a memoized version of a referentially transparent function. The
  // memoized version of the function keeps a cache of the mapping from arguments
  // to results and, when calls with the same arguments are repeated often, has
  // higher performance at the expense of higher memory use.
  def memoize[F,T](f:F=>T):F=>T = {
    val hash = scala.collection.mutable.Map[F,T]()
    def mem(i:F):T = {
      hash.getOrElse(i, locally{
        val v:T = f(i)
        hash(i) = v
        v
      })
    }
    mem
  }

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

  def fixedPoint[T](v:T, f:T=>T, goodEnough:(T,T)=>Boolean):T = {
    val v2 = f(v)
    if (goodEnough(v,v2))
      v
    else
      fixedPoint(v2,f,goodEnough)
  }

  def findSimplifier(simplifiers:List[( () => Type)]):Type = {
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

  def canonicalizeOnce:Type = this
  def canonicalize:Type = fixedPoint(this,
                                     (t:Type)=>t.canonicalizeOnce,
                                     (a:Type,b:Type) => a.getClass == b.getClass && a==b)

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
      Modifier.isFinal(cl.getModifiers)
    }
    def isInterface(cl:Class[_]):Boolean = {
      Modifier.isInterface(cl.getModifiers)
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
      case EmptyType => Some(false)
      case TopType => Some(true)
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
  // AtomicType(ct: Class[_])
  override def canonicalizeOnce:Type = {
    AtomicType(ct)
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
    val i = memoize((s:Type) => s.inhabited)
    if (U.exists(i(_).contains(true)))
      Some(true)
    else if (U.forall(i(_).contains(false)))
      Some(false)
    else
      super.inhabited
  }
  // UnionType(U: Type*)
  override protected def disjointDown(t: Type): Option[Boolean] = {
    val d = memoize((s:Type) => s.disjoint(t))
    if (U.forall(d(_).contains(true)))
      Some(true)
    else if (U.exists(d(_).contains(false)))
      Some(false)
    else
      super.disjointDown(t)
  }

  // UnionType(U: Type*)
  override def subtypep(t: Type): Option[Boolean] = {
    val s = memoize((s:Type) => s.subtypep(t))
    if (U.forall(s(_).contains(true)))
      Some(true)
    else if (U.exists(s(_).contains(false)))
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
    if (U.exists(_.inhabited.contains(false))) {
      // if one of an intersection is empty, then the intersection is empty
      Some(false)
    } else {
      // we don't know anything about whether the intersection is empty
      super.inhabited
    }
  }

  override protected def disjointDown(t: Type): Option[Boolean] = {
    if (U.exists(_.disjoint(t).getOrElse(false)))
      Some(true)
    else super.disjointDown(t)
  }

  override def subtypep(t: Type): Option[Boolean] = {
    if (U.exists(_.subtypep(t).contains(true)))
      Some(true)
    else
      super.subtypep(t)
  }

  // IntersectionType(U: Type*)
  override def canonicalizeOnce: Type = {
    findSimplifier(List[(() => Type)](
      () => {
        if (U.contains(EmptyType))
          EmptyType
        else
          this
      },
      () => {
        IntersectionType.apply(U.distinct :_*)
      },
      () => {
        if (U.isEmpty)
          TopType
        else if (U.tail.isEmpty)
          U.head
        else
          this
      },
      () => { // IntersectionType(EqlType(42), A, B, C)
        //  ==> EqlType(42)
        //  or EmptyType
        U.find {
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
        U.find {
          case MemberType(_) => true
          case _ => false
        } match {
          case Some(MemberType(xs@_*)) =>
            MemberType.apply(xs.filter(typep(_)))
          case _ => this
        }
      },
      () => { // IntersectionType(A,TopType,B) ==> IntersectionType(A,B)
        if (U.contains(TopType))
          IntersectionType((U.filterNot(_ == TopType)) : _*)
        else
          this
      },
      () => {
        // (and Long (not (member 1 2)) (not (member 3 4)))
        //  --> (and Long (not (member 1 2 3 4)))
        val notMembers = U.filter {
          case NotType(MemberType(_)) => true
          case _ => false
        }
        val notEqls = U.filter {
          case NotType(EqlType(_)) => true
          case _ => false
        }
        val others: Seq[Type] = U.filter {
          case NotType(EqlType(_)) => false
          case NotType(MemberType(_)) => false
          case _ => true
        }
        if (notEqls.isEmpty && notMembers.isEmpty)
          this
        else {
          val newEqls = notEqls.map { case NotType(EqlType(x)) => x }
          val newMembers = notMembers.flatMap { case NotType(MemberType(xs@_*)) => xs }
          val newElements: Seq[Any] = newEqls ++ newMembers
          IntersectionType((others ++ Seq(MemberType.apply(newElements))) : _*)
        }
      },
      () => {
        // (and Double (not (member 1.0 2.0 "a" "b"))) --> (and Double (not (member 1.0 2.0)))
        // (and Double (not (= "a"))) --> (and Double  (not (member)))
        val notMember = U.filter {
          case NotType(MemberType(xs@_*)) => true
          case _ => false
        }
        val notEql = U.filter {
          case NotType(EqlType(x)) => true
          case _ => false
        }
        if (notEql.isEmpty && notMember.isEmpty)
          this
        else {
          val others = U.filter {
            case NotType(MemberType(_@_*)) => false
            case NotType(EqlType(_@_*)) => false
            case _ => true
          }
          val memberValues = notMember.flatMap { case NotType(MemberType(xs@_*)) => xs }
          val eqlValues = notEql.map { case NotType(EqlType(x)) => x }
          val newMemberValues = (memberValues ++ eqlValues).filter {
            typep(_)
          }
          IntersectionType((others ++ Seq(NotType(MemberType.apply(newMemberValues)))) : _*)
        }
      },
      () => {
        // (and A (and B C) D) --> (and A B C D)
        val ands = U.find {
          case IntersectionType(_) => true
          case _ => false
        }
        if (ands.isEmpty)
          this
        else {
          IntersectionType((U.flatMap {
            case IntersectionType(xs@_*) => xs
            case x => Seq(x)
          }) : _*)
        }
      },
      () => {
        IntersectionType((U.map((t: Type) => t.canonicalize)) : _*)
    }
     ))

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

  override def subtypep(t: Type): Option[Boolean] = {
    this.s.disjoint(t)
  }

  // NotType(s: Type)
  override def canonicalizeOnce:Type = {
    s match {
      case NotType(s1) => s1.canonicalizeOnce
      case TopType => EmptyType
      case EmptyType => TopType
      case s2:Type => NotType(s2.canonicalizeOnce)
    }
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

  val anyType:Type = AtomicType(Any)
  val nothingType:Type = AtomicType(Nothing)

  val intType:Type = AtomicType(Int)
  val intJavaType:Type = AtomicType(Integer)
  val doubleJavaType:Type = AtomicType(Double)
  val stringType:Type = AtomicType(String)
  val listAnyType:Type = AtomicType(ListAny)
  val booleanJavaType:Type = AtomicType(Boolean)
  val unitRuntimeType:Type = AtomicType(Unit)
  val charJavaType:Type = AtomicType(Char)
  val anyRefType:Type = AtomicType(AnyRef)
  val numericType:Type = AtomicType(Numeric)

  val atomicTypesSeq: Seq[Type] =
    Seq(intType, intJavaType, doubleJavaType, stringType, listAnyType, booleanJavaType, unitRuntimeType,
      charJavaType, anyRefType, numericType)


  def isEven(x: Any): Boolean = {
    x match {
      case y: Int => y % 2 == 0
      case _ => false
    }
  }
  val evenType:CustomType = CustomType(isEven)

  def isOdd(x: Any): Boolean = {
    x match {
      case y: Int => abs(y % 2) == 1
      case _ => false
    }
  }
  val oddType:CustomType = CustomType(isOdd)

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
  val primeType:CustomType = CustomType(isPrime)


  def main(args: Array[String]): Unit = {
    val a = 2
    val t = AtomicType(classOf[Int])

    println("type of a = " + a.getClass)
    println("class of Int = " + classOf[Int])

    println(t.typep(a))
  }
}
