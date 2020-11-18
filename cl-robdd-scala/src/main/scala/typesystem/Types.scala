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

object Types {
  import scala.runtime.BoxedUnit

  implicit def class2type(c:Class[_]): Type = AtomicType(c)

  val atomicp: Type=>Boolean = {
    case AtomicType(_) => true
    case _ => false
  }
  val eqlp: Type=>Boolean = {
    case EqlType(_) => true
    case _ => false
  }
  val memberp: Type=>Boolean = {
    case MemberType(_*) => true
    case _ => false
  }
  val andp: Type=>Boolean = {
    case IntersectionType(_*) => true
    case _ => false
  }
  val orp: Type=>Boolean = {
    case UnionType(_*) => true
    case _ => false
  }

  def randomType(depth:Int):Type = {
    import scala.util.Random
    val random = new Random
    trait Trait1
    trait Trait2
    trait Trait3 extends Trait2
    abstract class Abstract1
    abstract class Abstract2 extends Trait3
    val interestingTypes:Vector[Type] = Vector(
      TopType,
      EmptyType,
      MemberType(1,2,3,4),
      MemberType(4,5,6),
      EqlType(0),
      EqlType(1),
      EqlType(-1),
      MemberType("a","b","c"),
      AtomicType(classOf[lang.Number]),
      AtomicType(classOf[String]),
      AtomicType(classOf[Integer]),
      AtomicType(classOf[Trait1]),
      AtomicType(classOf[Trait2]),
      AtomicType(classOf[Trait3]),
      AtomicType(classOf[Abstract1]),
      AtomicType(classOf[Abstract2])
    )
    val maxCompoundSize = 3
    val generators:Seq[()=>Type] = Vector(
      () => NotType(randomType(depth - 1)),
      () => IntersectionType(0 until random.nextInt(maxCompoundSize) map {_ => randomType(depth-1)} : _*),
      () => UnionType(0 until random.nextInt(maxCompoundSize) map {_ => randomType(depth-1)} : _*)
    )
    if (depth <= 0)
      interestingTypes(random.nextInt(interestingTypes.length))
    else {
      val g = generators(random.nextInt(generators.length))
      g()
    }
  }

  def conj[T](obj:T, seq:Seq[T]):Seq[T] = seq match {
    case Seq() => Seq(obj)
    case l @ _::_ => obj :: l
    case _ => seq :+ obj
  }

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
    import scala.math.abs
    x match {
      case y: Int => abs(y % 2) == 1
      case _ => false
    }
  }
  val oddType:CustomType = CustomType(isOdd)

  def isPrime(x: Any): Boolean = {
    @scala.annotation.tailrec
    def go(k: Int, y: Int): Boolean = {
      import scala.math.sqrt

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
    class Abstract1
    class Abstract2
    trait Trait1
    trait Trait2
    trait Trait3
    val t1 = IntersectionType(AtomicType(classOf[Trait1]),
                              UnionType(AtomicType(classOf[Abstract1]),
                                        AtomicType(classOf[Abstract2])),
                              AtomicType(classOf[Trait2]))
    val t2 = IntersectionType(AtomicType(classOf[Trait1]),
                              NotType(UnionType(AtomicType(classOf[Abstract1]),
                                                AtomicType(classOf[Abstract2]))),
                              AtomicType(classOf[Trait2]))
    println(t1)
    println(t2)
    println(t2.canonicalize(dnf=true))
    println(t1.canonicalize())
    println(t1.canonicalize(dnf=true))
    println(NotType(t1).canonicalize(dnf=true))
    (0 to 10). foreach { i =>
      val t = randomType(6)
      println(s"$i:" + t)
      println("   " + t.canonicalize())
    }

    println(t1 || t2)
    println(t1 && t2)
    println(!t1)

    println(classOf[String] || classOf[Integer])
  }
}
