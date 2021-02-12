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

package genus

import java.lang

import scala.annotation.tailrec

object NormalForm extends Enumeration {
  type NormalForm = Value
  val Dnf, Cnf = Value
}
import NormalForm._

object Types {
  import scala.runtime.BoxedUnit

  // allow implicit conversions from c:Class[_] to AtomicType(c)
  //    thus allowing Types such as classOf[java.lang.Integer] && !SEql(0)
  //    classOf[A] && classOf[B]
  implicit def class2type(c:Class[_]): SimpleTypeD = SAtomic(c)
  def createMember(xs:Any*):SimpleTypeD = xs match {
    case Seq() => SEmpty
    case Seq(a) => SEql(a)
    case _ => SMember(xs :_*)
  }
  val atomicp: SimpleTypeD=>Boolean = {
    case SAtomic(_) => true
    case _ => false
  }
  val eqlp: SimpleTypeD=>Boolean = {
    case SEql(_) => true
    case _ => false
  }
  val memberp: SimpleTypeD=>Boolean = {
    case SMember(_*) => true
    case SEql(_) => true
    case _ => false
  }
  val andp: SimpleTypeD=>Boolean = {
    case SAnd(_*) => true
    case _ => false
  }
  val orp: SimpleTypeD=>Boolean = {
    case SOr(_*) => true
    case _ => false
  }
  val notp: SimpleTypeD=>Boolean = {
    case SNot(_) => true
    case _ => false
  }
  def randomType(depth:Int):SimpleTypeD = {
    import scala.util.Random
    val random = new Random
    trait Trait1
    trait Trait2
    trait Trait3 extends Trait2
    abstract class Abstract1
    abstract class Abstract2 extends Trait3
    val interestingTypes:Vector[SimpleTypeD] = Vector(
      STop,
      SEmpty,
      SMember(1, 2, 3, 4),
      SMember(4, 5, 6),
      SEql(0),
      SEql(1),
      SEql(-1),
      SMember("a", "b", "c"),
      SAtomic(classOf[lang.Number]),
      SAtomic(classOf[String]),
      SAtomic(classOf[Integer]),
      SAtomic(classOf[Trait1]),
      SAtomic(classOf[Trait2]),
      SAtomic(classOf[Trait3]),
      SAtomic(classOf[Abstract1]),
      SAtomic(classOf[Abstract2])
      )
    val maxCompoundSize = 3
    val generators:Seq[()=>SimpleTypeD] = Vector(
      () => SNot(randomType(depth - 1)),
      () => SAnd(0 until random.nextInt(maxCompoundSize) map { _ => randomType(depth - 1)} : _*),
      () => SOr(0 until random.nextInt(maxCompoundSize) map { _ => randomType(depth - 1)} : _*)
      )
    if (depth <= 0)
      interestingTypes(random.nextInt(interestingTypes.length))
    else {
      val g = generators(random.nextInt(generators.length))
      g()
    }
  }

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

  def conj[T](obj:T, seq:Seq[T]):Seq[T] = seq match {
    // given an object ane a sequence, add the element to the sequence
    // either at the beginning or end, depending on the type of Seq.
    // It is easier to add to the begging of a list, but to the end of a vector.
    // The caller calls this function when it is not important whether the
    // new element be added to the beginning or the end.
    case Seq() => Seq(obj)
    case l:List[T] => obj :: l
    case _ => seq :+ obj
  }
  
  def compareSequence(tds1:Seq[SimpleTypeD], tds2:Seq[SimpleTypeD]):Boolean = {
    @tailrec
    def comp(as:List[SimpleTypeD], bs:List[SimpleTypeD]):Boolean = {
      (as,bs) match {
        case (Nil,Nil) => throw new Exception(s"not expecting equal sequences $tds1, $tds2")
        case (Nil,_) => true
        case (_,Nil) => false
        case (a::as,b::bs) =>
          if (a == b)
            comp(as,bs)
          else
            cmpTypeDesignators(a,b)
      }
    }
    comp(tds1.toList, tds2.toList)
  }

  /* compare two objects in a way compatible with sortWith.
  This function implements a strictly-less-than, thus it
  returns false on equal elements.
  */
  def cmpTypeDesignators(a:SimpleTypeD, b:SimpleTypeD):Boolean = {
    if( a == b )
      false
    else if (a.getClass eq b.getClass) {
      a.cmpToSameClassObj(b)
    }
    else {
      // just compare the class printed values alphabetically
      a.getClass.toString < b.getClass.toString
    }
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

  val anyType:SimpleTypeD = SAtomic(Any)
  val nothingType:SimpleTypeD = SAtomic(Nothing)

  val intType:SimpleTypeD = SAtomic(Int)
  val intJavaType:SimpleTypeD = SAtomic(Integer)
  val doubleJavaType:SimpleTypeD = SAtomic(Double)
  val stringType:SimpleTypeD = SAtomic(String)
  val listAnyType:SimpleTypeD = SAtomic(ListAny)
  val booleanJavaType:SimpleTypeD = SAtomic(Boolean)
  val unitRuntimeType:SimpleTypeD = SAtomic(Unit)
  val charJavaType:SimpleTypeD = SAtomic(Char)
  val anyRefType:SimpleTypeD = SAtomic(AnyRef)
  val numericType:SimpleTypeD = SAtomic(Numeric)

  val atomicTypesSeq: Seq[SimpleTypeD] =
    Seq(intType, intJavaType, doubleJavaType, stringType, listAnyType, booleanJavaType, unitRuntimeType,
      charJavaType, anyRefType, numericType)


  def isEven(x: Any): Boolean = {
    x match {
      case y: Int => y % 2 == 0
      case _ => false
    }
  }
  val evenType:SCustom = SCustom(isEven,"even")

  def isOdd(x: Any): Boolean = {
    import scala.math.abs
    x match {
      case y: Int => abs(y % 2) == 1
      case _ => false
    }
  }
  val oddType:SCustom = SCustom(isOdd,"odd")

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
  val primeType:SCustom = SCustom(isPrime)


  def main(args: Array[String]): Unit = {
    val a = 2
    val t = SAtomic(classOf[Int])

    println("type of a = " + a.getClass)
    println("class of Int = " + classOf[Int])

    println(t.typep(a))
    class Abstract1
    class Abstract2
    trait Trait1
    trait Trait2
    trait Trait3
    val t1 = SAnd(SAtomic(classOf[Trait1]),
                  SOr(SAtomic(classOf[Abstract1]),
                      SAtomic(classOf[Abstract2])),
                  SAtomic(classOf[Trait2]))
    val t2 = SAnd(SAtomic(classOf[Trait1]),
                  SNot(SOr(SAtomic(classOf[Abstract1]),
                           SAtomic(classOf[Abstract2]))),
                  SAtomic(classOf[Trait2]))
    println(t1)
    println(t2)
    println(t2.canonicalize(nf=Some(Dnf)))
    println(t1.canonicalize())
    println(t1.canonicalize(nf=Some(Dnf)))
    println(SNot(t1).canonicalize(nf=Some(Dnf)))
    (0 to 10). foreach { i =>
      val t = randomType(6)
      println(s"$i:" + t)
      println("   " + t.canonicalize())
    }

    println(t1 || t2 )
    println(t1 && t2)
    println(t1 ^^ t2)
    println(t1 - t2)
    println(!t1)

    println(classOf[String] || classOf[Integer])
  }
}
