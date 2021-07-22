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

object Types {
  import scala.runtime.BoxedUnit
  import scala.language.implicitConversions

  // allow implicit conversions from c:Class[_] to AtomicType(c)
  //    thus allowing Types such as classOf[java.lang.Integer] && !SEql(0)
  //    classOf[A] && classOf[B]
  implicit def class2type(c:Class[_]): SimpleTypeD = SAtomic(c)
  def createMember(xs:Seq[Any]):SimpleTypeD = xs match {
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
  val combop: SimpleTypeD=>Boolean = {
    case SOr(_*) => true
    case SAnd(_*) => true
    case _ => false
  }
  val notp: SimpleTypeD=>Boolean = {
    case SNot(_) => true
    case _ => false
  }

  def mdtd(tds:Set[SimpleTypeD]):Seq[SimpleTypeD] = {
    def recur(decomposition: Seq[SimpleTypeD], tds: Set[SimpleTypeD]): Seq[SimpleTypeD] = {
      if (tds.isEmpty)
        decomposition
      else {
        val td = tds.head
        val n = SNot(td).canonicalize(Some(NormalForm.Dnf))

        def f(td1: SimpleTypeD): Seq[SimpleTypeD] = {
          lazy val a = SAnd(td, td1).canonicalize(Some(NormalForm.Dnf))
          lazy val b = SAnd(n, td1).canonicalize(Some(NormalForm.Dnf))
          if (td.disjoint(td1).contains(true))
            Seq(td1)
          else if (n.disjoint(td1).contains(true))
            Seq(td1)
          else if (a.inhabited.contains(false))
            Seq(td1)
          else if (b.inhabited.contains(false))
            Seq(td1)
          else
            Seq(a, b)
        }

        recur(decomposition.flatMap(f), tds - td)
      }
    }

    recur(Seq(STop), tds - STop)
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
  val evenType:SSatisfies = SSatisfies(isEven, "even")

  def isOdd(x: Any): Boolean = {
    x match {
      case y: Int => scala.math.abs(y % 2) == 1
      case _ => false
    }
  }
  val oddType:SSatisfies = SSatisfies(isOdd, "odd")

  def isPrime(x: Any): Boolean = {
    @scala.annotation.tailrec
    def go(k: Int, y: Int): Boolean = {
      if (k > scala.math.sqrt(y)) true
      else if (y % k == 0) false
      else go(k + 1, y)
    }
    x match {
      case y: Int => go(2, y)
      case _ => false
    }
  }
  val primeType:SSatisfies = SSatisfies(isPrime)

}
