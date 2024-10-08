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
  // implicit def class2type(c: Class[_]): SimpleTypeD = SAtomic(c)

  def createMemberFromPairs(xs: Seq[(SimpleTypeD, Any)]): SimpleTypeD = {
    createMember(xs.map(_._2))
  }

  def createMember(xs: Seq[Any]): SimpleTypeD = {
    xs.toList match {
      case (_, _) :: _ => throw new Exception(s"warning createMember called with pairs: $xs")
      case _ => ()
    }

    def cmp(a: (SimpleTypeD, Any), b: (SimpleTypeD, Any)): Boolean = {
      if (a == b)
        false
      else if (a._1 != b._1)
        cmpTypeDesignators(a._1, b._1)
      else
        a._2.toString < b._2.toString
    }

    xs.map(x => (SAtomic(x.getClass), x)).distinct.sortWith(cmp) match {
      case Seq() => SEmpty
      case Seq(a) => SEql(a)
      case vec => new SMember(vec.toVector)
    }
  }

  val atomicp: SimpleTypeD => Boolean = {
    case SAtomic(_) => true
    case _ => false
  }
  val eqlp: SimpleTypeD => Boolean = {
    case SEql(_) => true
    case _ => false
  }
  val memberp: SimpleTypeD => Boolean = {
    case SMember(_) => true
    case SEql(_) => true
    case _ => false
  }
  val andp: SimpleTypeD => Boolean = {
    case SAnd(_*) => true
    case _ => false
  }
  val orp: SimpleTypeD => Boolean = {
    case SOr(_*) => true
    case _ => false
  }
  val combop: SimpleTypeD => Boolean = {
    case SOr(_*) => true
    case SAnd(_*) => true
    case _ => false
  }
  val notp: SimpleTypeD => Boolean = {
    case SNot(_) => true
    case _ => false
  }

  // Given a set of type designators, return a newly computed Map which denotes a Seq of triples:
  //    (td:SimpleTypeD,factors:Set[SimpleTypeD],disjoints:Set[SimpleTypeD])
  //    The return value is a Map whose keys are the values of td:SimpleTypeD
  //         and the value of a key is a 2-tuple of (factors,disjoints)
  // indicating type designators which implement the Maximal Disjoint Type Decomposition.
  // I.e., the computed list (the list of all the td components) designates a set, all of whose
  // elements are mutually disjoint.
  // Two values of td, i.e., x and y, have the property that if x and y
  // are disjoint, and if z in in the given set, tds, of type designators
  // then either x is disjoint from z or x is a subclass of z, but never does
  // x partially overlap z.  Consequently, if some of the given type designators
  // in tds are partially overlapping, then some td in the return value is a subtype
  // of multiple type designators from tds.
  def mdtd(tds: Set[SimpleTypeD]): Map[SimpleTypeD, (Set[SimpleTypeD], Set[SimpleTypeD])] = {
    type S = SimpleTypeD // local type def just to simplify the following type declarations

    @tailrec
    def recur(decomposition: Map[S, (Set[S], Set[S])], tds: Set[S]): Map[S, (Set[S], Set[S])] = {
      if (tds.isEmpty)
        decomposition
      else {
        val u = tds.head // take any element from the given type set, does not matter which
        val n = SNot(u)
        val nc = n.canonicalize(Some(NormalForm.Dnf))

        def f(pair: (S, (Set[S], Set[S]))): Map[S, (Set[S], Set[S])] = {
          val (td1, (factors, disjoints)) = pair
          lazy val a = SAnd(u, td1).canonicalize(Some(NormalForm.Dnf))
          lazy val b = SAnd(nc, td1).canonicalize(Some(NormalForm.Dnf))
          if (u.disjoint(td1).contains(true))
            Map(td1 -> (factors + n, disjoints + u))
          else if (n.disjoint(td1).contains(true))
            Map(td1 -> (factors + u, disjoints + n))
          // we omit the expensive inhabited check unless u.disjoint(td1) == dont-know
          else if (u.disjoint(td1).isEmpty && a.inhabited.contains(false))
            Map(td1 -> (factors + n, disjoints + u))
          // we omit the expensive inhabited check unless n.disjoint(td1) == dont-know
          else if (n.disjoint(td1).isEmpty && b.inhabited.contains(false))
            Map(td1 -> (factors + u, disjoints + n))
          else
            Map(a -> (factors + u, disjoints + n),
                b -> (factors + n, disjoints + u))
        }

        recur(decomposition.flatMap(f), tds - u)
      }
    }

    recur(Map(STop -> (Set(STop), Set(SEmpty))), tds - STop)
  }

  def compareSequence(tds1: Seq[SimpleTypeD], tds2: Seq[SimpleTypeD]): Boolean = {
    @tailrec
    def comp(as: List[SimpleTypeD], bs: List[SimpleTypeD]): Boolean = {
      (as, bs) match {
        case (Nil, Nil) => throw new Exception(s"not expecting equal sequences $tds1, $tds2")
        case (Nil, _) => true
        case (_, Nil) => false
        case (a :: as, b :: bs) =>
          if (a == b)
            comp(as, bs)
          else
            cmpTypeDesignators(a, b)
      }
    }

    comp(tds1.toList, tds2.toList)
  }

  /* compare two objects in a way compatible with sortWith.
  This function implements a strictly-less-than, thus it
  returns false on equal elements.
  */
  def cmpTypeDesignators(a: SimpleTypeD, b: SimpleTypeD): Boolean = {
    if (a == b)
      false
    else if (a.getClass eq b.getClass) {
      a.cmpToSameClassObj(b)
    }
    else {
      // just compare the class printed values alphabetically
      a.getClass.getName < b.getClass.getName
    }
  }

  private val Any: Class[Any] = classOf[Any]
  private val Nothing: Class[Nothing] = classOf[Nothing]
  private val Int: Class[Int] = classOf[Int]
  private val Integer: Class[Integer] = classOf[lang.Integer]
  private val Double: Class[lang.Double] = classOf[lang.Double]
  private val String: Class[String] = classOf[lang.String]
  private val ListAny: Class[List[Any]] = classOf[List[Any]]
  private val Boolean: Class[lang.Boolean] = classOf[lang.Boolean]
  private val Unit: Class[BoxedUnit] = classOf[scala.runtime.BoxedUnit]
  private val Char: Class[Character] = classOf[lang.Character]
  private val AnyRef: Class[AnyRef] = classOf[AnyRef]
  private val AnyVal: Class[AnyVal] = classOf[AnyVal]
  private val Numeric: Class[Number] = classOf[lang.Number]

  // The following definitions are defined as functions rather than
  //   simply as values, because the SAtomic constructor caches
  //   objects depending on the world view.
  def anyType(): SimpleTypeD = SAtomic(Any)

  def nothingType(): SimpleTypeD = SAtomic(Nothing)

  def intType(): SimpleTypeD = SAtomic(Int)

  def intJavaType(): SimpleTypeD = SAtomic(Integer)

  def doubleJavaType(): SimpleTypeD = SAtomic(Double)

  def stringType(): SimpleTypeD = SAtomic(String)

  def listAnyType(): SimpleTypeD = SAtomic(ListAny)

  def booleanType(): SimpleTypeD = SAtomic(Boolean)

  def unitRuntimeType(): SimpleTypeD = SAtomic(Unit)

  def charJavaType(): SimpleTypeD = SAtomic(Char)

  def anyRefType(): SimpleTypeD = SAtomic(AnyRef)

  def numericType(): SimpleTypeD = SAtomic(Numeric)

  def atomicTypesSeq(): Seq[SimpleTypeD] =
    Seq(intType(), intJavaType(), doubleJavaType(), stringType(),
        listAnyType(), booleanType(), unitRuntimeType(),
        charJavaType(), anyRefType(), numericType())

  def isEven(x: Any): Boolean = {
    x match {
      case y: Int => y % 2 == 0
      case _ => false
    }
  }

  def evenType(): SSatisfies = SSatisfies(isEven, "even")

  def isOdd(x: Any): Boolean = {
    x match {
      case y: Int => scala.math.abs(y % 2) != 0 // because e.g., -5 % 2 = -1, rather than 1
      case _ => false
    }
  }

  def oddType(): SSatisfies = SSatisfies(isOdd, "odd")

  def mysteryType(): SSatisfies =   SSatisfies(mystery, "mystery")

  private def mystery(a:Any):Boolean = {
    // This function returns false, but the purpose of the function is to
    //  model a predicate for which we do not know whether it is satisfiable.
    false
  }

  def isPrime(x: Any): Boolean = {
    @scala.annotation.tailrec
    def go(k: Int, y: Int, upper: Int): Boolean = {
      if (k > upper) true
      else if (y % k == 0) false
      // we know y is odd so it is not divisible by an even number,
      //     so increment k to next odd number
      else go(k + 2, y, upper)
    }

    x match {
      case 1 => false
      case 2 => true
      case 3 => true
      case y: Int if y % 2 == 0 => false
      case y: Int => go(3, y, scala.math.round(scala.math.sqrt(y)).toInt + 1)
      case _ => false
    }
  }

  def primeType(): SSatisfies = SSatisfies(isPrime)

  // TODO intp and oddp are useful for testing, but should be removed from library
  // TODO similar for SInt and SDouble
  def intp(a: Any): Boolean = {
    a match {
      case _: Int => true
      case _ => false
    }
  }

  def doublep(a: Any): Boolean = {
    a match {
      case _: Double => true
      case _ => false
    }
  }
}
