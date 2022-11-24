// Copyright (c) 2021 EPITA Research and Development Laboratory
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

import genus.NormalForm._

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.runtime.BoxedUnit

object RandomType {

  // The following definitions are defined as functions rather than
  //   simply as values, because the SAtomic constructor caches
  //   objects depending on the world view.

  val oddType: SSatisfies = SSatisfies(oddp, "odd")
  val primeType: SSatisfies = SSatisfies(isPrime)
  val evenType: SSatisfies = SSatisfies(evenp, "even")
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

  // allow implicit conversions from c:Class[_] to AtomicType(c)
  //    thus allowing Types such as classOf[java.lang.Integer] && !SEql(0)
  //    classOf[A] && classOf[B]
  implicit def class2type(c: Class[_]): SimpleTypeD = SAtomic(c)
  val interestingTypes: Vector[SimpleTypeD] = Vector(
    STop,
    SEmpty,
    SMember(1, 2, 3, 4),
    SMember(4, 5, 6),
    SEql(0),
    SEql(1),
    SEql(-1),
    SEql(true),
    SEql(false),
    //SInt, // removing these from the list because they trigger very long computation times
    //SDouble,
    SSatisfies(oddp, "oddp"),
    SSatisfies(evenp, "even"),
    SMember(false, true),
    SMember("a", "b", "c"),
    SAtomic(classOf[lang.Number]),
    SAtomic(classOf[String]),
    SAtomic(classOf[Boolean]),
    SAtomic(classOf[Integer]),
    SAtomic(classOf[Trait1]),
    SAtomic(classOf[Trait2]),
    SAtomic(classOf[Trait3]),
    SAtomic(classOf[Abstract1]),
    SAtomic(classOf[Abstract2]),
    SAtomic(classOf[Trait1X]),
    SAtomic(classOf[Trait2X]),
    SAtomic(classOf[Trait3X]),
    SAtomic(classOf[Abstract1X]),
    SAtomic(classOf[Abstract2X]),
    SAtomic(classOf[Class1X]),
    SAtomic(classOf[Class2X]),
    SAtomic(classOf[ADT1]),
    SAtomic(classOf[ADT2]),
    SOr(SAtomic(classOf[ADT1]), SAtomic(classOf[ADT2]), SAtomic(classOf[ADT3])),
    SAtomic(classOf[ADT_abstr])
    )
  val interestingValues: Vector[Any] = Vector(
    -1, -1, 0, 1, 2, 3, 4, 5, 6,
    1L, 0L, -1L, 1000L, 1000000L, // these values causes problems reported in issue #7
    3.14, 2.17, -math.sqrt(2),
    3.14d, 2.17d,
    3.14f, 2.17f,
    'a', 'b', 'c',
    true, false,
    "a", "b", "c", "d", "",
    new Class1X,
    new Class2X
    )
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

  def oddp(a: Any): Boolean = {
    a match {
      case a: Int => a % 2 != 0 // warning! -5 % 2 = -1, not 1.
      case _ => false
    }
  }

  def evenp(a: Any): Boolean = {
    a match {
      case a: Int => a % 2 == 0
      case _ => false
    }
  }

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

  def anyType(): SimpleTypeD = SAtomic(Any)

  def nothingType(): SimpleTypeD = SAtomic(Nothing)

  def atomicTypesSeq(): Seq[SimpleTypeD] =
    Seq(intType(), intJavaType(), doubleJavaType(), stringType(),
        listAnyType(), booleanType(), unitRuntimeType(),
        charJavaType(), anyRefType(), numericType())

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

  def randomType(depth: Int, filter: Option[Boolean]): SimpleTypeD = {
    if (filter.contains(false)) {
      RandomType.randomType(depth, a => !a.inhabited.contains(false))
    }
    else if (filter.isEmpty) {
      RandomType.randomType(depth, a => a.inhabited.contains(true))
    }
    else {
      RandomType.randomType(depth)
    }
  }

  def randomType(depth: Int, filter: SimpleTypeD => Boolean): SimpleTypeD = {
    @tailrec
    def recur(): SimpleTypeD = {
      val td = randomType(depth)
      if (filter(td))
        td
      else {
        recur()
      }
    }

    recur()
  }

  def randomType(depth: Int): SimpleTypeD = {
    val random = new scala.util.Random
    val maxCompoundSize = 2
    val generators: Seq[() => SimpleTypeD] = Vector(
      () => SNot(randomType(depth - 1)),
      // always at least one argument of SAnd and SOr
      () => SAnd(0 until 1 + random.nextInt(maxCompoundSize) + random.nextInt(maxCompoundSize) map { _ => randomType(depth - 1) }: _*),
      () => SOr(0 until 1 + random.nextInt(maxCompoundSize) + random.nextInt(maxCompoundSize) map { _ => randomType(depth - 1) }: _*)
      )
    if (depth <= 0)
      interestingTypes(random.nextInt(interestingTypes.length))
    else {
      val g = generators(random.nextInt(generators.length))
      g()
    }
  }

  def sanityTest(): Unit = {
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
    println(t2.canonicalize(nf = Some(Dnf)))
    println(t1.canonicalize())
    println(t1.canonicalize(nf = Some(Dnf)))
    println(SNot(t1).canonicalize(nf = Some(Dnf)))
    (0 to 10).foreach { i =>
      val t = randomType(6)
      println(s"$i:" + t)
      println("   " + t.canonicalize())
    }

    println(t1 || t2)
    println(t1 && t2)
    println(t1 ^^ t2)
    println(t1 - t2)
    println(!t1)

    println(classOf[String] || classOf[Integer])
  }

  // Given a set of type designators, return a newly computed Seq of triples:
  //    (td:SimpleTypeD,factors:List[SimpleTypeD],disjoints:List[SimpleTypeD])
  // indicating type designators which implement the Maximal Disjoint Type Decomposition.
  // I.e., the computed list (the list of all the td components) designates a set all of whose
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
    def recur(decomposition: Map[S, (Set[S], Set[S])], tds: List[S]): Map[S, (Set[S], Set[S])] = {
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

        recur(decomposition.flatMap(f), tds.tail)
      }
    }

    recur(Map(STop -> (Set(STop), Set(SEmpty))), (tds - STop).toList.sortBy(_.toString))
  }

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

  def main(args: Array[String]): Unit = {
    case class Box(value: Any)
    println(SAtomic(classOf[scala.runtime.RichInt]).typep(1))
    println(SAtomic(classOf[Int]).typep(Box(1).value))
    println(SAtomic(classOf[java.lang.Integer]).typep(Box(1).value))
    println(SAtomic(classOf[java.lang.Integer]).typep(1)) // works
    println(SAtomic(classOf[Integer]).typep(1)) // works
    println(classOf[java.lang.Integer].isInstance(1)) // works
    println(classOf[Int].isInstance(1))
    println(1.isInstanceOf[Int])
    println(1.isInstanceOf[Any])
    println(classOf[Any].isInstance(1))
    println(classOf[java.lang.Object].isInstance(1))

  }

  // the following classes have no instantiatable subclass
  trait Trait1

  trait Trait2

  trait Trait3 extends Trait2

  trait Trait1X

  trait Trait2X // has subclass Trait3X which has subclass Abstract2X

  trait Trait3X extends Trait2X // has subclass Abstract2X

  sealed abstract class ADT_abstr

  abstract class Abstract1

  abstract class Abstract2 extends Trait3

  abstract class Abstract1X // has subclass Class1X

  abstract class Abstract2X extends Trait3X

  class ADT1 extends ADT_abstr

  class ADT2 extends ADT_abstr

  class ADT3 extends ADT_abstr

  class Class1X extends Abstract1X

  class Class2X extends Abstract2X
}
