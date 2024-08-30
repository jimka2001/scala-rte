// Copyright (c) 2020,21 EPITA Research and Development Laboratory
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

import org.scalatest.funsuite.AnyFunSuite
import RandomType.randomType
import adjuvant.Adjuvant.eql
import adjuvant.AdjFunSuite
import genus.RandomType._
import genus.Types._
import genus.GenusImplicits


import scala.language.implicitConversions

class TypesTest extends AdjFunSuite {
  val SInt = SSatisfies(Types.intp,"SInt")
  val SDouble = SSatisfies(Types.doublep,"SDouble")

  test("isPrime"){
    val primes = Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
    for{p <- 1 to 30}
      assert(primes.contains(p) == isPrime(p), s"wrong answer for $p got ${isPrime(p)}")
  }
  test("SEql"){
    val z:SimpleTypeD = SEql(0)
    z match {
      case SEql((td,v)) =>
        assert(td == SAtomic(0.getClass))
        assert(v == 0)
      case _ => fail()
    }
    assert(z == SEql(0))
    assert(SEql(0) != SEql(0.0))
    assert(SEql(0) != SEql(0L))

    assert(SNot(SEql(0)) != SNot(SEql(0.0)))
    assert(SNot(SEql(0)) == SNot(SEql(0)))
    assert(SOr(SEql(0),SEql(0.0)) == SOr(SEql(0),SEql(0.0)))
    assert(SOr(SEql(0),SEql(0.0)) != SOr(SEql(0L),SEql(0.0)))

    assert(SEql(0).typep(0))
    assert(! SEql(0).typep(0.0))
    assert(! SEql(0).typep(0L))

    assert(SEql(0).disjoint(SEql(0.0)).contains(true))
    assert(SEql(0).inhabited.contains(true))
    assert(SEql(0).disjoint(SEql(0)).contains(false))

    assert(SEql(0).subtypep(SEql(0)).contains(true))
    assert(SEql(0).subtypep(SEql(0.0)).contains(false))
    assert(SEql(0).subtypep(SMember(0,0.0)).contains(true))
  }
  test("SMember"){
    val z:SimpleTypeD = SMember(0,0L,0.0)
    assert(z == SMember(0L,0.0,0))
    assert(z == SMember(0L,0,0.0))
    assert(z == SMember(0,0L,0.0))
    assert(z == SMember(0,0.0,0L))
    assert(z == SMember(0.0,0L,0))
    assert(z == SMember(0.0,0,0L))
    assert(z == SMember(0.0,0,0L,0L,0,0.0))
    assert(z != SMember(0.0,0))
    assert(SMember(0) == SEql(0))
    assert(SMember(0.0) != SEql(0))

    assert(!SMember(0,0L).typep(0.0))
    assert(SMember(0,0L).typep(0L))
    assert(SMember(0,0L).typep(0))
    assert(SMember(0,0L).inhabited.contains(true))
    assert(SMember().inhabited.contains(false))
    assert(SMember(0).inhabited.contains(true))
    assert(SMember(0,0L).disjoint(SMember(0.0,0)).contains(false))
    assert(SMember(0,0L).disjoint(SMember(0.0,1.0)).contains(true))
    assert(SMember(0,0L,0.0).subtypep(SMember(0,0L,0.0,1,1L,1.0)).contains(true))
    assert(SMember(0,0L,0.0).subtypep(SMember(0,0L,1,1L,1.0)).contains(false))
  }

  test("reflect malfunction"){
    val reflect = new org.reflections.Reflections(classOf[List[_]])
    // If this test fails, there may be some problem with the initialization of
    // the reflections library.
    for{c <- List(classOf[List[Any]],
                  classOf[Vector[Any]]
                  )}
      assert(reflect.getSubTypesOf(classOf[List[Any]]).toArray.nonEmpty,
             s"There seems to be a problem with org.reflections.Reflections().reflect.getSubTypesOf($c)," +
               " it cannot find any subtypes")


  }

  test("reflect.getSubTypesOf List"){
    val reflect = new org.reflections.Reflections(classOf[List[_]])
    assert(reflect.getSubTypesOf(classOf[List[Any]]).toArray.nonEmpty)
    assert(reflect.getSubTypesOf(classOf[List[Any]]).toArray.contains(List(1,2,3).getClass))
    assert(reflect.getSubTypesOf(classOf[List[Any]]).toArray.contains(List.empty.getClass))
  }

  test("reflect.getSubTypesOf Number"){
    val reflect = new org.reflections.Reflections()
    assert(reflect.getSubTypesOf(classOf[Number]).toArray.nonEmpty)
  }

  test("Number has instantiatable subclass"){

    import genus.SAtomic.existsInstantiatableSubclass
    import genus.SAtomic.reflections
    assert(reflections.getSubTypesOf(classOf[Number]).toArray.toList.nonEmpty)
    assert(existsInstantiatableSubclass(classOf[Number]))
  }

  test("Number not empty"){
    //println(classOf[Number])
    assert(SAtomic(classOf[Number]) != SEmpty)
  }

  test("reflection Number"){
    // this tests that reflection has properly installed the sublcass relationships to Number
    assert(SEql(1).subtypep(SAtomic(classOf[Number])) == Some(true))
    assert(SAtomic(classOf[Number]).inhabited == Some(true))
    assert(SMember(1,2,3).subtypep( SAtomic(classOf[Number])) == Some(true))
    assert(SAtomic(classOf[java.lang.Double]).subtypep(SAtomic(classOf[Number])) == Some(true))
  }
  test("sort 1") {
    assert(List(SEmpty, STop).sortWith(cmpTypeDesignators)
           == List(STop, SEmpty).sortWith(cmpTypeDesignators))
    trait Trait1
    trait Trait2
    for {seq <- List(List(SEmpty, STop),
                     List(SEmpty, SEql(0), STop),
                     List(SEmpty, SEql(0), SEql(""), STop),
                     List(SEmpty, SEql(0), SEql(""), SEql(-1), STop),
                     List(SEmpty, SEql(0), SEql(""), SEql(-1), SMember(1, 2, 3), STop),
                     List(SEmpty, SEql(0), SMember(2, 1, 4), SEql(""), SEql(-1), SMember(1, 2, 3), STop),
                     List(SAnd(), SNot(SEql(0)), SAnd(SAtomic( classOf[java.lang.String])),
                          SAnd(SAtomic( classOf[java.lang.String]),SAtomic( classOf[java.lang.Integer])),
                          SAnd(SEql(-1)),
                          SNot(SAtomic( classOf[java.lang.Integer]))),
                     List(
                       SNot(SMember("a","b","c")),
                       SNot(SAtomic(classOf[Trait1])),
                       SNot(SAtomic( classOf[java.lang.Integer]))
                       ),
                     List(SAnd(SEmpty,classOf[Trait1]),
                          SNot(SAtomic(classOf[java.lang.Number])),
                          SNot(SAtomic(classOf[Trait1])),
                          SNot(SMember("a","b","c")),
                          SOr(SAtomic(classOf[Trait2]),SAtomic(classOf[java.lang.Integer])))
                     )} {
      assert(seq.sortWith(cmpTypeDesignators) == seq.reverse.sortWith(cmpTypeDesignators))
    }
  }

  test("sort 2") {
    assert(List(SEmpty, STop).sortWith(cmpTypeDesignators)
           == List(STop, SEmpty).sortWith(cmpTypeDesignators))
    assert(! eql(SEql(0),SEql(0L)))
    locally {
      for {data <- Seq(Seq(SEql(0), SEql(1), SEql(0L)),
                       Seq(SEql(1), SEql(0L), SEql(0)),
                       Seq(SNot(SEql(0)), SNot(SEql(1)), SNot(SEql(0L))),
                       Seq(SNot(SEql(1)), SNot(SEql(0L)), SNot(SEql(0)))
                       )} {

        assert(data.sortWith(cmpTypeDesignators)
                 == data.reverse.sortWith(cmpTypeDesignators))
      }
    }
    //trait Trait1
    //trait Trait2
    assert(! eql(0,0.0))
    for {d <- 1 to 3
         n <- 1 to 100
         _ <- 1 to num_random_tests / 1000 * 20
         li = for {_ <- 1 to 10} yield randomType(d)
         m <- 1 to n
         prefix = li.take(m)
         } {
      val o1 = prefix.sortWith(cmpTypeDesignators)
      val o2 = prefix.reverse.sortWith(cmpTypeDesignators)
      assert(eql(o1,o2),
             s"\nd=$d, n=$n cannot sort ${prefix.getClass} : $prefix\n" +
             s"o1=$o1\n" +
             s"o2=$o2")
    }
  }
  def triangle_inequality(t1:SimpleTypeD,t2:SimpleTypeD,t3:SimpleTypeD):Unit = {
    if (cmpTypeDesignators(t1,t2) && cmpTypeDesignators(t2,t3))
      assert(cmpTypeDesignators(t1,t3),
             s"$t1 < $t2, and $t2 < $t3, but not $t1 < $t3")
  }
  test("triangle inequality"){
    trait Trait1
    trait Trait2
    triangle_inequality(SOr(SAtomic(classOf[Trait1])),
                        SNot(SEql(-1)),
                        SNot(SAtomic(classOf[java.lang.String]))
                        )
    triangle_inequality(SNot(SAtomic(classOf[java.lang.Integer])),
                        SNot(SAtomic(classOf[Trait1])),
                        SNot(SAtomic(classOf[Trait2]))
                        )
    for {_ <- 1 to num_random_tests/5
         d <- 1 to 4
         t1 = randomType(d)
         t2 = randomType(d)
         if cmpTypeDesignators(t1,t2)
         t3 = randomType(d)
         } triangle_inequality(t1,t2,t3)
  }
  test("mdtd") {
    val str = SAtomic(classOf[String])
    val int = SAtomic(classOf[Int])
    val m = mdtd(Set(str,
                     int,
                     SEql(-1),
                     SEql(1)))

    // the set of keys of m are all mutually disjoint types (SimpleTypeD)
    //   whose union in the same as the union of str, int, SEql(-1), SEql(1).
    // corresponding to each key, td, is a pair (factors, disjoints)
    //    where td is a subset of each factor
    //    and td is disjoint with each disjoint
    val expected = Map(SEql(-1 ) -> (Set(!SEql(1 ), STop, SEql(-1 ), int, !str),
                                     Set(!int, SEql(1 ), SEmpty, !SEql(-1 ), str)),
                       SEql(1 ) -> (Set(STop, SEql(1), int, !SEql(-1 ), !str),
                                    Set(!SEql(1 ), SEql(-1 ), !int, SEmpty, str)),
                       SAnd(!int,!str) -> (Set(!SEql(1), STop, !int, !SEql(-1), !str),
                                           Set(SEql(-1), SEql(1), int, SEmpty, str)),
                       SAnd(int,!SMember(-1, 1)) -> (Set(!SEql(1), STop, int, !SEql(-1), !str),
                         Set(SEql(-1 ), !int, SEql(1 ), SEmpty, str)),
                       str -> (Set(!SEql(1), STop, !int, !SEql(-1), str),
                         Set(SEql(-1 ), SEql(1 ), int, SEmpty, !str)))

    if (m != expected){
      for {(a, e) <- m.zip(expected)
           } if ( a == e) {
        println(s"     SAME: $a")
      } else {
        println(s"DIFFERENT:")
        println(s"         : a=$a")
        println(s"         : e=$e")
      }
    }
    for {(a, e) <- m.zip(expected)} assert(a == e)
    assert(m == expected)
  }
  test("typeEquivalent random") {
    for {_ <- 0 to num_random_tests
         d <- 0 to 4
         t1 = randomType(d)
         t2 = randomType(d)
         } {
      assert(t1.typeEquivalent(t1).contains(true))
      assert(!t1.typeEquivalent(SNot(t1)).contains(true))
      assert(! SNot(SAnd(t1,t2)).typeEquivalent(SOr(SNot(t1),SNot(t2))).contains(false))
    }
  }

  test("typeEquivalent fixed"){
    def f(_a:Any):Boolean = {
      true
    }
    def g(_a:Any):Boolean = {
      true
    }
    val t1 = SSatisfies(f,"f")
    val t2 = SSatisfies(g,"g")

    val sodd = oddType()
    assert(sodd.typep(7) == true)
    assert(sodd.typep(6) == false)
    assert(sodd.typep(7.0) == false)
    assert(sodd.typep("hello") == false)

    assert(sodd.subtypep(SAtomic(classOf[Int])) == None)
    assert(SAtomic(classOf[Int]).subtypep(sodd) == None)
    assert(sodd.subtypep(STop) == Some(true))
    assert(SEmpty.subtypep(sodd) == Some(true))

    assert(SAtomic(classOf[Int]).subtypep(SAtomic(classOf[Number])) == Some(true))

    for {(a: SimpleTypeD, b: SimpleTypeD, lt: Option[Boolean], gt: Option[Boolean], eq: Option[Boolean]) <-
           Seq((t1, t1, Some(true), Some(true), Some(true)),
               // A < B = None    B < A = None
               (t1, t2, None, None, None),
               //  A < B = None   B < A = False
               (sodd, SEql(2), None, Some(false), Some(false)),
               // A < B = None   B < A = True
               (t1, SEmpty, None, Some(true), None),

               // A < B = True   B < A = None
               (SEmpty, t1, Some(true), None, None),
               // A < B = True   B < A = False
               (SMember(1, 2), SMember(1, 2, 3), Some(true), Some(false), Some(false)),
               // A < B = True   B < A = True
               (SMember(1, 2, 3), SMember(2, 1, 3), Some(true), Some(true), Some(true)),

               // A < B = False  B < A = None
               (SMember(2,4), sodd, Some(false), None, Some(false)),
               // A < B = False  B < A = False
               (SMember(1, 2, 3), SMember(2, 3, 4), Some(false), Some(false), Some(false)),
               // A < B = False  B < A = True
               (SMember(1, 2, 3), SMember(2, 3), Some(false), Some(true), Some(false))
               )
         } {
      assert(a.subtypep(b) == lt, s"\na=$a\nb=$b\nexpecting a < b = $lt, got ${a.subtypep(b)}")
      assert(b.subtypep(a) == gt, s"\na=$a\nb=$b\nexpecting a > b = $gt, got ${b.subtypep(a)}")
      assert(a.typeEquivalent(b) == eq, s"\na=$a\nb=$b\nexpecting a = b = $eq, got ${a.typeEquivalent(b)}")
    }
  }
  test("test 103") {
    val td = SAnd(SNot(SEql(true)),
                  SMember(false, true)
                  )
    val dnf = SAnd(SAtomic(classOf[Boolean]), !SEql(true: Boolean))
    val diff: SAnd = SAnd(dnf, SNot(td))
    assert(SAnd(dnf,SNot(td)).canonicalize() == SEmpty)
  }
  test("genus implicits"){
    import genus.GenusImplicits

    assert(SOr(SAtomic(classOf[String]),
               SAtomic(classOf[Int])) == SOr(classOf[String],classOf[Int]))

  }
}

