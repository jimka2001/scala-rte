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

import genus.Types._
import org.scalatest.funsuite.AnyFunSuite
import RandomType.randomType
import adjuvant.Adjuvant.eql

class TypesTest extends AnyFunSuite {

  test("SEql"){
    val z:SimpleTypeD = SEql(0)
    z match {
      case SEql((td,v)) => {
        assert(td == SAtomic(0.getClass()))
        assert(v == 0)
      }
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

  test("reflect.getSubTypesOf"){
    val reflect = new org.reflections.Reflections()
    assert(reflect.getSubTypesOf(classOf[List[Any]]).toArray.contains(List(1,2,3).getClass))
    assert(reflect.getSubTypesOf(classOf[List[Any]]).toArray.contains(List.empty.getClass))
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
    trait Trait1
    trait Trait2
    assert(! eql(0,0.0))
    for {d <- 1 to 3
         n <- 1 to 100
         _ <- 1 to 80
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
    for {_ <- 1 to 1000
         d <- 1 to 4
         t1 = randomType(d)
         t2 = randomType(d)
         t3 = randomType(d)
         } triangle_inequality(t1,t2,t3)
  }
  test("mdtd"){
    // Set(String, Int?, [= -1], [= 1])
    mdtd(Set(SAtomic(classOf[String]),
             SInt,
             SEql(-1),
             SEql(1)))
  }
}

