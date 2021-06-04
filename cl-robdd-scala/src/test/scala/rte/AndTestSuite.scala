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

package rte

import genus._
import org.scalatest.funsuite.AnyFunSuite
//import rte.RteImplicits._

class AndTestSuite extends AnyFunSuite {
  test("and test 31"){
    assert(And(Star(Singleton(SAtomic(classOf[String]))),
               Rte.sigmaStar).canonicalizeOnce
             != Rte.sigmaStar)
    assert(And(Star(Singleton(SAtomic(classOf[String]))),
               Rte.sigmaStar).canonicalize
             != Rte.sigmaStar)
    assert(And(Star(Singleton(SAtomic(classOf[String]))),
               Not(EmptySet)).canonicalizeOnce
             != Rte.sigmaStar)
    assert(And(Star(Singleton(SAtomic(classOf[String]))),
               Not(EmptySet)).canonicalize
             != Rte.sigmaStar)
  }
  test("canonicalize and 29a"){
    val I = Singleton(SInt)
    val S = Singleton(SAtomic(classOf[String]))
    val X = Singleton(SEql(-1))
    val Y = Singleton(SEql(1))
    val ε = EmptyWord
    import adjuvant.Adjuvant.trace

    val rte4 = Or(And(Or(Cat(Star(S),I),ε),
                      Cat(Star(S),I)),
                  And(Or(Cat(Star(S),I),ε),
                      ε))

    // rte5 = Or(And(Cat(Star(S),I),
    //               Or(Cat(Star(S),I),
    //                  ε)),
    //           And(ε,
    //               Or(Cat(Star(S),I),
    //                  ε)))
    val rte5 = rte4.canonicalizeOnce
    // rte6 = Or(And(Or(Cat(Star(S),I),
    //                  ε),
    //               Cat(Star(S),I)),
    //           And(Or(Cat(Star(S),I),
    //                  ε),
    //               ε))
    val rte6 = rte5.canonicalizeOnce

    //assert(rte6 != rte4,"line 45")
    rte6.canonicalize
  }
  test("canonicalize and 29b"){
    // And(Or(Cat((<String>)*,<Int?>),ε),
    //     Not(Cat((<String>)*,<[= -1]>)),
    //     Not(Cat((<String>)*,<[= 1]>)),
    //     Not((<String>)*))
    val I = Singleton(SInt)
    val S = Singleton(SAtomic(classOf[String]))
    val X = Singleton(SEql(-1))
    val Y = Singleton(SEql(1))
    val rte1 = And(Or(Cat(Star(S),I), EmptyWord),
        Not(Cat(Star(S),X)),
        Not(Cat(Star(S),Y)),
        Not(Star(S)))
    import adjuvant.Adjuvant.trace
    val rte2 = rte1.canonicalizeOnce
    val rte3 = rte2.canonicalizeOnce
    val rte4 = rte3.canonicalizeOnce
    val rte5 = rte4.canonicalizeOnce
    rte5.canonicalize
  }
  test("canonicalize and 31"){
    assert(And(Singleton(SEql(0)), Not(Cat(Sigma,Star(Sigma)))).canonicalize != Singleton(SEql(0)))
    assert(And(Singleton(SEql(0)), Not(Cat(Sigma,Star(Sigma)))).canonicalize ~= EmptySet)
  }

  test("canonicalize and 242") {
    // And(Not(<[= 4]>),<[Member 0,4,5,6]>,Not(<[= 1]>),<[Member 1,2,3,4]>)
    assert(And(Not(Singleton(SEql(4))),
               Singleton(SMember(0, 4, 5, 6)),
               Not(Singleton(SEql(1))),
               Singleton(SMember(1, 2, 3, 4))).canonicalize == EmptySet)
    assert(And(Singleton(SEql(4)),
               Singleton(SMember(0, 4, 5, 6)),
               Not(Singleton(SEql(1))),
               Singleton(SMember(1, 2, 3, 4))).canonicalize == Singleton(SEql(4)))

    assert(And(Singleton(genus.SEql(0)), Sigma).canonicalize == Singleton(genus.SEql(0)))
    assert(And(EmptySet, EmptySet).canonicalize == EmptySet)
  }
  test("and 114"){
    // And(A,B,Or(X,Y,Z),C,D)
    // --> Or(And(A,B,   X,   C, C)),
    //        And(A,B,   Y,   C, C)),
    //        And(A,B,   Z,   C, C)))
    trait TraitA
    trait TraitB
    trait TraitX
    trait TraitY
    class ClassA extends TraitX with TraitY with TraitA with TraitB

    val A = Singleton(SAtomic(classOf[TraitA]))
    val B = Singleton(SAtomic(classOf[TraitB]))
    val X = Singleton(SAtomic(classOf[TraitX]))
    val Y = Singleton(SAtomic(classOf[TraitY]))

    assert(And(A,Or(X,Y)).canonicalizeOnce == Or(And(A,X),And(A,Y)))
    assert(And(A,Or(X,Y),B).canonicalizeOnce == Or(And(A,B,X),And(A,B,Y)))
  }
  test("canonicalize and") {
    assert(And(EmptySet,EmptySet).canonicalize == EmptySet)
    assert(EmptySet.canonicalize == EmptySet)
    class TestSup
    class TestSub extends TestSup
    class TestD1 // disjoint from TestD2
    class TestD2 // disjoint from TestD1
    val trsup = Singleton(genus.SAtomic(classOf[TestSup]))
    val trsub = Singleton(genus.SAtomic(classOf[TestSub]))
    val trd1 = Singleton(genus.SAtomic(classOf[TestD1]))
    val trd2 = Singleton(genus.SAtomic(classOf[TestD2]))
    for {depth <- 0 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         } {
      // removing duplicates
      assert(And(r1, r1).canonicalize == r1.canonicalize,
             s"\nAnd(r1,r1)=${And(r1, r1).canonicalize}\n   r1=${r1.canonicalize}")
      assert(And(r1, r2, r1).canonicalize == And(r1, r2).canonicalize ||
               And(r1, r2, r1).canonicalize == And(r2, r1).canonicalize
             )
      assert(And(r1, Sigma, r2, EmptyWord).canonicalize == EmptySet)
      assert(And(r1, EmptySet, r2).canonicalize == EmptySet)

      assert(And(And(r1, r2), r3).canonicalize ~= And(r1, And(r2, r3)).canonicalize,
             s"\nr1=$r1  \nr2=$r2  \nr3=$r3   \ncanonicalized: \n  r1=${r1.canonicalize}\n  r2=${r2.canonicalize}\n  r3=${r3.canonicalize}")
      assert(And(And(r1, r2), r3).canonicalize ~= And(r1, r2, r3).canonicalize)
      assert(And(r1, Rte.sigmaStar, r2, r3).canonicalize ~= And(r1, r2, r3).canonicalize)
      assert(And(r1, Sigma.*, r2, r3).canonicalize ~= And(r1, r2, r3).canonicalize)
      assert(And(r1, trsup, r2, trsub, r3).canonicalize ~= And(r1, r2, trsub, r3).canonicalize)
      assert(And(r1, trsub, r2, trsup, r3).canonicalize ~= And(r1, trsub, r2, r3).canonicalize)

      assert(And(r1, r2, Not(r1), r3).canonicalize ~= EmptySet,
             "\nexpecting " +
               And(r1, r2, Not(r1), r3) +
               " to reduce to EmptySet" +
               "\nnot to " + And(r1, r2, Not(r1), r3).canonicalize
             )
      assert(And(r1, trd1, r2, trd2, r3).canonicalize ~= EmptySet)
    }
  }
  test("and 149"){
    val r1 = Singleton(SEql(1))
    val r2 = Singleton(SEql(2))
    val r3 = Singleton(SEql(3))
    val r4 = Singleton(SEql(4))

    val rte0 = And(r1,Or(r2,r3),r4)
    val rte1 = rte0.canonicalizeOnce
    val rte2 = rte1.canonicalizeOnce
    val rte3 = rte2.canonicalizeOnce

  }
  test("canonicalize and 88") {

    val r1 = Singleton(SEql(1))
    val r2 = Singleton(SEql(2))
    val r3 = Singleton(SEql(3))
    val r4 = Singleton(SEql(4))
    // And(a,Or(x,y),b) --> Or(And(a,x,b),And(a,y,b))
    assert(And(r1, Or(r2, r3), r4).canonicalize ~= Or(And(r1, r2, r4),
                                                      And(r1, r3, r4)).canonicalize,
           s"\nr1=$r1  r2=$r2  r3=$r3  r4=$r4" +
             s"\n  canonicalized: r1=${r1.canonicalize}  r2=${r2.canonicalize}  r3=${r3.canonicalize}  r4=${r4.canonicalize}" +
             "\n And(r1,r2,r4)=" + s" And($r1,$r2,$r4)=" + And(r1, r2, r4).canonicalize +
             "\n And(r1,r3,r4)=" + s" And($r1,$r3,$r4)=" + And(r1, r3, r4).canonicalize +
             "\n And(r1,Or(r2,r3),r4)=" + And(r1, Or(r2,r3), r4).canonicalize +
             "\n Or(And(r1,r2,r4),And(r1,r3,r4))=" + Or(And(r1, r2, r4),
                                                        And(r1, r3, r4)).canonicalize
           )

  }

  test("canonicalize and 105") {
    abstract class Test1
    val r1 = Star(Sigma)
    val r2 = Singleton(SEql(1))
    val r3 = r1
    val r4 = Star(Singleton(SAtomic(classOf[Test1])))
    // And(a,Or(x,y),b) --> Or(And(a,x,b),And(a,y,b))
    assert(Or(r1, r4).canonicalize == r1)
    assert(Or(r4, r1).canonicalize == r1)
    assert(And(r1, Or(r2, r3), r4).canonicalize ~= Or(And(r1, r2, r4),
                                                      And(r1, r3, r4)).canonicalize,
           s"\nr1=$r1  r2=$r2  r3=$r3  r4=$r4" +
             s"\n  canonicalized: r1=${r1.canonicalize}  r2=${r2.canonicalize}  r3=${r3.canonicalize}  r4=${r4.canonicalize}" +
             "\n And(r1, Or(r2, r3), r4)=" + s"  And($r1, Or($r2, $r3), $r4)=" + And(r1, Or(r2, r3), r4).canonicalize +
             "\n Or(r2, r3)=" + s"  Or($r2, $r3)=" + Or(r2, r3).canonicalize +
             "\n And(r1,r2,r4)=" + s" And($r1,$r2,$r4)=" + And(r1, r2, r4).canonicalize +
             "\n And(r1,r3,r4)=" + s" And($r1,$r3,$r4)=" + And(r1, r3, r4).canonicalize +
             "\n Or(And(r1, r2, r4), And(r1, r3, r4)) = " + Or(And(r1, r2, r4), And(r1, r3, r4)).canonicalize
           )
  }

  test("canonicalize and 228") {
    assert(And(Cat(Sigma,Star(Sigma)),Sigma).canonicalize
             == Sigma)
    for {depth <- 0 to 4
         _ <- 1 to 500
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         r4 = Rte.randomRte(depth)
         } {

      // And(a,Or(x,y),b) --> Or(And(a,x,b),And(a,y,b))
      assert(And(r1, Or(r2, r3), r4).canonicalize ~= Or(And(r1, r2, r4),
                                                        And(r1, r3, r4)).canonicalize,
             s"\nr1=$r1  \nr2=$r2  \nr3=$r3  \nr4=$r4" +
               s"\n  canonicalized:\n  r1=${r1.canonicalize}\n  r2=${r2.canonicalize}\n  r3=${r3.canonicalize}\n  r4=${r4.canonicalize}" +
               "\n  And(r1,r2,r4)= " + And(r1, r2, r4).canonicalize +
               "\n  And(r1,r3,r4)= " + And(r1, r3, r4).canonicalize +
               "\n  Or(And(r1,r2,r4), And(r1,r3,r4))= "+Or(And(r1, r2, r4),
                                                         And(r1, r3, r4)).canonicalize +
               "\n  And(r1, Or(r2, r3), r4)= "+And(r1, Or(r2, r3), r4).canonicalize
             )
    }
  }

  test("canonicalize and 253") {
    assert(And(Star(Sigma),Star(Sigma),Sigma).canonicalize == Sigma)
    assert(And(Star(Sigma),Or(Star(Sigma),Star(Sigma)),Sigma).canonicalize == Sigma)
    assert(And(Singleton(STop),Not(Singleton(SMember(4,5,6)))).canonicalize
             != Not(Singleton(SMember(4,5,6))))
    val r1 =And(Star(Sigma), Or(Star(Sigma),Star(Singleton(SEql(1)))), Sigma)
    val r2 = r1.canonicalize
    locally{
      val r1:Rte = Not(Singleton(SMember(4,5,6)))
      val r2:Rte = Singleton(SMember("a","b","c"))
      val r3:Rte = Or(Singleton(SMember(1,2,3,4)),Singleton(STop))
      val r4:Rte = Sigma
      val r5 = And(r1,Or(r2,r3),r4)
      val r6 = r5.canonicalizeOnce
      val r7 = r6.canonicalizeOnce
      val r8 = r7.canonicalizeOnce
      val r9 = r8.canonicalizeOnce
      val r10 = r9.canonicalizeOnce
      val r11 = r10.canonicalizeOnce
      val r12 = r11.canonicalizeOnce
      val r13 = r12.canonicalizeOnce
      val r14= r13.canonicalizeOnce
      val r15= r14.canonicalizeOnce

      assert(And(r1,Or(r2,r3).canonicalize,r4).canonicalize
             == And(r1,Or(r2,r3),r4).canonicalize)
    }
    assert(r2.canonicalize == Sigma)
    for {depth <- 0 to 1
         _ <- 1 to 5000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         r4 = Rte.randomRte(depth)
         } {
      assert(And(r1, Or(r2, r3).canonicalize, r4).canonicalize ~=
               And(r1, Or(r2, r3), r4).canonicalize,
             s"\nr1=$r1"+
               s"\nr2=$r2"+
               s"\nr3=$r3"+
               s"\nr4=$r4"+
               "\ncanonicalized:" +
               s"\n  r1= " + r1.canonicalize +
               s"\n  r2= " + r2.canonicalize +
               s"\n  r3= " + r3.canonicalize +
               s"\n  r4= " + r4.canonicalize +
               s"\n Or(r2, r3).canonicalize= " + Or(r2, r3).canonicalize +
               s"\n And(r1, Or(r2, r3).canonicalize, r4).canonicalize= " + And(r1, Or(r2, r3).canonicalize, r4).canonicalize +
               s"\n And(r1, Or(r2, r3), r4).canonicalize= " + And(r1, Or(r2, r3), r4).canonicalize
             )
    }
  }

  test("canonicalize and 352"){
    abstract class Test1
    val r1 = Star(Sigma)
    val r2 = Singleton(SEql(1))
    val r4 = Star(Singleton(SAtomic(classOf[Test1])))
    assert(And(r2,r4).canonicalize == EmptySet)
    assert(And(r1,r2,r4).canonicalize == EmptySet)
  }
  test("canonicalize and 315"){
    val r1 = Or(//Cat(Sigma,Sigma,Star(Sigma)),
                Not(Singleton(SEql(0))),
                Star(Singleton(SEql(1)))
                )
    val r2 = Star(Sigma)

    assert(And(r1, r2, Not(r1)).canonicalize == EmptySet,
           s"r1=$r1  r2=$r2 ")
  }

  test("canonicalize and 325") {

    class TestD1 // disjoint from TestD2
    class TestD2 // disjoint from TestD1

    val trd1 = Singleton(genus.SAtomic(classOf[TestD1]))
    val trd2 = Singleton(genus.SAtomic(classOf[TestD2]))
    for {depth <- 0 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         } {

      assert(And(r1, Not(r1)).canonicalize ~= EmptySet,
             s"r1=$r1")
      assert(And(r1, r2, Not(r1)).canonicalize ~= EmptySet,
             s"r1=$r1  r2=$r2 ")
      assert(And(r1, r2, Not(r1), r3).canonicalize ~= EmptySet,
             s"r1=$r1  r2=$r2   r3=$r3")
      assert(And(r1, Not(r1), r3).canonicalize ~= EmptySet,
             s"r1=$r1  r3=$r3")
      assert(And(r1, trd1, r2, trd2, r3).canonicalize ~= EmptySet,
             s"r1=$r1  r2=$r2   r3=$r3")
    }
  }
  test("and conversion1"){
    assert(And().conversion1() == Sigma)
  }
  test("and conversion2"){
    assert(And(And()).conversion2() == And())
  }
  test("and conversion3"){
    assert(And(And(),EmptySet,And()).conversion3() == EmptySet)
  }
  test("and conversion4"){
    assert(And(Or(),And(),And(),Or()).conversion4() == And(And(),Or()))
  }
  test("and conversion5"){
    assert(And(Singleton(SEql(2)),Singleton(SEql(1))).conversion5()
             == And(Singleton(SEql(1)),Singleton(SEql(2))))
    assert(And(Singleton(SEql(1)),Singleton(SEql(2))).conversion5()
             == And(Singleton(SEql(1)),Singleton(SEql(2))))
  }
  test("and conversion6"){
    // remove Sigma* and flatten And(And(...)...)
    assert(And(Star(Sigma),
               And(Singleton(SEql(1)),Singleton(SEql(2))),
               And(Singleton(SEql(3)),Singleton(SEql(4))),
               Star(Sigma)).conversion6()
      == And(Singleton(SEql(1)),Singleton(SEql(2)),
             Singleton(SEql(3)),Singleton(SEql(4))))
  }
  test("and conversion7"){
    assert(And(EmptyWord,Singleton(SEql(1))).conversion7(true)
           == EmptySet)
  }
  test("and conversion8"){
    // if operands contains EmptyWord, then the intersection is either EmptyWord or EmptySet
    assert(And(EmptyWord,Star(Singleton(SEql(1)))).conversion8()
             == EmptyWord)
    assert(And(EmptyWord,Singleton(SEql(1))).conversion8()
             == EmptySet)
  }
  test("and conversion9"){
    val x = Singleton(SEql(1))
    val y = Singleton(SEql(1))
    // if x matches only singleton then And(x,y*) -> And(x,y)
    assert(And(x,Star(y)).conversion9(true)
             == And(x,y))
    assert(And(Star(x),y).conversion9(true)
             == And(x,y))
    assert(And(Star(x),Star(y)).conversion9(false)
             == And(Star(x),Star(y)))
    assert(And(x,y).conversion9(true)
             == And(x,y))
  }
  test("and conversion10"){
    // And(A,B,Or(X,Y,Z),C,D)
    // --> Or(And(A,B,   X,   C, C)),
    //        And(A,B,   Y,   C, C)),
    //        And(A,B,   Z,   C, C)))
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    val c = Singleton(SEql("c"))
    val d = Singleton(SEql("d"))
    val x = Singleton(SEql("x"))
    val y = Singleton(SEql("y"))
    val z = Singleton(SEql("z"))
    assert(And(a,b,Or(x,y,z),c,d).conversion10()
           == Or(And(a,b,x,c,d),
                 And(a,b,y,c,d),
                 And(a,b,z,c,d)))
  }
  test("and conversion11"){
    // And(...,x,Not(x)...)
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    assert(And(a,b,Not(a)).conversion11()
             == EmptySet)
    assert(And(a,Not(b),Not(Not(b))).conversion11()
             == EmptySet)
  }
  test("and conversion12"){
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    assert(And(Singleton(SEmpty),a,b).conversion12(List(SEmpty,SEql("a"),SEql("b")))
             == EmptySet)
  }
  test("and conversion13"){
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    assert(And(Sigma,Singleton(SEmpty),a,b).conversion13(List(SEmpty,SEql("a"),SEql("b")))
             == And(Singleton(SEmpty),a,b))
    assert(And(Sigma,Singleton(SEmpty),Singleton(SEmpty)).conversion13(List(SEmpty,SEmpty))
             == And(Sigma,Singleton(SEmpty),Singleton(SEmpty)))
  }
  test("and conversion15"){

  }
  test("and conversion16"){

  }
  test("and conversion17"){

  }
  test("and conversion18"){

  }
  test("and conversion19"){

  }
  test("and conversion20"){

  }
  test("and conversion21"){
    assert(And(Singleton(SMember(1,2,3,4)),Not(Singleton(SMember(1,2)))).conversion21()
             == Singleton(SMember(3,4)))
    assert(And(Singleton(SMember(1,2,3,4)),Not(Singleton(SMember(1,2,3)))).conversion21()
             == Singleton(SEql(4)))
    assert(And(Singleton(SMember(1,2,3,4)),Not(Singleton(SMember(1,2,3,4,5,6)))).conversion21()
             == Singleton(SEmpty))
  }
}