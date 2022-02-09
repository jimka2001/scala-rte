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

import adjuvant.Adjuvant.{eql,fixedPoint}
import genus._
import org.scalatest.funsuite.AnyFunSuite
import rte.RteImplicits._


//noinspection RedundantDefaultArgument
class AndTestSuite extends AnyFunSuite {
  test("implicits") {
    assert(And(SEql(1)) == And(Singleton(SEql(1))))
  }

  test("and test 31") {
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
  test("canonicalize and 29a") {
    val I = Singleton(SInt)
    val S = Singleton(SAtomic(classOf[String]))
    //val X = Singleton(SEql(-1))
    //val Y = Singleton(SEql(1))
    val ε = EmptyWord

    val rte4 = Or(And(Or(Cat(Star(S), I), ε),
                      Cat(Star(S), I)),
                  And(Or(Cat(Star(S), I), ε),
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
  test("canonicalize and 29b") {
    // And(Or(Cat((<String>)*,<Int?>),ε),
    //     Not(Cat((<String>)*,<[= -1]>)),
    //     Not(Cat((<String>)*,<[= 1]>)),
    //     Not((<String>)*))
    val I = Singleton(SInt)
    val S = Singleton(SAtomic(classOf[String]))
    val X = Singleton(SEql(-1))
    val Y = Singleton(SEql(1))
    val rte1 = And(Or(Cat(Star(S), I), EmptyWord),
                   Not(Cat(Star(S), X)),
                   Not(Cat(Star(S), Y)),
                   Not(Star(S)))
    val rte2 = rte1.canonicalizeOnce
    val rte3 = rte2.canonicalizeOnce
    val rte4 = rte3.canonicalizeOnce
    val rte5 = rte4.canonicalizeOnce
    rte5.canonicalize
  }
  test("canonicalize and 31") {
    assert(And(Singleton(SEql(0)), Not(Cat(Sigma, Star(Sigma)))).canonicalize != Singleton(SEql(0)))
    assert(And(Singleton(SEql(0)), Not(Cat(Sigma, Star(Sigma)))).canonicalize ~= EmptySet)
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
  test("and 114") {
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

    //val Z = Singleton(SAtomic(classOf[TraitX]))

    assert(And(A, Or(X, Y)).conversionA10() == Or(And(A, X), And(A, Y)))
    assert(And(A, Or(X, Y), B).conversionA10() == Or(And(A, X, B), And(A, Y, B)))
  }
  test("canonicalize and") {
    assert(And(EmptySet, EmptySet).canonicalize == EmptySet)
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
             s"\nr1=$r1  \nr2=$r2  \nr3=$r3" +
               s"\ncanonicalized: \n  r1=${r1.canonicalize}\n  r2=${r2.canonicalize}\n  r3=${r3.canonicalize}")
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
  test("and 149") {
    val r1 = Singleton(SEql(1))
    val r2 = Singleton(SEql(2))
    val r3 = Singleton(SEql(3))
    val r4 = Singleton(SEql(4))

    val rte0 = And(r1, Or(r2, r3), r4)
    val rte1 = rte0.canonicalizeOnce
    rte1.canonicalizeOnce
    //val rte3 = rte2.canonicalizeOnce

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
             "\n And(r1,Or(r2,r3),r4)=" + And(r1, Or(r2, r3), r4).canonicalize +
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
    assert(And(Cat(Sigma, Star(Sigma)), Sigma).canonicalize
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
               "\n  Or(And(r1,r2,r4), And(r1,r3,r4))= " + Or(And(r1, r2, r4),
                                                             And(r1, r3, r4)).canonicalize +
               "\n  And(r1, Or(r2, r3), r4)= " + And(r1, Or(r2, r3), r4).canonicalize
             )
    }
  }

  test("discovered case 258") {
    // r1=<STop>
    // r2=Or(Cat(Σ,Σ,(Σ)*),ε)
    //      // r3=Not(<{a,b,c}>)
    // r4=(<[= -1]>)*
    val r1 = Singleton(STop)
    val r2 = Or(Cat(Sigma, Sigma, Star(Sigma)), EmptyWord)
    val r3 = Not(Singleton(SMember("a", "b", "c")))
    // Or( Cat(Σ,Σ,(Σ)*), Not(<{a,b,c}>))
    //   --> Not(<{a,b,c}>)
    assert(Or(Cat(Sigma, Sigma, Star(Sigma)),r3).canonicalizeOnce
             == r3)

    val r4 = Star(Singleton(SEql(-1)))
    val r5a = And(r1, Or(r2, r3).canonicalize, r4)
    val r5b = And(r1, Or(r2, r3), r4)
    // Or(Or(Cat(Σ,Σ,(Σ)*),ε),Not(<{a,b,c}>))
    assert(Or(r2, r3).canonicalize == Not(Singleton(SMember("a", "b", "c"))))
    val r5bc = r5b.canonicalize
    //val dfa5a = r5a.toDfa(true)
    //val dfa5ac = r5a.canonicalize.toDfa(true)
    val dfa5b = r5b.toDfa(true)
    val dfa5bc = r5bc.toDfa(true)
    for{ v <- List("a","b","c","d",0,-1)}
      {
        println(s"testing with $v")
        //assert(dfa5a.simulate(Seq(v)) == dfa5ac.simulate(Seq(v)))
        assert(dfa5b.simulate(Seq(v)) == dfa5bc.simulate(Seq(v)),
               s"\nlhs=$r5b" +
                 s"\nrhs=${r5b.canonicalize}" +
                 s"\n  failed on v=$v"
               )
        //assert(dfa5a.simulate(Seq(v)) == dfa5b.simulate(Seq(v)))
        //assert(dfa5ac.simulate(Seq(v)) == dfa5bc.simulate(Seq(v)))
      }
    assert(r5a.canonicalize ~= r5b.canonicalize,
           s"\nr1=$r1" +
             s"\nr2=$r2" +
             s"\nr3=$r3" +
             s"\nr4=$r4" +
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

  test("discovered case 285"){
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
      r13.canonicalizeOnce
      // val r15= r14.canonicalizeOnce

      assert(And(r1,Or(r2,r3).canonicalize,r4).canonicalize
               ~= And(r1,Or(r2,r3),r4).canonicalize,
             "\nlhs = " + And(r1,Or(r2,r3).canonicalize,r4).canonicalize +
               "\nrhs = " + And(r1,Or(r2,r3),r4).canonicalize)
    }
  }
  test("discovered case 309"){
    locally {
      val r1 = And(Star(Sigma), Or(Star(Sigma), Star(Singleton(SEql(1)))), Sigma)
      val r2 = r1.canonicalize
      assert(r2.canonicalize == Sigma)
    }
  }
  test("discovered case 316"){
    assert(And(Star(Sigma),Star(Sigma),Sigma).canonicalize == Sigma)
    assert(And(Star(Sigma),Or(Star(Sigma),Star(Sigma)),Sigma).canonicalize == Sigma)
    assert(And(Singleton(STop),Not(Singleton(SMember(4,5,6)))).canonicalize
             != Not(Singleton(SMember(4,5,6))))
  }
  test("canonicalize and 253") {
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
  test("discovered case 403"){
    val r1 = SAnd(SMember(1L, 2, 3L, 4),
                  SMember(1,2,3,4))
    assert(eql(SNot(r1).canonicalize(),SNot(SMember(2,4))))
    assert(eql(SAnd(r1,SNot(r1)).canonicalize(), SEmpty))
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
             s"\n  r1=$r1\n empty=${And(r1, Not(r1)).canonicalize}")
      assert(And(r1, r2, Not(r1)).canonicalize ~= EmptySet,
             s"And(r1, r2, Not(r1))" +
             s"\n  r1=$r1\n  r2=$r2 ")
      assert(And(r1, r2, Not(r1), r3).canonicalize ~= EmptySet,
             s"\n  r1=$r1\n  r2=$r2   r3=$r3")
      assert(And(r1, Not(r1), r3).canonicalize ~= EmptySet,
             s"\n  r1=$r1\n  r3=$r3")
      assert(And(r1, trd1, r2, trd2, r3).canonicalize ~= EmptySet,
             s"\n  r1=$r1\n  r2=$r2\n  r3=$r3")
    }
  }

  test("discovered case 432"){
    val r1 = Star(Not(Sigma))
    val r2 = Not(Sigma)
    assert(! (r1 ~= EmptyWord))
    assert( r1 ~= r2)
  }
  test("and conversion3"){
    assert(And(And(),EmptySet,And()).conversionC3() == EmptySet)
  }

  test("and conversion4"){
    assert(And(Or(),And(),And(),Or()).conversionC4() == And(And(), Or()))
  }

  test("and conversion5"){
    assert(And(Singleton(SEql(2)),Singleton(SEql(1))).conversionC5()
             == And(Singleton(SEql(1)),Singleton(SEql(2))))
    assert(And(Singleton(SEql(1)),Singleton(SEql(2))).conversionC5()
             == And(Singleton(SEql(1)),Singleton(SEql(2))))
  }

  test("and conversion6"){
    // remove Sigma* and flatten And(And(...)...)
    assert(And(Star(Sigma),
               And(Singleton(SEql(1)),Singleton(SEql(2))),
               And(Singleton(SEql(3)),Singleton(SEql(4))),
               Star(Sigma)).conversionC6()
      == And(Singleton(SEql(1)),Singleton(SEql(2)),
             Singleton(SEql(3)),Singleton(SEql(4))))
  }

  test("and conversionA7"){
    assert(And(EmptyWord,Singleton(SEql(1))).conversionA7()
           == EmptySet)
  }
  
  test("and conversion8"){
    // if operands contains EmptyWord, then the intersection is either EmptyWord or EmptySet
    assert(And(EmptyWord,Star(Singleton(SEql(1)))).conversionA8()
             == EmptyWord)
    assert(And(EmptyWord,Singleton(SEql(1))).conversionA8()
             == EmptySet)
  }
  test("and conversion9"){
    val x = Singleton(SEql(1))
    val y = Singleton(SEql(1))
    // if x matches only singleton then And(x,y*) -> And(x,y)
    assert(And(x,Star(y)).conversionA9()
             == And(x,y))
    assert(And(Star(x),y).conversionA9()
             == And(x,y))
    assert(And(Star(x),Star(y)).conversionA9()
             == And(Star(x),Star(y)))
    assert(And(x,y).conversionA9()
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
    assert(And(a,b,Or(x,y,z),c,d).conversionA10()
           == Or(And(a,b,x,c,d),
                 And(a,b,y,c,d),
                 And(a,b,z,c,d)))
  }
  test("and conversion11"){
    // And(...,x,Not(x)...)
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    assert(And(a,b,Not(a)).conversionC11()
             == EmptySet)
    assert(And(a,Not(b),Not(Not(b))).conversionC11()
             == EmptySet)
  }
  test("and conversionA18"){
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    assert(And(Singleton(SEmpty),a,b).conversionA18()
             == EmptySet)
  }
  test("and conversion13"){
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    assert(And(Sigma,Singleton(SEmpty),a,b).conversionA13()
             == And(Singleton(SEmpty),a,b))
    assert(And(Sigma,Singleton(SEmpty),Singleton(SEmpty)).conversionA13()
             == And(Sigma,Singleton(SEmpty),Singleton(SEmpty)))
  }
  test("and conversion21"){
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    val ab = Singleton(SMember("a","b"))
    // detect intersection of disjoint
    assert(And(a,b).conversionC21()
           == EmptySet)
    assert(And(a,ab).conversionC21()
           == And(a,ab))
    assert(Or(a,b).conversionC21()
           == Or(a,b))
    assert(Or(Not(a),Not(b)).conversionC21()
           == Star(Sigma))
    assert(Or(a,ab).conversionC21()
           == Or(a,ab))

  }
  test("and conversionC16"){
    // test And(<STop>,Not(<{4,5,6}>)) does not reduce to Not(<{4,5,6}>)
    // detect supertype

    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    val ab = Singleton(SMember("a","b"))
    val ac = Singleton(SMember("a","c"))
    assert(And(a,b,ab,ac).conversionC16()
           == And(a,b))

    assert(And(ab,Not(a)).conversionC16()
           == And(ab,Not(a)))



    assert(And(Singleton(SAtomic(classOf[Any])),Not(ab)).conversionC16()
           == And(Singleton(SAtomic(classOf[Any])),Not(ab)))
    assert(And(Singleton(SAtomic(classOf[Any])),ab).conversionC16()
           == ab)
  }
  test("and conversionC16b"){
    // test And(<STop>,Not(<{4,5,6}>)) does not reduce to Not(<{4,5,6}>)
    // detect supertype

    // val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    val ab = Singleton(SMember("a","b"))
    val ac = Singleton(SMember("a","c"))


    // And(b,Not(ac)) => And(b)
    assert(And(b,Not(ac)).conversionD16b()
           == b)

    assert(And(Singleton(SAtomic(classOf[Any])),Not(ab)).conversionD16b()
           == And(Singleton(SAtomic(classOf[Any])),Not(ab)))
  }
  test("and conversion17"){
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    assert(And(Sigma,Cat(a,b)).conversionA17()
             == EmptySet)
    assert(And(Sigma,Cat(Star(a),b)).conversionA17()
             == And(Sigma,Cat(Star(a),b)))
    assert(And(Sigma,Cat(a,Star(a),b)).conversionA17()
             == EmptySet)

    assert(And(a,Cat(a,b)).conversionA17()
             == EmptySet)
    assert(And(a,Cat(Star(a),b)).conversionA17()
             == And(a,Cat(Star(a),b)))
    assert(And(a,Cat(a,Star(a),b)).conversionA17()
             == EmptySet)
  }
  test("and conversion17a"){
    val a = Singleton(SEql("a"))
    // val b = Singleton(SEql("b"))
    // considering only the Cat's with no nullables,
    //    they must all have the same number of operands
    assert(And(Cat(a,a),Cat(a,a,a)).conversionA17a()
             == EmptySet)
    assert(And(Cat(a,a,a),Cat(a,a,a)).conversionA17a()
             != EmptySet)
    assert(And(Cat(a,a),Cat(a,a),Cat(a,a),Cat(a,a,a)).conversionA17a()
             == EmptySet)

    assert(And(Cat(a,a),Cat(a,a,Star(a))).conversionA17a()
             != EmptySet)
    assert(And(Cat(Sigma, Sigma, Star(Sigma)),
               Cat(Cat(Sigma, Star(Sigma)))).conversionA17a() != EmptySet)
    assert(And(Cat(Sigma, Sigma, Sigma),
               Cat(Cat(Sigma, Star(Sigma)))).conversionA17a() != EmptySet)
  }
  test("and conversion17a2"){
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    // considering only the Cat's with no nullables,
    //    they must all have the same number of operands

    assert(And(Cat(a,b),
               Cat(b,a),
               Star(b)).conversionA17a2()
             == And(Cat(And(a,b),And(b,a)),
                    Star(b)))

    assert(And(Cat(a,b,b,a),
               Cat(b,a,b,b),
               Star(b)).conversionA17a2()
             == And(Cat(And(a,b),
                        And(b,a),
                        And(b,b),
                        And(a,b)),
                    Star(b)))
    assert(And(Cat(Sigma, Cat(Sigma, Sigma)),
               Cat(Cat(Sigma,Sigma),Sigma)
               ).conversionA17a2() != EmptySet)
  }
  test("and conversion17b"){
    // if there is a Cat having no nullables,
    //    then every cat must have that many non-nullables or
    //    or fewer plus a nullable
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))

    assert(And(Cat(a,a),Cat(a,a,a,Star(a))).conversionA17b()
             == EmptySet)

    assert(And(Cat(a,a),Cat(a,a,Star(a))).conversionA17b()
             != EmptySet)
    assert(And(Cat(a,a),Cat(a,Star(b),a,Star(a))).conversionA17b()
             != EmptySet)
    assert(And(Cat(Sigma, Sigma, Star(Sigma)),
               Cat(Cat(Sigma, Star(Sigma)))).conversionA17b() != EmptySet)
    assert(And(Cat(Sigma, Sigma, Sigma),
               Cat(Cat(Sigma, Star(Sigma)))).conversionA17b() != EmptySet)
  }
  test("and conversion17c"){
    // if And(...) contains a Cat with no nullables,
    //  then remove the nullables from ever other Cat with that many non-nullables.
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    val c = Singleton(SEql("c"))
    assert(And(Cat(a,b,Sigma),
               Cat(Star(c),a,Star(c),b,Star(Sigma),c)).conversionA17c()
             == And(Cat(a,b,Sigma),Cat(a,b,c)))
    assert(And(Cat(a,b,Star(Sigma)),
               Cat(Star(c),a,Star(c),b,Star(Sigma),c)).conversionA17c()
             == And(Cat(a,b,Star(Sigma)),
                    Cat(Star(c),a,Star(c),b,Star(Sigma),c)))

    assert(And(Cat(a,b,c),
               Cat(a,Star(b)),
               Cat(a,b,Star(c)),
               Cat(Star(c),a,Star(c),b,Star(Sigma),c),
               Cat(a,a,a,a,Star(b))).conversionA17c()
             == And(Cat(a,b,c),
                    Cat(a,Star(b)),
                    Cat(a,b,Star(c)),
                    Cat(a,b,c),
                    Cat(a,a,a,a)))

    // e.g. And( Cat(Sigma,Star(Sigma)), Sigma,...) --> And(Sigma,...)
    val ab = Singleton(SMember("a","b"))
    assert(And(Cat(Sigma,Star(Sigma)),Sigma).conversionA17c()
             == And(Sigma,Sigma))
    assert(And(ab,Cat(Sigma,Star(Sigma)),Sigma).conversionA17c()
             == And(ab,Sigma,Sigma))

    val problematic = And(Cat(Sigma, Sigma, Sigma),
                          Cat(Sigma, Sigma, Star(Sigma),
                              Cat(Sigma, Star(Sigma))))
    assert(problematic.conversionA17c() == problematic)
  }
  test("and conversion18"){
    // if And(...) contains a singleton which is non inhabited
    assert(And(Singleton(SAnd(SEql(1),SEql(2)))).conversionA18()
           == EmptySet)
  }
  test("and conversionA19"){
    val ab = Singleton(SMember("a","b"))
    val ac = Singleton(SMember("a","c"))
    val bc = Singleton(SMember("b","c"))
    assert(And(ab,ac,bc).conversionA19()
           == EmptySet)
  }

  test("and conversionC17"){
    assert(And(Singleton(SMember(1,2,3,4)),Not(Singleton(SMember(1,2)))).conversionC17()
             == And(Singleton(SMember(3,4)),Not(Singleton(SMember(1,2)))))
    assert(And(Singleton(SMember(1,2,3,4)),Not(Singleton(SMember(1,2,3)))).conversionC17()
             == And(Singleton(SEql(4)),Not(Singleton(SMember(1,2,3)))))
    assert(And(Singleton(SMember(1,2,3,4)),Not(Singleton(SMember(1,2,3,4,5,6)))).conversionC17()
             == And(Singleton(SEmpty),Not(Singleton(SMember(1,2,3,4,5,6)))))

    assert(And(Singleton(SMember(1,2,3,"a","b","c")),
               Not(Singleton(SInt))).conversionC17()
             == And(Singleton(SMember("a","b","c")),
                    Not(Singleton(SInt))))
    assert(And(Singleton(SMember(1,2,3,"a","b","c")),
               Not(Cat(Singleton(SInt)))).conversionC17()
             == And(Singleton(SMember(1,2,3,"a","b","c")),
                    Not(Cat(Singleton(SInt)))))
    assert(And(Singleton(SMember(1,2,3,"a","b","c")),
               Or(Cat(Singleton(SEql(1))),
                  Cat(Singleton(SEql("a"))))).conversionC17()
             == And(Singleton(SMember(1,2,3,"a","b","c")),
                    Or(Cat(Singleton(SEql(1))),
                       Cat(Singleton(SEql("a"))))))
    assert(And(Singleton(SMember(1,2,3,"a","b","c")),
               Cat(Singleton(SEql("a"))),
               Singleton(SEmpty)).conversionC17()
           == And(Singleton(SEmpty),
                  Cat(Singleton(SEql("a"))),
                  Singleton(SEmpty)))
    assert(And(Singleton(SEmpty),
               Cat(Singleton(SEql("a")),EmptySet))
           == And(Singleton(SEmpty),
                  Cat(Singleton(SEql("a")),EmptySet)))
    val s1 = Singleton(SSatisfies(_=>true,"satisfies"))
    assert(And(Singleton(SMember(1,2,3,"a","b","c")),
               Cat(Singleton(SEql("a")),
                   s1)).conversionC17()
             == And(Singleton(SMember(1,2,3,"a","b","c")),
                    Cat(Singleton(SEql("a")),
                        s1)))
    assert(And(Singleton(SMember(1,2,3,"a","b","c")),
               Singleton(SEql("a")),
               Cat(s1)).conversionC17()
             == And(Singleton(SEql("a")),
                    Singleton(SEql("a")),
                    Cat(s1)))

    val s2 = Singleton(SSatisfies(_=>false,"satisfies"))
    assert(And(Singleton(SMember(1,2,3,"a","b","c")),
               Cat(Singleton(SEql("a")),
                   s2)).conversionC17()
             == And(Singleton(SMember(1,2,3,"a","b","c")),
                    Cat(Singleton(SEql("a")),
                        s2)))
  }
  test("test_discovered_785") {
    val problematic:Rte = And(Cat(Sigma, Sigma, Star(Sigma)),
                                 Cat(Cat(Sigma, Star(Sigma))))

    def good_enough(a: Rte, b: Rte): Boolean = {
      a.getClass == b.getClass && a == b
    }

    def try_example(rt: Rte, seq: Seq[Int]): Boolean = {
      val expecting = problematic.simulate(true, seq)
      val got = rt.simulate(true, seq)
      if (got != expecting) {
        print(s" seq= $seq")
        print(s"  rt=$rt")
        print(s" expecting = $expecting")
        print(s"       got = $got")
      }
      got == expecting
    }

    def invariant(rt:Rte):Boolean = {
      val sequences:Seq[Seq[Int]] = Seq(Seq(),
                                        Seq(1),
                                        Seq(1,1),
                                        Seq(1,1,1))
      sequences.forall(seq => try_example(rt,seq))
    }
    def f(r:Rte):Rte = {
      r.canonicalizeOnce
    }

    fixedPoint[Rte](problematic,
                    f,
                    good_enough,
                    invariant _)
  }
}
