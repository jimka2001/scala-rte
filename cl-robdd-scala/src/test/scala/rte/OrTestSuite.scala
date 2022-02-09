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
import rte.RteImplicits._

class OrTestSuite extends AnyFunSuite {

  test("canonicalize or 31") {
    class TestSup
    class TestSub extends TestSup
    class TestD1 // disjoint from TestD2
    class TestD2 // disjoint from TestD1
    val trd1 = Singleton(genus.SAtomic(classOf[TestD1]))
    val r1 = Star(trd1)
    val r2 = EmptyWord
    val r3 = trd1 // Not(Not(trd1))
    assert(Or(EmptyWord, Cat(r2, r3, Star(Cat(r2, r3)))).canonicalize
             == Or(EmptyWord, Star(Cat(r2, r3))).canonicalize)
    assert(Or(EmptyWord, Cat(r1, Star(Cat(r1)))).canonicalize
             == Or(EmptyWord, Star(Cat(r1))).canonicalize)

    assert(Or(EmptyWord, Cat(r1, r3, Star(Cat(r1, r3)))).canonicalize
             == Or(EmptyWord, Star(Cat(r1, r3))).canonicalize)

    assert(Or(EmptyWord, Cat(r1, r2, r3, Star(Cat(r1, r2, r3)))).canonicalize
             == Or(EmptyWord, Star(Cat(r1, r2, r3))).canonicalize)
  }

  test("canonicalize or 58") {
    val r1 = Not(Singleton(SAtomic(classOf[java.lang.Integer])))
    abstract class Test1
    val r2 = Singleton(SAtomic(classOf[Test1]))
    val r3 = Not(Singleton(SEql(0)))
    val r4 = Not(Singleton(SEql(1)))
    assert(Or(r1, r2, r3, r4).canonicalize ~= Or(r1, Or(r2, r3), r4).canonicalize)
  }

  test("canonicalize or a") {
    assert(Or(EmptySet, EmptySet).canonicalize == EmptySet)
    assert(Or().canonicalize == EmptySet)
    class TestSup
    class TestSub extends TestSup
    class TestD1 // disjoint from TestD2
    class TestD2 // disjoint from TestD1

    val trd1 = Singleton(genus.SAtomic(classOf[TestD1]))
    val trd2 = Singleton(genus.SAtomic(classOf[TestD2]))

    assert(Rte.sigmaStar == Or(Sigma,
                               Star(Cat(Sigma, Star(Sigma)))).canonicalize)
    assert(Rte.sigmaStar == Or(Sigma,
                               Star(trd2),
                               Star(Cat(Sigma, Star(Sigma)))).canonicalize)
    // Star(Σ) did not equal Or(<Trait3$1>,Σ,Star(<[Member 1,2,3,4]>),Star(Cat(Σ,Star(Σ))))
    assert(Rte.sigmaStar == Or(trd1,
                               Sigma,
                               Star(trd2),
                               Star(Cat(Sigma, Star(Sigma)))).canonicalize)

    // Or(A,ε,<java.lang.String>,<java.lang.Integer>) did not equal Or(A,<java.lang.String>,ε,<java.lang.Integer>)
    assert(Or().canonicalize
             == Or().canonicalize)
  }
  test("avoid infinite loop a") {
    val number = SAtomic(classOf[Number])

    Cat(Star(Sigma),
        number,
        Singleton(SEql(-1))).derivative1(Some(number))

  }
  test("avoid infinite loop b") {
    // r1=Cat(Cat(Σ,(Σ)*),Cat(<Integer>,<{a,b,c}>))
    // r2=And((<Number>)*,Or(<Trait3$1>,<[= -1]>))
    val number = Singleton(SAtomic(classOf[Number]))

    trait Trait2
    trait Trait3 extends Trait2
    val r3 = Or(SMember(1, 2, 3, 4),
                Cat(Not(SEql(-1)),
                    number,
                    SEql(-1)))
    r3.canonicalizeOnce

  }
  test("canonicalize or b1") {
    for {depth <- 2 to 2
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         } {
      try {
        Or(r1, r2).canonicalize
      }
      catch {
        case e: StackOverflowError =>
          println(s"r1=$r1")
          println(s"r2=$r2")
          throw e
      }
    }
  }

  test("canonicalize or b2") {

    for {depth <- 0 to 5
         _ <- 1 to 5000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         r4 = Rte.randomRte(depth)
         } {

      // Or(Or(x,y),b) --> Or(x,y,b)
      assert(Or(Or(r2, r3), r4).canonicalize ~= Or(r2, r3, r4).canonicalize,
             s"\n   r2=$r2 \n   r3=$r3 \n   r4=$r4" +
               s"\n   Or(Or(r2,r3),r4)= " + Or(Or(r2, r3), r4).canonicalize +
               s"\n   Or(r2,r3,r4)=     " + Or(r2, r3, r4))

      // Or(a,Or(x,y),b) --> Or(a,x,y,b)
      assert(Or(r1, Or(r2, r3), r4).canonicalize ~= Or(r1, r2, r3, r4).canonicalize,
             s"\n   r1=$r1 \n   r2=$r2 \n   r3=$r3 \n   r4=$r4" +
               s"\n   Or(r1,Or(r2,r3),r4)= " + Or(r1, Or(r2, r3), r4).canonicalize +
               s"\n   Or(r1,r2,r3,r4)=     " + Or(r1, r2, r3, r4))

      // (:or A B (:* B) C)
      // --> (:or A (:* B) C)

      assert(Or(EmptyWord, Cat(r3, Star(r3))).canonicalize
               ~= Or(EmptyWord, Star(r3)).canonicalize,
             "" + Or(EmptyWord, Cat(r3, Star(r3))).canonicalize
               + " not isomorphic with "
               + Or(EmptyWord, Star(r3)).canonicalize)
    }
  }

  test("canonicalize or c") {
    class TestSup
    class TestSub extends TestSup
    val trsup = Singleton(genus.SAtomic(classOf[TestSup]))
    val trsub = Singleton(genus.SAtomic(classOf[TestSub]))

    for {depth <- 0 to 5
         _ <- 1 to 500
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         r4 = Rte.randomRte(depth)
         } {

      // Or(:epsilon,Cat(X,Y,Z,Star(Cat(X,Y,Z))))
      //  --> Or(:epsilon,Star(Cat(X,Y,Z)))
      locally {

        assert(Or(EmptyWord, Cat(r1, r2, r3, Star(Cat(r1, r2, r3)))).canonicalize
                 ~= Or(EmptyWord, Star(Cat(r1, r2, r3))).canonicalize,
               s"r1=$r1 r2=$r2  r3=$r3")
      }
      // Or(Star(A),Cat(X,Y,Z,Star(Cat(X,Y,Z))))
      //  --> Or(Star(A),Star(Cat(X,Y,Z)))
      assert(Or(Star(r4), Cat(r1, r2, r3, Star(Cat(r1, r2, r3)))).canonicalize
               ~= Or(Star(r4), Star(Cat(r1, r2, r3))).canonicalize,
             s"r1=$r1 r2=$r2  r3=$r3  r4=$r4")

      // Expected :Star(Σ)
      // Actual   :Or(ε,Cat(Star(Σ),Σ,Star(Σ)))

      //     Star(And(A,
      //              Or(A,<java.lang.Integer>),
      //              Or(Star(<[Member 4,5,6]>),Not(B))))
      // --> Star(Or(And(A,Star(<[Member 4,5,6]>)),
      //             And(A,Not(B))))

      // (:or A :epsilon B (:cat X (:* X)) C)
      //   --> (:or A :epsilon B (:* X) C )
      assert(Or(r1, EmptyWord, r2, Cat(r3, Star(r3)), r4).canonicalize
               ~= Or(r1, EmptyWord, r2, Star(r3), r4).canonicalize,
             "" + Or(r1, EmptyWord, r2, Cat(r3, Star(r3)), r4).canonicalize
               + " not isomorphic with "
               + Or(r1, EmptyWord, r2, Star(r3), r4).canonicalize)
      assert(Or(r1, EmptyWord, r2, r3.+, r4).canonicalize
               ~= Or(r1, EmptyWord, r2, Star(r3), r4).canonicalize)

      // (:or A :epsilon B (:* X) C)
      //   --> (:or A B (:* X) C)
      assert(Or(r1, EmptyWord, r2, Star(r3), r4).canonicalize ~=
               Or(r1, r2, Star(r3), r4).canonicalize)

      // remove subset
      assert(Or(r1, trsub, r2, trsup, r3).canonicalize ~=
               Or(r1, r2, trsup, r3).canonicalize)
    }
  }

  test("or 415") {
    assert(Or(EmptyWord, Cat(Star(Sigma), Sigma, Star(Sigma))).canonicalize
             == Star(Sigma))
  }
  test("canonicalize or 315") {
    assert(Or(Not(Singleton(SEql(0))),
              Star(Singleton(SEql(1)))).canonicalize == Not(Singleton(SEql(0))))
  }
  test("or conversionC16") {
    // detect and remove subtype

    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    val ab = Singleton(SMember("a", "b"))
    val ac = Singleton(SMember("a", "c"))
    assert(Or(a, b, ab, ac).conversionC16()
             == Or(ab, ac))

    assert(Or(ab, Not(a)).conversionC16()
             == Or(ab, Not(a)))

    // Or(b,Not(ac)) => Or(b,Sigma)
    assert(Or(b, Not(ac)).conversionC16()
             == Or(b, Not(ac)))

    assert(Or(Singleton(SAtomic(classOf[Any])), ab).conversionC16()
             == Singleton(SAtomic(classOf[Any])))
  }
  test("or conversionC16b") {
    // detect Or(a,Not(b)) with a disjoint from b, remove a

    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    val ab = Singleton(SMember("a", "b"))
    val ac = Singleton(SMember("a", "c"))


    assert(Or(ab, Not(a)).conversionD16b()
             == Or(ab, Not(a)))

    // Or(b,Not(ac)) => Or(Not(ac))
    assert(Or(b, Not(ac)).conversionD16b()
             == Not(ac))

    assert(Or(Singleton(SAtomic(classOf[Any])), Not(ab)).conversionD16b()
             == Or(Singleton(SAtomic(classOf[Any])), Not(ab)))

  }
  test("canonicalize or 203") {
    abstract class Abstract2
    val r1 = Singleton(SAtomic(classOf[Abstract2]))
    val r2 = And(Sigma, Singleton(SMember(4, 5, 6)))
    val r3 = Not(r1)
    val r4 = Star(Singleton(SAtomic(classOf[java.lang.String])))
    assert(Or(r1, r3, r4).canonicalize != r3)
    assert(Or(r1, r3, r4).canonicalize == Rte.sigmaStar)
    assert(Or(Singleton(SAtomic(classOf[java.lang.Integer])),
              Not(Singleton(SEql(1))),
              r1).canonicalize
             == Rte.sigmaStar)
    assert(Or(r1, And(r2, r3)).canonicalize ~= And(Or(r1, r2),
                                                   Or(r1, r3)).canonicalize)
    assert(Or(And(r2, r3), r4).canonicalize ~= And(Or(r2, r4),
                                                   Or(r3, r4)).canonicalize)

    assert(Or(r1, And(r2, r3), r4).canonicalize ~= And(Or(r1, r2, r4),
                                                       Or(r1, r3, r4)).canonicalize,
           s"\nr1=$r1" +
             s"\nr2=$r2" +
             s"\nr3=$r3" +
             s"\nr4=$r4" +
             s"\n And(r2,r3) = And($r1,$r2) = " + And(r1, r2).canonicalize +
             s"\n Or(r1, r2, r4)= Or($r1, $r2, $r4)= " + Or(r1, r2, r4).canonicalize +
             s"\n Or(r1, r3, r4)= Or($r1, $r3, $r4)= " + Or(r1, r3, r4).canonicalize +
             "\n Or(r1, And(r2, r3), r4)= " + Or(r1, And(r2, r3), r4).canonicalize +
             "\n And(Or(r1, r2, r4), Or(r1, r3, r4))= " + And(Or(r1, r2, r4),
                                                              Or(r1, r3, r4)).canonicalize
           )
  }

  test("canonicalize or 204") {
    for {depth <- 0 to 4
         _ <- 1 to 100
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         r4 = Rte.randomRte(depth)
         } {
      // And(a,Or(x,y),b) --> Or(And(a,x,b),And(a,y,b))
      if (!Or(r1, And(r2, r3), r4).canonicalize ~= And(Or(r1, r2, r4),
                                                       Or(r1, r3, r4)).canonicalize)
        fail(
          s"\nr1=$r1  \nr2=$r2  \nr3=$r3  \nr4=$r4" +
            s"\n  canonicalized: r1=${r1.canonicalize}  r2=${r2.canonicalize}" +
            s"\n                 r3=${r3.canonicalize}  r4=${r4.canonicalize}" +
            "\n Or(r1,r2,r4)=" + Or(r1, r2, r4).canonicalize +
            "\n Or(r1,r3,r4)=" + Or(r1, r3, r4).canonicalize
          )
    }
  }
  test("discovered case 279") {
    val s = Singleton(SEql(-1))
    val c = Cat(Sigma, Sigma, Star(Sigma))
    val r1 = Or(And(c, s), s)
    r1.canonicalize
  }

  test("discovered case 286") {
    // r1= <[= 1]>
    // r2= Or(Cat(And(<Trait3X>,<[= true]>),Not(<Integer>)),Or(Or(Cat(Σ,Σ,(Σ)*),ε),ε))
    // r3= <STop>
    // r4= And(((<[= false]>)*)*,ε)
    val r1 = Singleton(SEql(1))
    val ra = Cat(And(Singleton(SAtomic(classOf[RandomType.Trait3X])),
                     Singleton(SEql(true))),
                 Not(Singleton(SAtomic(classOf[Integer]))))
    val r2 = Or(ra,
                Or(Or(Cat(Sigma, Sigma, Star(Sigma)), EmptyWord), EmptyWord))
    val r3 = Singleton(STop)
    val r4 = And(Star(Star(Singleton(SEql(false)))), EmptyWord)

    ///And(r2,r3).canonicalizeDebug(10,List("a",1,0,true,false))

    val lhs = Or(r1, And(r2, r3).canonicalize, r4)
    val rhs = Or(r1, And(r2, r3), r4)
    //println(s"lhs=$lhs")
    //println(s"rhs=$rhs")
    // lhs.canonicalizeDebug(10,List("a",1,0,true,false))

    //rhs.canonicalizeDebug(10,List("a",1,0,true,false))

    assert(lhs.canonicalize ~=
             rhs.canonicalize)
  }

  test("canonicalize or 253") {
    for {depth <- 0 to 4
         _ <- 1 to 500
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         r4 = Rte.randomRte(depth)
         } {
      assert(Or(r1, And(r2, r3).canonicalize, r4).canonicalize ~=
               Or(r1, And(r2, r3), r4).canonicalize,
             s"\nr1= $r1" +
               s"\nr2= $r2" +
               s"\nr3= $r3" +
               s"\nr4= $r4" +
               s"\ncanonicalized" +
               s"\n   r1= ${r1.canonicalize}" +
               s"\n   r2= ${r2.canonicalize}" +
               s"\n   r3= ${r3.canonicalize}" +
               s"\n   r4= ${r4.canonicalize}" +
               s"\n   Or(r2,r3) = ${Or(r2, r3).canonicalize}" +
               s"\n   r4= ${r4.canonicalize}" +
               s"\nlhs = " + Or(r1, And(r2, r3).canonicalize, r4).canonicalize +
               s"\nrhs = " + Or(r1, And(r2, r3), r4).canonicalize
             )
    }
  }

  test("or conversion3") {
    // Or(... Sigma* ....) -> Sigma*
    assert(Or(Or(), Star(Sigma), Or()).conversionC3() == Star(Sigma))
    assert(Or(Or(), Star(Sigma), Star(Sigma), Or()).conversionC3() == Star(Sigma))
    assert(Or(Or(), Or()).conversionC3() != Star(Sigma))
  }
  test("or conversion4") {
    // uniquify
    assert(Or(Or(), Star(Sigma), Or()).conversionC4() == Or(Star(Sigma), Or()))

    // order is maintained
    assert(Or(Or(), And()).conversionC4() == Or(Or(), And()))
  }
  test("or conversion5") {
    val X = Singleton(SEql(1))
    val Y = Singleton(SEql(2))
    assert(Or(Y, X).conversionC5() == Or(X, Y))
    assert(Or(X, Y).conversionC5() == Or(X, Y))
  }
  test("or conversion6") {
    // remove EmptySet and flatten Or(Or(...)...)
    val X = Singleton(SEql(1))
    val Y = Singleton(SEql(2))
    assert(Or(X).conversionC6() == X)
    assert(Or(Or(X)).conversionC6() == X)
    assert(Or(Or(Or(X))).conversionC6() == Or(X))
    assert(Or(Or(X), Or(X)).conversionC6() == Or(X, X))
    assert(Or(X, Y, EmptySet).conversionC6() == Or(X, Y))
    assert(Or(EmptySet, X, EmptySet, Y, EmptySet).conversionC6() == Or(X, Y))
    assert(Or(X, Y).conversionC6() == Or(X, Y))
    assert(Or(Y, X, Y).conversionC6() == Or(Y, X, Y))
  }
  test("or conversion7") {
    // (:or A B (:* B) C)
    // --> (:or A (:* B) C)
    val A = Singleton(SEql(1))
    val B = Singleton(SEql(2))
    val C = Singleton(SEql(3))
    assert(Or(A, B, Star(B), C).conversionC7() == Or(A, Star(B), C))
    assert(Or(A, Star(C), B, C).conversionC7() == Or(A, Star(C), B))
    assert(Or(A, Star(B), B, C, Star(C)).conversionC7() == Or(A, Star(B), Star(C)))
  }
  test("or conversion8") {
    // (:or (:* C) (:cat X (:* X)))
    //   --> (:or (:* C) (:* X))

    val A = Singleton(SEql(1))
    val B = Singleton(SEql(2))
    val C = Singleton(SEql(3))
    val X = Singleton(SEql(4))

    // (:or A :epsilon B (:cat X (:* X)) C)
    //   --> (:or A :epsilon B (:* X) C )
    assert(Or(A, EmptyWord, B, Cat(X, Star(X)), C).conversionO8(true)
             == Or(A, EmptyWord, B, Star(X), C))

    // (:or :epsilon (:cat X (:* X)))
    //   --> (:or :epsilon (:* X))
    assert(Or(EmptyWord, Cat(X, Star(X))).conversionO8(true)
             == Or(EmptyWord, Star(X)))

    // (:or (:* C) (:cat X (:* X)))
    //   --> (:or (:* C) (:* X))
    assert(Or(Star(C), Cat(X, Star(X))).conversionO8(true)
             == Or(Star(C), Star(X)))

    // (:or (:* C) (:cat X (:* A)))
    //   --> no change
    assert(Or(Star(C), Cat(X, Star(A))).conversionO8(true)
             == Or(Star(C), Cat(X, Star(A))))
  }
  test("or conversion9") {
    val A = Singleton(SEql("A"))
    val B = Singleton(SEql("B"))
    val C = Singleton(SEql("C"))
    val X = Singleton(SEql("X"))
    val Y = Singleton(SEql("Y"))
    val Z = Singleton(SEql("Z"))

    // (:or A :epsilon B (:cat X Y Z (:* (:cat X Y Z))) C)
    //   --> (:or A :epsilon B (:* (:cat X Y Z)) C )
    assert(Or(A, EmptyWord, B, Cat(X, Y, Z, Star(Cat(X, Y, Z))), C).conversionO9(true)
             == Or(A, EmptyWord, B, Star(Cat(X, Y, Z)), C))

    // (:or B* (:cat X Y Z (:* (:cat X Y Z))))
    //   --> (:or B* (:* (:cat X Y Z)))
    assert(Or(Star(B), Cat(X, Y, Z, Star(Cat(X, Y, Z)))).conversionO9(true)
             == Or(Star(B), Star(Cat(X, Y, Z))))
  }
  test("or conversion10") {
    // (:or A :epsilon B (:* X) C)
    //   --> (:or A B (:* X) C)
    val A = Singleton(SEql("A"))
    val B = Singleton(SEql("B"))
    val C = Singleton(SEql("C"))
    val X = Singleton(SEql("X"))
    assert(Or(A, EmptyWord, B, Star(X), C).conversionO10()
             == Or(A, B, Star(X), C))
    assert(Or(A, EmptyWord, B, C).conversionO10()
             == Or(A, EmptyWord, B, C))
  }
  test("or conversion11b") {
    // if Sigma is in the operands, then filter out all singletons
    // Or(Singleton(A),Sigma,...) -> Or(Sigma,...)
    val A = Singleton(SEql("A"))
    val AB = Singleton(SMember("A", "B"))
    val C = Singleton(SEql("C"))
    val X = Star(A)
    assert(Or(A, AB, Sigma, C, X).conversionO11b()
             == Or(Sigma, X))
    assert(Or(A, AB, C, X).conversionO11b()
             == Or(A, AB, C, X))
  }
  test("or conversionC16 b") {
    // filter out singletons which are a subclass of other singletons
    val A = Singleton(SEql("A"))
    val AB = Singleton(SMember("A", "B"))
    val BA = Singleton(SMember("B", "A"))
    val C = Singleton(SEql("C"))
    val X = Singleton(SEql("X"))

    assert(AB == BA)
    assert(Or(A, C).conversionC16()
             == Or(A, C))
    assert(And(A, C).conversionC16()
             == And(A, C))
    assert(Or(A, AB, C, X).conversionC16()
             == Or(AB, C, X))
    assert(Or(C, A, X, AB).conversionC16()
             == Or(C, X, AB))
    assert(Or(AB, C, A, X).conversionC16()
             == Or(AB, C, X))
    assert(Or(AB, BA).conversionC16()
             == EmptySet)
    assert(And(AB, BA).canonicalize == AB)
  }
  test("or conversionC12") {
    // sigmaSigmaStarSigma = Cat(Sigma, Sigma, sigmaStar)
    // Or(   A, B, ... Cat(Sigma,Sigma,Sigma*) ... Not(Singleton(X)) ...)
    //   --> Or( A, B, ... Not(Singleton(X))
    val A = Singleton(SEql("A"))
    val B = Singleton(SEql("B"))
    val X = Singleton(SEql("X"))
    assert(Or(A, Cat(Sigma, Sigma, Star(Sigma)), B, Not(X)).conversionC12()
             == Or(A, B, Not(X)).conversionC12())
    assert(And(A, Cat(Sigma, Sigma, Star(Sigma)), B, Not(X)).conversionC12()
             == And(A, Cat(Sigma, Sigma, Star(Sigma)), B).conversionC12())
  }
  test("combo conversion13") {
    // Or(A,Not(A),X) -> SigmaStar
    val A = Singleton(SEql("A"))
    val B = Singleton(SEql("B"))
    val X = Singleton(SEql("X"))
    assert(Or(A, B, Not(A), X).conversionC11()
             == Star(Sigma))
    assert(Or(A, B, X).conversionC11()
             == Or(A, B, X))
    assert(Or(A, Not(B), X).conversionC11()
             == Or(A, Not(B), X))

    assert(And(A, B, Not(A), X).conversionC11()
             == EmptySet)
    assert(And(A, B, X).conversionC11()
             == And(A, B, X))
    assert(And(A, Not(B), X).conversionC11()
             == And(A, Not(B), X))
  }
  test("combo conversionC14") {
    // Or(A,Not(B),X) -> Sigma* if B is subtype of A
    val A = Singleton(SMember("A", "B"))
    val B = Singleton(SEql("B"))
    val X = Singleton(SEql("X"))
    assert(Or(A, Not(B), X).conversionC14()
             == Star(Sigma))
    assert(Or(Not(A), B, X).conversionC14()
             != Star(Sigma))
    // And(A,Not(B),X) -> EmptySet if A is subtype of B
    assert(And(B, Not(A), X).conversionC14()
             == EmptySet)
    assert(And(Not(B), A, X).conversionC14()
             != EmptySet)

  }
  test("or conversion15") {
    // Or(Not(A),B*) = Not(A) if A and B  disjoint
    //  only works if length = 2
    val A = Singleton(SMember("A", "B"))
    val B = Singleton(SEql("B"))
    val C = Singleton(SEql("C"))
    val D = Singleton(SEql("D"))
    assert(Or(Not(A), Star(C)).conversionO15()
             == Not(A))
    assert(Or(Not(A), Star(B)).conversionO15()
             == Or(Not(A), Star(B)))
    assert(Or(Not(A), Star(C), B).conversionO15()
             == Or(Not(A), B))
    assert(Or(Not(A), Star(C), B, Star(D), Star(A)).conversionO15()
             == Or(Not(A), B, Star(A)))
  }

  test("combo conversionC15") {
    // Or(<{1,2,3}>,<{a,b,c}>,Not(<{4,5,6}>))
    assert(Or(Singleton(SMember(1, 2, 3, 4)),
              Singleton(SMember(3, 4, 5, 6)),
              Not(Singleton(SMember(10, 11, 12, 13))),
              Not(Singleton(SMember(12, 13, 14, 15)))).conversionC15()
             == Or(Singleton(SMember(1, 2, 3, 4, 5, 6)),
                   Not(Singleton(SMember(12, 13)))))
    assert(And(Singleton(SMember(1, 2, 3, 4)),
               Singleton(SMember(3, 4, 5, 6)),
               Not(Singleton(SMember(10, 11, 12, 13))),
               Not(Singleton(SMember(12, 13, 14, 15)))).conversionC15()
             == And(Singleton(SMember(3, 4)),
                    Not(Singleton(SMember(10, 11, 12, 13, 14, 15)))))

    val X = Not(Singleton(SEql(100)))
    val Y = Not(Singleton(SEql(200)))
    assert(Or(Singleton(SMember(1, 2, 3)),
              X,
              Singleton(SMember("a", "b", "c")),
              Y,
              Singleton(SEql(0))).conversionC15()
             == Or(Not(Singleton(SEmpty)), Singleton(SMember(1, 2, 3, "a", "b", "c", 0))))
    assert(Or(Singleton(SMember(1, 2, 3)),
              X,
              Singleton(SMember("a", "b", "c")),
              Singleton(SMember(1, 2, 3)),
              Y,
              Singleton(SEql(0))).conversionC15()
             == Or(Not(Singleton(SEmpty)), Singleton(SMember(1, 2, 3, "a", "b", "c", 0))))
  }
  test("or conversionC17") {
    // Or(<{1,2,3,4}>,Not(<{3,4,5,6}>))
    //  --> Or(<{3,4}>,Not(<{3,4,5,6}>))
    //  i.e., 1 and 2 are already in Not(<{3,4,5,6}>), so they can
    //     be removed from <{1,2,3,4}> without effecting the overall result

    assert(Or(Singleton(SMember(1, 2, 3, 4)),
              Not(Singleton(SMember(3, 4, 5, 6)))).conversionC17()
             == Or(Singleton(SMember(3, 4)),
                   Not(Singleton(SMember(3, 4, 5, 6)))))

    assert(Or(Singleton(SMember(1, 2, 3)),
              Not(Singleton(SMember(1, 2, 3, 4, 5, 6)))).conversionC17()
             == Or(Singleton(SMember(1, 2, 3)),
                   Not(Singleton(SMember(1, 2, 3, 4, 5, 6)))))

    assert(Or(Singleton(SMember(7, 8, 9)),
              Not(Singleton(SMember(3, 4, 5, 6)))).conversionC17()
             == Or(Singleton(SEmpty),
                   Not(Singleton(SMember(3, 4, 5, 6)))))

    assert(Or(Singleton(SMember(7, 8, 9)),
              Singleton(SEql(0)),
              Not(Singleton(SMember(3, 4, 5, 6)))).conversionC17()
             == Or(Singleton(SEmpty),
                   Singleton(SEql(0)),
                   Not(Singleton(SMember(3, 4, 5, 6)))))

    assert(Or(Singleton(SMember(1, 2, 3)),
              Singleton(SEql(0)),
              Not(Singleton(SMember(1, 2, 3, 4, 5, 6)))).conversionC17()
             == Or(Singleton(SMember(1, 2, 3)),
                   Singleton(SEql(0)),
                   Not(Singleton(SMember(1, 2, 3, 4, 5, 6)))))
    val i = Singleton(SMember(0, 1, 2, 3, 4, 5))
    assert(Or(Singleton(SMember(1, 2, 3, "a", "b", "c")),
              Or(i)).conversionC17()
             == Or(Singleton(SMember(1, 2, 3, "a", "b", "c")),
                   Or(i)))

    assert(Or(Singleton(SMember(1, 2, 3, "a", "b", "c")),
              Not(i)).conversionC17()
             == Or(Singleton(SMember(1, 2, 3)),
                   Not(i)))

    assert(Or(Singleton(SMember(1, 2, 3, "a", "b", "c")),
              Cat(Star(i), Singleton(SEql("a")))).conversionC17()
             == Or(Singleton(SMember(1, 2, 3, "a", "b", "c")),
                   Cat(Star(i), Singleton(SEql("a")))))
    //assert(Or(Singleton(SMember(1,2,3,"a","b","c")),
    //          // TODO, currently we don't know whether Not(Cat(...)) is inhabited
    //          //    so we cant reduce this member{1,2,3,"a","b","c"}
    //          Not(Cat(Star(i),Singleton(SEql("a"))))).conversionC17()
    //         == Or(Singleton(SMember(1,2,3,"a")),
    //               Not(Cat(Star(i),Singleton(SEql("a"))))))
    // can't convert because we cannot figure how whether And(Cat(Sigma,Sigma,Star(Sigma)),Singleton(SEql(-1)))
    //   is inhabited.
    assert(Or(And(Cat(Sigma, Sigma, Star(Sigma)), Singleton(SEql(-1))), Singleton(SEql(-1))).conversionC17()
             == Or(And(Cat(Sigma, Sigma, Star(Sigma)), Singleton(SEql(-1))), Singleton(SEql(-1))))
  }
  test("or conversion99") {
    // canonicalizing sub nodes
    val A = Singleton(SMember("A", "B"))
    val B = Singleton(SEql("B"))
    assert(Or(Or(A, A), Or(B, B)).conversionC99()
             == Or(A, B))
  }
}
