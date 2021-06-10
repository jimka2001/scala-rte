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
        Singleton(SEql(-1))).derivative(Some(number))

  }
  test("avoid infinite loop b"){
    // r1=Cat(Cat(Σ,(Σ)*),Cat(<Integer>,<{a,b,c}>))
    // r2=And((<Number>)*,Or(<Trait3$1>,<[= -1]>))
    val number = Singleton(SAtomic(classOf[Number]))

    trait Trait2
    trait Trait3 extends Trait2
    val r3 = Or(SMember(1,2,3,4),
                Cat(Not(SEql(-1)),
                    number,
                    SEql(-1)))
    r3.canonicalizeOnce

  }
  test("canonicalize or b1") {
    for {depth <- 2 to 2
         r <- 1 to 5000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         } {
      // remove EmptySet
      try {
        Or(r1,r2).canonicalize }
      catch{
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
  test("discovered case 279"){
    val s = Singleton(SEql(-1))
    val c = Cat(Sigma,Sigma,Star(Sigma))
    val r1 = Or(And(c,s),s)
    r1.canonicalizeDebug(5)
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

    // Or(<[= 1]>,And(Cat(And(<Trait3X>,<[= true]>),Not(<Integer>)),<STop>),And(Or(Or(Cat(Σ,Σ,(Σ)*),ε),ε),<STop>),ε)
    val r5 = Or(r1,
                And(ra, r3),
                And(Or(Or(Cat(Sigma, Sigma, Star(Sigma)), EmptyWord), EmptyWord), r3),
                EmptyWord)
    //r5.canonicalizeDebug(10,List("a",1,0,true,false))

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
               s"\n   Or(r2,r3) = ${Or(r2,r3).canonicalize}" +
               s"\n   r4= ${r4.canonicalize}" +
               s"\nlhs = " + Or(r1, And(r2, r3).canonicalize, r4).canonicalize +
               s"\nrhs = " + Or(r1, And(r2, r3), r4).canonicalize
             )
    }
  }

  test("or conversion3"){
    // Or(... Sigma* ....) -> Sigma*
    assert(Or(Or(),Star(Sigma),Or()).conversion3() == Star(Sigma))
    assert(Or(Or(),Star(Sigma),Star(Sigma),Or()).conversion3() == Star(Sigma))
    assert(Or(Or(),Or()).conversion3() != Star(Sigma))
  }
  test("or conversion4"){
    // uniquify
    assert(Or(Or(),Star(Sigma),Or()).conversion4() == Or(Star(Sigma),Or()))

    // order is maintained
    assert(Or(Or(),And()).conversion4() == Or(Or(),And()))
  }
  test("or conversion5"){
    val X = Singleton(SEql(1))
    val Y = Singleton(SEql(2))
    assert(Or(Y,X).conversion5() == Or(X,Y))
    assert(Or(X,Y).conversion5() == Or(X,Y))
  }
  test("or conversion6"){
    // remove EmptySet and flatten Or(Or(...)...)
    val X = Singleton(SEql(1))
    val Y = Singleton(SEql(2))
    assert(Or(X).conversion6() == X)
    assert(Or(Or(X)).conversion6() == X)
    assert(Or(Or(Or(X))).conversion6() == Or(X))
    assert(Or(Or(X),Or(X)).conversion6() == Or(X,X))
    assert(Or(X,Y,EmptySet).conversion6() == Or(X,Y))
    assert(Or(EmptySet,X,EmptySet,Y,EmptySet).conversion6() == Or(X,Y))
    assert(Or(X,Y).conversion6() == Or(X,Y))
    assert(Or(Y,X,Y).conversion6() == Or(Y,X,Y))
  }
  test("or conversion7"){
    // (:or A B (:* B) C)
    // --> (:or A (:* B) C)
    val A = Singleton(SEql(1))
    val B = Singleton(SEql(2))
    val C = Singleton(SEql(3))
    assert(Or(A,B,Star(B),C).conversion7() == Or(A,Star(B),C))
    assert(Or(A,Star(C),B,C).conversion7() == Or(A,Star(C),B))
    assert(Or(A,Star(B),B,C,Star(C)).conversion7() == Or(A,Star(B),Star(C)))
  }
  test("or conversion8"){
    // (:or (:* C) (:cat X (:* X)))
    //   --> (:or (:* C) (:* X))

    val A = Singleton(SEql(1))
    val B = Singleton(SEql(2))
    val C = Singleton(SEql(3))
    val X = Singleton(SEql(4))

    // (:or A :epsilon B (:cat X (:* X)) C)
    //   --> (:or A :epsilon B (:* X) C )
    assert(Or(A,EmptyWord,B,Cat(X,Star(X)),C).conversion8(true)
           == Or(A,EmptyWord,B,Star(X),C))

    // (:or :epsilon (:cat X (:* X)))
    //   --> (:or :epsilon (:* X))
    assert(Or(EmptyWord,Cat(X,Star(X))).conversion8(true)
           == Or(EmptyWord,Star(X)))

    // (:or (:* C) (:cat X (:* X)))
    //   --> (:or (:* C) (:* X))
    assert(Or(Star(C),Cat(X,Star(X))).conversion8(true)
             == Or(Star(C),Star(X)))

    // (:or (:* C) (:cat X (:* A)))
    //   --> no change
    assert(Or(Star(C),Cat(X,Star(A))).conversion8(true)
             == Or(Star(C),Cat(X,Star(A))))
  }
  test("or conversion9"){
    val A = Singleton(SEql("A"))
    val B = Singleton(SEql("B"))
    val C = Singleton(SEql("C"))
    val X = Singleton(SEql("X"))
    val Y = Singleton(SEql("Y"))
    val Z = Singleton(SEql("Z"))

    // (:or A :epsilon B (:cat X Y Z (:* (:cat X Y Z))) C)
    //   --> (:or A :epsilon B (:* (:cat X Y Z)) C )
    assert(Or(A,EmptyWord,B,Cat(X,Y,Z,Star(Cat(X,Y,Z))), C).conversion9(true)
             == Or(A,EmptyWord,B,Star(Cat(X,Y,Z)),C))

    // (:or B* (:cat X Y Z (:* (:cat X Y Z))))
    //   --> (:or B* (:* (:cat X Y Z)))
    assert(Or(Star(B),Cat(X,Y,Z,Star(Cat(X,Y,Z)))).conversion9(true)
             == Or(Star(B),Star(Cat(X,Y,Z))))
  }
  test("or conversion10"){
    // (:or A :epsilon B (:* X) C)
    //   --> (:or A B (:* X) C)
    val A = Singleton(SEql("A"))
    val B = Singleton(SEql("B"))
    val C = Singleton(SEql("C"))
    val X = Singleton(SEql("X"))
    assert(Or(A,EmptyWord,B,Star(X),C).conversion10()
             == Or(A,B,Star(X),C))
    assert(Or(A,EmptyWord,B,C).conversion10()
             == Or(A,EmptyWord,B,C))
  }
  test("or conversion11b"){
    // if Sigma is in the operands, then filter out all singletons
    // Or(Singleton(A),Sigma,...) -> Or(Sigma,...)
    val A = Singleton(SEql("A"))
    val AB = Singleton(SMember("A","B"))
    val C = Singleton(SEql("C"))
    val X = Star(A)
    assert(Or(A,AB,Sigma,C,X).conversion11b()
             == Or(Sigma,X))
    assert(Or(A,AB,C,X).conversion11b()
             == Or(A,AB,C,X))
  }
  test("or conversion11"){
    // filter out singletons which are a subclass of other singletons
    val A = Singleton(SEql("A"))
    val AB = Singleton(SMember("A","B"))
    val BA = Singleton(SMember("B","A"))
    val C = Singleton(SEql("C"))
    val X = Singleton(SEql("X"))

    assert(Or(A,AB,C,X).conversion11()
             == Or(AB,C,X))
    assert(Or(C,A,X,AB).conversion11()
             == Or(C,X,AB))
    assert(Or(AB,C,A,X).conversion11()
             == Or(AB,C,X))
    assert(Or(AB,BA).conversion11()
           == AB ||
             Or(AB,BA).conversion11()
               == BA
           )
  }
  test("or conversion12"){
    // sigmaSigmaStarSigma = Cat(Sigma, Sigma, sigmaStar)
    // Or(   A, B, ... Cat(Sigma,Sigma,Sigma*) ... Not(Singleton(X)) ...)
    //   --> Or( A, B, ... Not(Singleton(X))
    val A = Singleton(SEql("A"))
    val B = Singleton(SEql("B"))
    val X = Singleton(SEql("X"))
    assert(Or(A,Cat(Sigma,Sigma,Star(Sigma)),B,Not(X)).conversion12()
           == Or(A,B,Not(X)).conversion12())
  }
  test("or conversion13"){
    // Or(A,Not(A),X) -> SigmaStar
    val A = Singleton(SEql("A"))
    val B = Singleton(SEql("B"))
    val X = Singleton(SEql("X"))
    assert(Or(A,B,Not(A),X).conversion13()
           == Star(Sigma))
    assert(Or(A,B,X).conversion13()
             == Or(A,B,X))
    assert(Or(A,Not(B),X).conversion13()
             == Or(A,Not(B),X))
  }
  test("or conversion14"){
    // Or(A,Not(B),X) -> Sigma* if B is subtype of A
    val A = Singleton(SMember("A","B"))
    val B = Singleton(SEql("B"))
    val X = Singleton(SEql("X"))
    assert(Or(A,Not(B),X).conversion14()
             == Star(Sigma))
    assert(Or(Not(A),B,X).conversion14()
             != Star(Sigma))
  }
  test("or conversion15"){
    // Or(Not(A),B*) = Not(A) if A and B  disjoint
    //  only works if length = 2
    val A = Singleton(SMember("A","B"))
    val B = Singleton(SEql("B"))
    val C = Singleton(SEql("C"))
    val D = Singleton(SEql("D"))
    assert(Or(Not(A),Star(C)).conversion15()
             == Not(A))
    assert(Or(Not(A),Star(B)).conversion15()
             == Or(Not(A),Star(B)))
    assert(Or(Not(A),Star(C),B).conversion15()
             == Or(Not(A),B))
    assert(Or(Not(A),Star(C),B,Star(D),Star(A)).conversion15()
             == Or(Not(A),B,Star(A)))
  }
  test("or conversion16"){
    // Or(<{1,2,3}>,<{a,b,c}>,Not(<{4,5,6}>))
    val X = Not(Singleton(SEql(100)))
    val Y = Not(Singleton(SEql(200)))
    assert(Or(Singleton(SMember(1,2,3)),
              X,
              Singleton(SMember("a","b","c")),
              Y,
              Singleton(SEql(0))).conversion16()
             == Or(X,Y,Singleton(SMember(1,2,3,"a","b","c",0))))
    assert(Or(Singleton(SMember(1,2,3)),
              X,
              Singleton(SMember("a","b","c")),
              Singleton(SMember(1,2,3)),
              Y,
              Singleton(SEql(0))).conversion16()
             == Or(X,Y,Singleton(SMember("a","b","c",1,2,3,0))))
  }
  test("or conversion17"){
    // Or(<{1,2,3,4}>,Not(<{3,4,5,6}>))
    //  --> Or(<{3,4}>,Not(<{3,4,5,6}>))
    //  i.e., 1 and 2 are already in Not(<{3,4,5,6}>), so they can
    //     be removed from <{1,2,3,4}> without effecting the overall result

    assert(Or(Singleton(SMember(1,2,3,4)),
              Not(Singleton(SMember(3,4,5,6)))).conversion17()
             == Or(Singleton(SMember(3,4)),
                   Not(Singleton(SMember(3,4,5,6)))))

    assert(Or(Singleton(SMember(1,2,3)),
              Not(Singleton(SMember(1,2,3,4,5,6)))).conversion17()
             == Or(Singleton(SMember(1,2,3)),
                   Not(Singleton(SMember(1,2,3,4,5,6)))))

    assert(Or(Singleton(SMember(7,8,9)),
              Not(Singleton(SMember(3,4,5,6)))).conversion17()
             == Or(Singleton(SEmpty),
                    Not(Singleton(SMember(3,4,5,6)))))

    assert(Or(Singleton(SMember(7,8,9)),
              Singleton(SEql(0)),
              Not(Singleton(SMember(3,4,5,6)))).conversion17()
             == Or(Singleton(SEmpty),
                   Singleton(SEql(0)),
                   Not(Singleton(SMember(3,4,5,6)))))

    assert(Or(Singleton(SMember(1,2,3)),
              Singleton(SEql(0)),
              Not(Singleton(SMember(1,2,3,4,5,6)))).conversion17()
             == Or(Singleton(SMember(1,2,3)),
                   Singleton(SEql(0)),
                   Not(Singleton(SMember(1,2,3,4,5,6)))))
    val i = Singleton(SMember(0,1,2,3,4,5))
    assert(Or(Singleton(SMember(1,2,3,"a","b","c")),
              Or(i )).conversion17()
             == Or(Singleton(SMember(1,2,3,"a","b","c")),
                   Or(i)))

    assert(Or(Singleton(SMember(1,2,3,"a","b","c")),
              Not(i)).conversion17()
             == Or(Singleton(SMember(1,2,3)),
                   Not(i)))

    assert(Or(Singleton(SMember(1,2,3,"a","b","c")),
              Cat(Star(i),Singleton(SEql("a")))).conversion17()
             == Or(Singleton(SMember(1,2,3,"a", "b","c")),
                   Cat(Star(i),Singleton(SEql("a")))))
    //assert(Or(Singleton(SMember(1,2,3,"a","b","c")),
    //          // TODO, currently we don't know whether Not(Cat(...)) is inhabited
    //          //    so we cant reduce this member{1,2,3,"a","b","c"}
    //          Not(Cat(Star(i),Singleton(SEql("a"))))).conversion17()
    //         == Or(Singleton(SMember(1,2,3,"a")),
    //               Not(Cat(Star(i),Singleton(SEql("a"))))))
    // can't convert because we cannot figure how whether And(Cat(Sigma,Sigma,Star(Sigma)),Singleton(SEql(-1)))
    //   is inhabited.
    assert(Or(And(Cat(Sigma,Sigma,Star(Sigma)),Singleton(SEql(-1))), Singleton(SEql(-1))).conversion17()
           == Or(And(Cat(Sigma,Sigma,Star(Sigma)),Singleton(SEql(-1))), Singleton(SEql(-1))))
  }
  test("or conversion99"){
    // canonicalizing sub nodes
    val A = Singleton(SMember("A","B"))
    val B = Singleton(SEql("B"))
    assert(Or(Or(A,A),Or(B,B)).conversion99()
           == Or(A,B))
  }
}