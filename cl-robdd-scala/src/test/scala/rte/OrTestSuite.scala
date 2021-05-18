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
// import rte.RteImplicits._

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

  test("canonicalize or") {
    assert(Or(EmptySet, EmptySet).canonicalize == EmptySet)
    assert(Or().canonicalize == EmptySet)
    class TestSup
    class TestSub extends TestSup
    class TestD1 // disjoint from TestD2
    class TestD2 // disjoint from TestD1
    val trsup = Singleton(genus.SAtomic(classOf[TestSup]))
    val trsub = Singleton(genus.SAtomic(classOf[TestSub]))
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

    //                     Or(ε,A,Cat(C,B,Star(Cat(C,B))))
    // not isomorphic with Or(A,Star(Cat(C,B)))
    for {depth <- 0 to 5
         _ <- 1 to 500
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         r4 = Rte.randomRte(depth)
         } {
      // remove EmptySet
      assert(Or(r1, EmptySet, r2).canonicalize == Or(r1, r2).canonicalize)

      // remove duplicate
      assert(Or(r1, r2, r3, r2).canonicalize == Or(r1, r2, r3).canonicalize ||
               Or(r1, r2, r3, r2).canonicalize == Or(r1, r3, r2).canonicalize
             )
      // Or(x) = x
      assert(Or(r1).canonicalize == r1.canonicalize)

      // Or(a,Star(Sigma),b) --> Star(Sigma)
      assert(Or(r1, Star(Sigma), r2).canonicalize == Rte.sigmaStar)

      // Or(a,Or(x,y)) --> Or(a,x,y)
      assert(Or(r1, Or(r2, r3)).canonicalize ~= Or(r1, r2, r3).canonicalize,
             s"\n   r1=$r1 \n   r2=$r2 \n   r3=$r3" +
               s"\n   Or(r1,Or(r2,r3))= " + Or(r1, Or(r2, r3)).canonicalize +
               s"\n   Or(r1,r2,r3)=     " + Or(r1, r2, r3))

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

      // (:or :epsilon (:cat X (:* X)))
      //   --> (:or :epsilon (:* X))
      //println("canonicalizing A " + Or(EmptyWord,Cat(r3,Star(r3)))
      //          + " --> " + Or(EmptyWord,Cat(r3,Star(r3))).canonicalize )
      //println("canonicalizing B " + Or(EmptyWord,Star(r3))
      //          + " --> " + Or(EmptyWord,Star(r3)).canonicalize)
      //     Or(ε,Cat(Cat(A,B),Star(Cat(A,B))))
      // --> Or(ε,Cat(A,B,Star(Cat(A,B))))

      assert(Or(EmptyWord, Cat(r3, Star(r3))).canonicalize
               ~= Or(EmptyWord, Star(r3)).canonicalize,
             "" + Or(EmptyWord, Cat(r3, Star(r3))).canonicalize
               + " not isomorphic with "
               + Or(EmptyWord, Star(r3)).canonicalize)

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


  test("canonicalize or 253") {

    for {depth <- 0 to 4
         _ <- 1 to 500
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         r4 = Rte.randomRte(depth)
         } {
      assert(Or(r1, And(r2, r3).canonicalize, r4).canonicalize ~=
               Or(r1, And(r2, r3), r4).canonicalize)
    }
  }
}