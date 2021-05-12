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

import org.scalatest.funsuite.AnyFunSuite
import RteImplicits._
import genus._

class RteTestSuite extends AnyFunSuite {

  test("implicits test") {

    assert(Not(SAtomic(classOf[Integer])) == Not(classOf[Integer]))
    assert(Not(SAtomic(classOf[Long])) != Not(classOf[Integer]))

    assert(And(SAtomic(classOf[Integer])) == And(classOf[Integer]))
    assert(And(SAtomic(classOf[Long])) != And(classOf[Integer]))

    assert(Or(SAtomic(classOf[Integer])) == Or(classOf[Integer]))
    assert(Or(SAtomic(classOf[Long])) != Or(classOf[Integer]))

    assert(Cat(SAtomic(classOf[Integer])) == Cat(classOf[Integer]))
    assert(Cat(SAtomic(classOf[Long])) != Cat(classOf[Integer]))

    assert(Star(SAtomic(classOf[Integer])) == Star(classOf[Integer]))
    assert(Star(SAtomic(classOf[Long])) != Star(classOf[Integer]))
  }
  test("LaTeX") {
    Or(And(SAtomic(classOf[Integer]),
           Not(SAtomic(classOf[Long]))),
       Not(SEql(43))).toLaTeX

    Or(And(classOf[Integer],
           Not(SAtomic(classOf[Long]))),
       Not(SEql(44))).toLaTeX
  }
  test("nullable") {
    assert(Sigma.nullable == false)
    assert(EmptySet.nullable == false)
    assert(Star(Sigma).nullable == true)
    assert(Star(EmptySet).nullable == true)
    for {depth <- 1 to 5
         _ <- 0 to 100
         r = Rte.randomRte(depth)} {
      assert(Star(r).nullable == true)
      assert(Not(Star(r)).nullable == false)
    }
    for {depth <- 1 to 5
         _ <- 1 to 1000
         r = Rte.randomRte(depth)}
      r.nullable
  }

  test("firstTypes") {
    for {depth <- 1 to 5
         _ <- 1 to 1000
         r = Rte.randomRte(depth)}
      r.firstTypes
  }

  test("operators") {
    for {depth <- 1 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)} {
      assert((r1 | r2) == Or(r1, r2))
      assert((r1 & r2) == And(r1, r2))
      assert(r1 ++ r2 == Cat(r1, r2)) // check that reversing the arguments works correctly
      assert(!r1 == Not(r1))
      assert(r1.? == Or(r1, EmptyWord))
      assert(r1.* == Star(r1))
      assert(r1.+ == Cat(r1, Star(r1)))
      assert((r1 ^ 0) == EmptyWord)
      assert((r1 ^ 1) == r1)
      assert((r1 ^ 2).canonicalize == Cat(r1, r1).canonicalize)
      assert((r1 ^ 3).canonicalize == Cat(r1, r1, r1).canonicalize)
    }
  }
  test("canonicalize random") {
    for {depth <- 0 to 5
         _ <- 1 to 10000
         r1 = Rte.randomRte(depth = depth)
         } {
      r1.canonicalize
    }
  }

  test("canonicalize") {

    assert(EmptySet.canonicalize == EmptySet)
    assert(Sigma.canonicalize == Sigma)
    assert(EmptyWord.canonicalize == EmptyWord)
  }

  test("canonicalize Singleton") {
    import Types.randomType
    for {
      _ <- 1 to 1000
      t1 = randomType(0)
      t2 = randomType(0)
    } {
      assert(Singleton(SAnd(t1, t2)).canonicalize == And(Singleton(t1), Singleton(t2)).canonicalize)
      assert(Singleton(SOr(t1, t2)).canonicalize == Or(Singleton(t1), Singleton(t2)).canonicalize)
      assert(Singleton(SNot(t1)).canonicalize == And(Not(t1), Sigma).canonicalize)
    }
  }

  test("canonicalize nullable"){
    for {depth <- 0 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         } {
      assert(r1.nullable == r1.canonicalize.nullable,
             s"r1=$r1 is"
               + (if (r1.nullable) "" else " not ")
               + "nullable while r1.canonicalize="
               + r1.canonicalize
               + " is"
               + (if (r1.canonicalize.nullable) "" else " not ")
               + "nullable")
    }
  }

  test("canonicalize cat") {
    assert(Cat().canonicalize == EmptyWord)
    for {depth <- 0 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         } {
      assert(Cat(r1).canonicalize == r1.canonicalize)
      assert(Cat(EmptySet, r1).canonicalize == EmptySet)
      assert(Cat(r1, EmptySet).canonicalize == EmptySet)
      assert(Cat(r1, EmptySet, r2).canonicalize == EmptySet)

      assert(Cat(EmptyWord, r1).canonicalize == r1.canonicalize)
      assert(Cat(r1, EmptyWord).canonicalize == r1.canonicalize)
      assert(Cat(r1, EmptyWord, r2).canonicalize == Cat(r1, r2).canonicalize)

      assert(Cat(Cat(r1, r2), Cat(r1, r2)).canonicalize == Cat(r1, r2, r1, r2).canonicalize)
      assert(Cat(r1, Cat(r1, r2), r2).canonicalize == Cat(r1, r1, r2, r2).canonicalize)

      assert(Cat(r1, r2.*, r2.*, r1).canonicalize == Cat(r1, r2.*, r1).canonicalize)
    }
  }
  test("canonicalize star") {
    assert(EmptyWord.*.canonicalize == EmptyWord)
    assert(EmptySet.*.canonicalize == EmptyWord)

    for {depth <- 0 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         } {
      assert(Star(Star(r1)).canonicalize == Star(r1).canonicalize)
      assert(Star(r1).canonicalize == Star(r1.canonicalize).canonicalize)
      assert(Star(Cat(r1,Star(r1))).canonicalize == Star(r1).canonicalize) // (x x*)* --> x*
      assert(Star(Cat(Star(r1),r1)).canonicalize == Star(r1).canonicalize) // (x* x)* --> x*
    }
  }
  test("canonicalize not") {
    assert(Not(Sigma).canonicalize == Or(Cat(Sigma, Sigma, Star(Sigma)),
                                         EmptyWord))
    assert(Not(Star(Sigma)).canonicalize == EmptySet)
    assert(Not(EmptyWord).canonicalize == Cat(Sigma, Star(Sigma)))
    assert(Not(EmptySet).canonicalize == Star(Sigma))
    for {depth <- 0 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         } {
      assert(Not(Not(r1)).canonicalize == r1.canonicalize)
      assert(Not(And(r1, r2)).canonicalize == Or(Not(r1), Not(r2)).canonicalize)
      assert(Not(Or(r1, r2)).canonicalize == And(Not(r1), Not(r2)).canonicalize)
      assert(Not(r1).canonicalize == Not(r1.canonicalize).canonicalize)
    }
  }
  test("canonicalize and") {
    // And(Not(<[= 4]>),<[Member 0,4,5,6]>,Not(<[= 1]>),<[Member 1,2,3,4]>)
    assert(And(Not(Singleton(SEql(4))),
               Singleton(SMember(0,4,5,6)),
               Not(Singleton(SEql(1))),
               Singleton(SMember(1,2,3,4))).canonicalize == EmptySet)
    assert(And(Singleton(SEql(4)),
               Singleton(SMember(0,4,5,6)),
               Not(Singleton(SEql(1))),
               Singleton(SMember(1,2,3,4))).canonicalize == Singleton(SEql(4)))

    assert(And(Singleton(genus.SEql(0)), Sigma).canonicalize == Singleton(genus.SEql(0)))
    assert(And(EmptySet, EmptySet).canonicalize == EmptySet)
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
         r4 = Rte.randomRte(depth)
         } {
      // removing duplicates
      assert(And(r1, r1).canonicalize == r1.canonicalize, s"And(r1,r1)=${And(r1, r1).canonicalize}   r1=${r1.canonicalize}")
      assert(And(r1, r2, r1).canonicalize == And(r1, r2).canonicalize ||
               And(r1, r2, r1).canonicalize == And(r2, r1).canonicalize
             )
      assert(And(r1, Sigma, r2, EmptyWord).canonicalize == EmptySet)
      assert(And(r1, EmptySet, r2).canonicalize == EmptySet)

      assert(And(And(r1, r2), r3).canonicalize == And(r1, And(r2, r3)).canonicalize,
             s"r1=$r1  r2=$r2  r3=$r3   canonicalized: ${r1.canonicalize}  ${r2.canonicalize}  ${r3.canonicalize}")
      assert(And(And(r1, r2), r3).canonicalize == And(r1, r2, r3).canonicalize)
      assert(And(r1, Rte.sigmaStar, r2, r3).canonicalize == And(r1, r2, r3).canonicalize)
      assert(And(r1, Sigma.*, r2, r3).canonicalize == And(r1, r2, r3).canonicalize)
      assert(And(r1, trsup, r2, trsub, r3).canonicalize == And(r1, r2, trsub, r3).canonicalize)
      assert(And(r1, trsub, r2, trsup, r3).canonicalize == And(r1, trsub, r2, r3).canonicalize)

      // And(a,Or(x,y),b) --> Or(And(a,x,b),And(a,y,b))
      assert(And(r1, Or(r2, r3), r4).canonicalize == Or(And(r1, r2, r4),
                                                        And(r1, r3, r4)).canonicalize,
             s"r1=$r1  r2=$r2  r3=$r3  r4=$r4" +
               s"  canonicalized: r1=${r1.canonicalize}  r2=${r2.canonicalize}  r3=${r3.canonicalize}  r4=${r4.canonicalize}" +
               " And(r1,r2,r4)=" + And(r1, r2, r4).canonicalize +
               " And(r1,r3,r4)=" + And(r1, r3, r4).canonicalize
             )
      assert(And(r1, r2, Not(r1), r3).canonicalize == EmptySet)
      assert(And(r1, trd1, r2, trd2, r3).canonicalize == EmptySet)
    }
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

    locally{
      val r1 = Star(trd1)
      val r2 = EmptyWord
      val r3 = trd1 // Not(Not(trd1))
      assert(Or(EmptyWord, Cat( r2, r3, Star(Cat( r2, r3)))).canonicalize
               == Or(EmptyWord, Star(Cat( r2, r3))).canonicalize)
      assert(Or(EmptyWord, Cat( r1, Star(Cat( r1)))).canonicalize
               == Or(EmptyWord, Star(Cat( r1))).canonicalize)

      println(List(Or(EmptyWord, Cat( r1, r3, Star(Cat( r1, r3)))),
                   Or(EmptyWord, Cat( r1, r3, Star(Cat( r1, r3)))).canonicalize))

      println(List(Or(EmptyWord, Star(Cat( r1, r3))),
                   Or(EmptyWord, Star(Cat( r1, r3))).canonicalize))

      assert((     Or(EmptyWord, Cat( r1, r3, Star(Cat( r1, r3)))).canonicalize)
               == (Or(EmptyWord, Star(Cat( r1, r3))).canonicalize))

      assert(Or(EmptyWord, Cat(r1, r2, r3, Star(Cat(r1, r2, r3)))).canonicalize
               == (Or(EmptyWord, Star(Cat(r1, r2, r3))).canonicalize))
    }

    assert(Rte.sigmaStar == Or(Sigma,
                               Star(Cat(Sigma,Star(Sigma)))).canonicalize)
    assert(Rte.sigmaStar == Or(Sigma,
                               Star(trd2),
                               Star(Cat(Sigma,Star(Sigma)))).canonicalize)
    // Star(Σ) did not equal Or(<Trait3$1>,Σ,Star(<[Member 1,2,3,4]>),Star(Cat(Σ,Star(Σ))))
    assert(Rte.sigmaStar == Or(trd1,
                               Sigma,
                               Star(trd2),
                               Star(Cat(Sigma,Star(Sigma)))).canonicalize)

    // Or(A,ε,<java.lang.String>,<java.lang.Integer>) did not equal Or(A,<java.lang.String>,ε,<java.lang.Integer>)
    assert(Or().canonicalize
             == Or().canonicalize)



    //                     Or(ε,A,Cat(C,B,Star(Cat(C,B))))
    // not isomorphic with Or(A,Star(Cat(C,B)))
    for {depth <- 0 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         r3 = Rte.randomRte(depth)
         r4 = Rte.randomRte(depth)
         } {
      // remove EmptySet
      assert(Or(r1,EmptySet,r2).canonicalize == Or(r1,r2).canonicalize)

      // remove duplicate
      assert(Or(r1,r2,r3,r2).canonicalize == Or(r1,r2,r3).canonicalize ||
               Or(r1,r2,r3,r2).canonicalize == Or(r1,r3,r2).canonicalize
             )
      // Or(x) = x
      assert(Or(r1).canonicalize == r1.canonicalize)

      // Or(a,Star(Sigma),b) --> Star(Sigma)
      assert(Or(r1,Star(Sigma),r2).canonicalize == Rte.sigmaStar)

      // Or(a,Or(x,y),b) --> Or(a,x,y,b)
      assert(Or(r1,Or(r2,r3),r4).canonicalize == Or(r1,r2,r3,r4).canonicalize)

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

      assert(Or(EmptyWord,Cat(r3,Star(r3))).canonicalize
               ~= Or(EmptyWord,Star(r3)).canonicalize,
             "" + Or(EmptyWord,Cat(r3,Star(r3))).canonicalize
               + " not isomorphic with "
               + Or(EmptyWord,Star(r3)).canonicalize)

      // Or(:epsilon,Cat(X,Y,Z,Star(Cat(X,Y,Z))))
      //  --> Or(:epsilon,Star(Cat(X,Y,Z)))
      locally {

        assert(Or(EmptyWord, Cat(r1, r2, r3, Star(Cat(r1, r2, r3)))).canonicalize
                 == Or(EmptyWord, Star(Cat(r1, r2, r3))).canonicalize,
               s"r1=$r1 r2=$r2  r3=$r3")
      }
      // Or(Star(A),Cat(X,Y,Z,Star(Cat(X,Y,Z))))
      //  --> Or(Star(A),Star(Cat(X,Y,Z)))
      assert(Or(Star(r4),Cat(r1,r2,r3,Star(Cat(r1,r2,r3)))).canonicalize
               == Or(Star(r4), Star(Cat(r1,r2,r3))).canonicalize,
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
      assert(Or(r1,EmptyWord,r2,Cat(r3,Star(r3)),r4).canonicalize
               ~= Or(r1,EmptyWord,r2,Star(r3),r4).canonicalize,
             "" + Or(r1,EmptyWord,r2,Cat(r3,Star(r3)),r4).canonicalize
               + " not isomorphic with "
               + Or(r1,EmptyWord,r2,Star(r3),r4).canonicalize)
      assert(Or(r1, EmptyWord, r2, r3.+, r4).canonicalize
               ~= Or(r1,EmptyWord,r2,Star(r3),r4).canonicalize)

      // (:or A :epsilon B (:* X) C)
      //   --> (:or A B (:* X) C)
      assert(Or(r1,EmptyWord,r2,Star(r3),r4).canonicalize ~=
             Or(r1,r2,Star(r3),r4).canonicalize)

      // remove subset
      assert(Or(r1,trsub,r2,trsup,r3).canonicalize ==
             Or(r1,r2,trsup,r3).canonicalize)
    }
  }
  test("derivative special cases"){
    import genus.STop
    import Types.randomType
    assert (EmptySet.derivative(Some(STop)) == EmptySet)
    assert (Sigma.derivative(Some(STop)) == EmptyWord)
    assert (EmptyWord.derivative(Some(STop)) == EmptySet)
    assert (Singleton(STop).derivative(Some(STop)) == EmptyWord)
    assert (Singleton(SEmpty).derivative(Some(STop)) == EmptySet)

    assert (EmptySet.derivative(Some(SEmpty)) == EmptySet)
    assert (Sigma.derivative(Some(SEmpty)) == EmptySet)
    assert (EmptyWord.derivative(Some(SEmpty)) == EmptySet)
    assert (Singleton(STop).derivative(Some(SEmpty)) == EmptySet)
    assert (Singleton(SEmpty).derivative(Some(SEmpty)) == EmptySet)

    // deriv wrt EmptyWord
    assert (EmptySet.derivative(None) == EmptySet)
    assert (Sigma.derivative(None) == Sigma)
    assert (EmptyWord.derivative(None) == EmptyWord)
    assert (Singleton(STop).derivative(None) == Sigma)
    assert (Singleton(SEmpty).derivative(None) == EmptySet)

    for {depth <- 0 to 5
         _ <- 1 to 1000
         td = randomType(depth)
         rt = Singleton(td)
         } {
      if (td.inhabited.contains(true))
        assert(Sigma.derivative(Some(td)) == EmptyWord)
      else if (td.inhabited.contains(false))
        assert(Sigma.derivative(Some(td)) == EmptySet)

      assert(EmptySet.derivative(Some(td)) == EmptySet)
      assert(EmptyWord.derivative(Some(td)) == EmptySet)
      if (td.inhabited.contains(true))
        assert(rt.derivative(Some(STop)) == EmptyWord,
               s"failed deriv of $rt wrt Some(STop)")
      else if (td.inhabited.contains(false))
        assert(rt.derivative(Some(STop)) == EmptySet,
               s"failed inhabited=${td.inhabited} deriv of $rt wrt Some(STop)")
      assert(rt.derivative(Some(SEmpty)) == EmptySet)

      assert(rt.derivative(None).canonicalize == rt.canonicalize,
             s"deriv of $rt wrt EmptyWord/None returned ${rt.derivative(None)} expecting $rt which reduces to ${rt.canonicalize}")
    }
  }
  test("derivative random") {

    for {depth <- 0 to 5
         _ <- 1 to 1000
         rt = Rte.randomRte(depth)
         can = rt.canonicalize
         m = Types.mdtd(can.firstTypes)
         td <- m
         } {
      can.derivative(Some(td))
    }
  }
  test("derivatives random") {

    for {depth <- 0 to 6
         _ <- 1 to 2000
         rt = Rte.randomRte(depth)
         (intToV,m) = rt.derivatives()
         } {
      println(s"rt = $rt")
      println(s"   intToV = $intToV")
      println(s"   m = $m")
    }
  }

  test("rte to dfa") {
    import xymbolyco.GraphViz._

    for {depth <- 5 to 6
         _ <- 1 to 2
         rt = Rte.randomRte(depth)
         sdfa = rt.toDfa()
         } {
      dfaView(sdfa, abbreviateTransitions=true)
    }
  }
}