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
      assert((r1 ^ 2) == Cat(r1, r1))
      assert((r1 ^ 3) == Cat(r1, r1, r1))
    }
  }
  test("canonicalize random"){
    for {depth <- 0 to 5
         _ <- 1 to 10000
         r1 = Rte.randomRte(depth=depth)
         } {
      r1.canonicalize
    }
  }

  test("canonicalize") {

    assert(EmptySet.canonicalize == EmptySet)
    assert(Sigma.canonicalize == Sigma)
    assert(EmptyWord.canonicalize == EmptyWord)
  }

  test("canonicalize Singleton"){
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
  test("canonicalize cat") {
    assert(Cat().canonicalize == EmptyWord)
    for{depth <- 0 to 5
        _ <- 1 to 1000
        r1 = Rte.randomRte(depth)
        r2 = Rte.randomRte(depth)
        } {
      assert(Cat(r1).canonicalize == r1.canonicalize)
      assert(Cat(EmptySet,r1).canonicalize == EmptySet)
      assert(Cat(r1,EmptySet).canonicalize == EmptySet)
      assert(Cat(r1,EmptySet,r2).canonicalize == EmptySet)

      assert(Cat(EmptyWord,r1).canonicalize == r1.canonicalize)
      assert(Cat(r1,EmptyWord).canonicalize == r1.canonicalize)
      assert(Cat(r1,EmptyWord,r2).canonicalize == Cat(r1,r2).canonicalize)

      assert(Cat(Cat(r1,r2),Cat(r1,r2)).canonicalize == Cat(r1,r2,r1,r2).canonicalize)
      assert(Cat(r1,Cat(r1,r2),r2).canonicalize == Cat(r1,r1,r2,r2).canonicalize)

      assert(Cat(r1,r2.*, r2.*, r1).canonicalize == Cat(r1,r2.*,r1).canonicalize)
    }
  }
  test("canonicalize star") {
    assert(EmptyWord.*.canonicalize == EmptyWord)
    assert(EmptySet.*.canonicalize == EmptyWord)

    for{depth <- 0 to 5
        _ <- 1 to 1000
        r1 = Rte.randomRte(depth)
        } {
      assert(Star(Star(r1)).canonicalize == Star(r1).canonicalize)
      assert(Star(r1).canonicalize == Star(r1.canonicalize).canonicalize)
    }
  }

}