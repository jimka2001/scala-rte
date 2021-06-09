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

class CatTestSuite extends AnyFunSuite {

  test("cat case 99"){
    assert(Cat(Sigma,Sigma,Star(Sigma),Sigma,Sigma,Star(Sigma)).canonicalize
             == Cat(Sigma,Sigma,Sigma,Sigma,Star(Sigma)).canonicalize)
    assert(Cat(Sigma,Sigma,Star(Sigma),Sigma,Sigma,Star(Sigma)).canonicalize
             == Cat(Star(Sigma),Sigma,Sigma,Sigma,Sigma).canonicalize)
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

      assert(Cat(Cat(r1, r2), Cat(r1, r2)).canonicalize == Cat(r1, r2, r1, r2).canonicalize,
             s"r1=$r1   r2=$r2")
      assert(Cat(r1, Cat(r1, r2), r2).canonicalize == Cat(r1, r1, r2, r2).canonicalize)

      assert(Cat(r1, r2.*, r2.*, r1).canonicalize == Cat(r1, r2.*, r1).canonicalize)
    }
  }

  test("canonicalize cat case 168"){
    val r1 = Singleton(SEql(1))
    val r2 = Star(r1)
    assert(Cat(Cat(r1,r2),Cat(r1,r2)).canonicalize !=
           Cat(r1,r2).canonicalize)
    assert(Cat(r1,r2,r1,r2).canonicalize !=
           Cat(r1,r2))
    assert(Cat(Cat(r1,r2),Cat(r1,r2)).canonicalize ==
             Cat(r1,r2,r1,r2).canonicalize)

  }
  test("canonicalize cat case 166") {

    assert(And(Not(Singleton(SEql(1))), Sigma).canonicalize
             != Not(Singleton(SEql(1))))

    assert(Cat(And(Not(Singleton(SMember(1))),
                   Sigma),
               Singleton(SMember("a"))).canonicalize
             != Cat(Not(Singleton(SEql(1))),
                    Singleton(SEql("a"))))

    assert(Star(Cat(And(Not(Singleton(SMember(1))),
                        Sigma),
                    Singleton(SMember("a")))).canonicalize
             != Star(Cat(Not(Singleton(SEql(1))),
                         Singleton(SEql("a")))))
  }
  test("cat conversion3"){
    // detect contains EmptySet
    val a = Singleton(SEql("a"))
    val x = Singleton(SEql("x"))
    assert(Cat(a,EmptySet,x).conversion3()
           == EmptySet)
  }
  test("cat conversion4"){
    // remove EmptyWord and flatten Cat(Cat(...)...)
    val a = Singleton(SEql("a"))
    val x = Singleton(SEql("x"))

    assert(Cat(a,EmptyWord,a,Cat(a,x),x).conversion4()
           == Cat(a,a,a,x,x).conversion4())
  }
  test("cat conversion5"){
    //  Cat(..., x*, x, x* ...) --> Cat(..., x*, x, ...)
    //  and Cat(..., x*, x* ...) --> Cat(..., x*, ...)
    val a = Singleton(SEql("a"))
    val x = Singleton(SEql("x"))
    assert(Cat(a,Star(x),x,Star(x),a).conversion5()
           == Cat(a,x,Star(x),a).conversion5())
    assert(Cat(a,Star(x),Star(x),a).conversion5()
           == Cat(a,Star(x),a))
  }
  test("cat conversion6"){
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    val c = Singleton(SEql("c"))
    val d = Singleton(SEql("d"))
    val x = Singleton(SEql("x"))
    // Cat(A,B,X*,X,C,D) --> Cat(A,B,X,X*,C,D)
    assert(Cat(a,b,Star(x),x,c,d).conversion6()
           == Cat(a,b,x,Star(x),c,d))
  }
  test("cat conversion"){

  }
}