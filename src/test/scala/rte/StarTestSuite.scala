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

import adjuvant.AdjFunSuite
import genus._
import org.scalatest.funsuite.AnyFunSuite

class StarTestSuite extends AdjFunSuite {

  test("canonicalize star") {
    assert(EmptySeq.*.canonicalize == EmptySeq)
    assert(EmptySet.*.canonicalize == EmptySeq)

    for {depth <- 0 to 5
         _ <- 1 to num_random_tests
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         } {
      assert(Star(Star(r1)).canonicalize == Star(r1).canonicalize)
      assert(Star(r1).canonicalize == Star(r1.canonicalize).canonicalize)
      assert(Star(Cat(r1,Star(r1))).canonicalize == Star(r1).canonicalize) // (x x*)* --> x*
      assert(Star(Cat(Star(r1),r1)).canonicalize == Star(r1).canonicalize) // (x* x)* --> x*
      assert(Star(Cat(r1,r1,Star(Cat(r1,r1)))).canonicalize == Star(Cat(r1,r1)).canonicalize) // (x x (x x)*)* --> (x x)*
      assert(Star(Cat(r1,r2,Star(Cat(r1,r2)))).canonicalize == Star(Cat(r1,r2)).canonicalize) // (x x (x x)*)* --> (x x)*
      assert(Star(Cat(Star(Cat(r1,r2)),r1,r2)).canonicalize == Star(Cat(r1,r2)).canonicalize) // ((x y)* x y)* --> (x y)*
    }
  }
  test("star conversion1"){
    assert(Star(EmptySeq).conversion1() == EmptySeq)
    assert(Star(EmptySet).conversion1() == EmptySeq)
    val a = Singleton(SEql("a"))
    assert(Star(Star(a)).conversion1() == Star(a))
  }

  test("star conversion2"){
    // Cat(x x*)* = x*
    // Cat(x* x)* = x*
    // Cat(x* x x*)* = x*
    val a = Singleton(SEql("a"))
    val b = Singleton(SEql("b"))
    assert(Star(Cat(a,Star(a))).conversion2() == Star(a))
    assert(Star(Cat(Star(a),a)).conversion2() == Star(a))
    assert(Star(Cat(Star(a),a,Star(a))).conversion2() == Star(a))

    assert(Star(Cat(a,Star(b))).conversion2() == Star(Cat(a,Star(b))))
    assert(Star(Cat(Star(b),a)).conversion2() == Star(Cat(Star(b),a)))
    assert(Star(Cat(Star(a),b,Star(a))).conversion2() == Star(Cat(Star(a),b,Star(a))))
  }
  test("star conversion3"){
    val x = Singleton(SEql("x"))
    val y = Singleton(SEql("y"))
    val z = Singleton(SEql("z"))
    // Star(Cat(X, Y, Z, Star( Cat(X, Y, Z))))
    //   -->    Star( Cat(X, Y, Z))
    assert(Star(Cat(x,y,z,Star(Cat(x,y,z)))).conversion3()
           == Star(Cat(x,y,z)))

    // Star(Cat(Star( Cat(X, Y, Z)), X, Y, Z))
    //   -->    Star( Cat(X, Y, Z))
    assert(Star(Cat(Star(Cat(x,y,z)),x,y,z)).conversion3()
           == Star(Cat(x,y,z)))

    // Star(Cat(Star( Cat(X, Y, Z)), X, Y, Z, Star(Cat(X,Y,Z)))
    //   -->    Star( Cat(X, Y, Z))
    assert(Star(Cat( Star(Cat(x,y,z)),
                     x,y,z,
                     Star(Cat(x,y,z)))).conversion3()
           == Star(Cat(x,y,z)))

    assert(Star(Cat()).conversion3()
           == Star(Cat()))
    assert(Star(Cat(x)).conversion3()
           == Star(Cat(x)))
  }
}
