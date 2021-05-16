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

class StarTestSuite extends AnyFunSuite {

  test("canonicalize star") {
    assert(EmptyWord.*.canonicalize == EmptyWord)
    assert(EmptySet.*.canonicalize == EmptyWord)

    for {depth <- 0 to 5
         _ <- 1 to 1000
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


}