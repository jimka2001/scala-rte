// Copyright (c) 2019 EPITA Research and Development Laboratory
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

package accumulators

import accumulators.Accumulators._
import org.scalatest._

class AccumulatorSuite extends FunSuite {
  
  test("successor"){

    val primes = List(11,13,17,19)
    val odds = Array(3,5,7)


    val s1 = successor(0)
    val accumulated = for {
      w <- primes
      x = s1(_+3)
      s2 = successor(false)
      y <- odds
      z = s2(!_)
    } yield (w,x,y,z)

    assert( accumulated == List((11,3,3,true), (11,3,5,false), (11,3,7,true), (13,6,3,true), (13,6,5,false), (13,6,7,true),
                                (17,9,3,true), (17,9,5,false), (17,9,7,true), (19,12,3,true), (19,12,5,false), (19,12,7,true)))
  }
  test("collector") {
    assert(List(1, 2, 3) == withCollector[Int](collect =>
                                                 (1 to 3).foreach { i => collect(i) }
                                               ))
  }
  test("maximize") {
    assert(10 == withMaximizer[Int](0)(
                                        maximize => {
                                          maximize(1)
                                          maximize(3)
                                          maximize(10)
                                          maximize(4)
                                        }))
  }
  test("minimize") {
    assert(-10 == withMinimizer[Int](0)(
                                         maximize => {
                                           maximize(-1)
                                           maximize(-3)
                                           maximize(-10)
                                           maximize(-4)
                                         }))
  }
  test("nconc") {
    assert(List(1, 2, 3, 4, 5, 6) == withNconc[Int](
                                                     nconc => {
                                                       nconc(List(1, 2, 3))
                                                       nconc(List(4, 5))
                                                       nconc(List(6))
                                                     }
                                                     ))
  }
  test("summer") {
    assert(8 == withSummer[Int](0, (a: Int, b: Int) => a + b)(summing => {
      summing(1)
      summing(3)
      summing(4)
    }))
  }
  test("printer") {
    assert("hello world" == withOutputToString(printer => {
      printer("hello")
      printer(" ")
      printer("world")
    }))
  }

}


