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

import adjuvant.Adjuvant.{eql, fixedPoint}
import adjuvant.MyFunSuite
import genus._
import rte.RteImplicits._
import scala.language.implicitConversions
import rte.Rte.{rteCase,rteIfThenElse}

//
class RteCaseTestSuite extends MyFunSuite {
  test("rteCase"){
    val int = classOf[Int]
    val str = classOf[String]
    var caught = 0
    val f = rteCase(Seq((Star(int),1),
                        (Star(str), 2),
                        (Cat(Star(int), Star(str)),3),
                        (Cat(int, str), 0)),
                    rte => {
                      caught = 1
                      val leftOver = And(Cat(int,str),
                                     Not(Or(Cat(Star(int),
                                                Star(str)),
                                            Star(str),
                                            Star(int))))

                      assert(rte.isomorphic(leftOver).contains(true))
                    })

    assert(caught == 1)
    assert(f(List(1, 2, 3, 4, 5)) == Some(1))
    assert(f(List("one", "two", "three", "four")) == Some(2))
    assert(f(List("one", "two", 3, 4)) == None)
    assert(f(List(1, 2, "three", "four")) == Some(3))
  }

  test("rteIfThenElse") {
    val int = classOf[Int]
    val str = classOf[String]
    var caught = 0
    val f = rteIfThenElse(Seq(
      Star(int) -> (() => {
        1
      }),
      Star(str) -> (() => {
        2
      }),
      Cat(Star(int), Star(str)) -> (() => {
        3
      }),
      Cat(int, str) -> (() => {
        //  case impossible
        0
      })),
                          () => {
                            // default case
                            4
                          },
                          handleUnreachable = (rte => {
                            caught = 1
                          }))
    assert(caught == 1)
    assert(f(List(1, 2, 3, 4, 5)) == 1)
    assert(f(List("one", "two", "three", "four")) == 2)
    assert(f(List("one", "two", 3, 4)) == 4)
    assert(f(List(1, 2, "three", "four")) == 3)
  }
  test("lazy") {
    val int = classOf[Int]
    val str = classOf[String]
    var caught = 0
    val f = rteIfThenElse(Seq(
      Star(int) -> (() => {
        1
      }),
      Star(str) -> (() => {
        2
      }),
      Cat(Star(int), Star(str)) -> (() => {
        3
      }),
      Cat(int, str) -> (() => {
        //  case impossible
        0
      })),
                          () => {
                            // default case
                            4
                          },
                          handleUnreachable = (rte => {
                            caught = 1
                          }))
    assert(caught == 1)
    def data(n:Int):LazyList[Int] = {
      if (n <= 0)
        LazyList[Int]()
      else
        n #:: data(n-1)
    }
    assert(f(data(5)) == 1)
  }
}
