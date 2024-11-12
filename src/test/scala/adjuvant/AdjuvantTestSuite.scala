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

package adjuvant

import adjuvant.Adjuvant._
import org.scalatest.funsuite.AnyFunSuite

class AdjuvantTestSuite extends AdjFunSuite {
  test("printTime"){
    def makeTime(ns:Long, us:Long, ms:Long, sec:Long, min:Long, hour:Long):Long = {
      (ns
      + us * 1000L
      + ms * 1000L * 1000L
      + sec * 1000L * 1000L * 1000L
      + min * 60L * 1000L * 1000L * 1000L
      + hour * 60L * 60L * 1000L * 1000L * 1000L)
    }
    assert("1 ns" == printTime(1))
    assert("3 us" == printTime(3007))
    assert("4 ms" == printTime(4L * 1000 * 1000 + 5L * 1000 + 31L))
    assert("4 ms" == printTime(makeTime(31, 5, 4, 0, 0, 0)))
    assert("31 sec 341 ms" == printTime(makeTime(ns=12, us=13, ms=341, sec=31, min=0, hour=0)))
    assert("11 min 43 sec" == printTime(makeTime(ns=5, us=6, ms=7, sec=43, min=11, hour=0)))
    assert("5 hours 4 min 3 sec" == printTime(makeTime(ns=0, us=0, ms=0, sec=3, min=4, hour=5)))

  }
  test("conj"){
    val l1 = List( 1, 2, 3)
    val v1 = Vector(1,2,3)
    val s1 = Seq(1,2,3)

    assert(conj(l1,4).contains(2))
    assert(conj(l1,4).contains(4))

    assert(conj(v1,4).contains(2))
    assert(conj(v1,4).contains(4))

    assert(conj(s1,4).contains(2))
    assert(conj(s1,4).contains(4))
  }
  test("uniquify"){
    assert(uniquify(List()) == List())
    assert(uniquify(List(1)) == List(1))
    assert(uniquify(List(1,2)) == List(1,2))
    assert(uniquify(List(1,2,3)) == List(1,2,3))
    assert(uniquify(List(1,2,2,3)) == List(1,2,3))
    assert(uniquify(List(3,1,2,2,3)) == List(1,2,3) ||
             uniquify(List(3,1,2,2,3)) == List(3,1,2))

    assert(uniquify(Vector()) == Vector())
    assert(uniquify(Vector(1)) == Vector(1))
    assert(uniquify(Vector(1,2)) == Vector(1,2))
    assert(uniquify(Vector(1,2,3)) == Vector(1,2,3))
    assert(uniquify(Vector(1,2,2,3)) == Vector(1,2,3))
    assert(uniquify(Vector(3,1,2,2,3)) == Vector(1,2,3) ||
             uniquify(Vector(3,1,2,2,3)) == Vector(3,1,2))

  }
  test("searchReplace"){
    assert(searchReplace(Seq(1,2,3),2,20)
             == Seq(1,20,3))
    assert(searchReplace(Seq(1,2,3),2,Seq(20))
             == Seq(1,20,3))
    assert(searchReplace(Seq(1,2,3),2,Seq())
             == Seq(1,3))
    assert(searchReplace(Seq(1,2,3),2,Seq(20,21,22))
    == Seq(1,20,21,22,3))
  }
  test("traceGraph"){
    def edges(i:Int):Seq[(String,Int)] = {
      if(i == 0)
        Seq(("a",1),("b",2))
      else if (i == 1)
        Seq(("a",2))
      else
        Seq(("b",2),("a",0))
    }
    traceGraph(0,edges)
  }

  test("eql"){
    import adjuvant.Adjuvant.eql
    assert(eql(1,1))
    assert(! eql(1, 1.0))
    assert(! eql(1:Short, 1:Long))
  }

  test("diff"){
    import adjuvant.Adjuvant.{diff,eql}
    assert(diff(Seq(1,2,3),Seq(1L, 2L, 3L)) == Seq(1,2,3))
    assert(eql(diff(Seq(1,2,3),Seq(1L, 2L, 3L)), Seq(1,2,3)))
    assert(! eql(diff(Seq(1,2,3),Seq(1L, 2L, 3L)), Seq[Any](1.0,2,3)))
  }

//  test("xyzzy"){
//    val one:Long = 1
//    one match {
//      case _:Int => true
//      case _ => false
//    }
//  }
}


