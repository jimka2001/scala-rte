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

    assert( Not(SAtomic(classOf[Integer])) == Not(classOf[Integer]))
    assert( Not(SAtomic(classOf[Long])) != Not(classOf[Integer]))

    assert( And(SAtomic(classOf[Integer])) == And(classOf[Integer]))
    assert( And(SAtomic(classOf[Long])) != And(classOf[Integer]))

    assert( Or(SAtomic(classOf[Integer])) == Or(classOf[Integer]))
    assert( Or(SAtomic(classOf[Long])) != Or(classOf[Integer]))

    assert( Cat(SAtomic(classOf[Integer])) == Cat(classOf[Integer]))
    assert( Cat(SAtomic(classOf[Long])) != Cat(classOf[Integer]))

    assert( Star(SAtomic(classOf[Integer])) == Star(classOf[Integer]))
    assert( Star(SAtomic(classOf[Long])) != Star(classOf[Integer]))
  }
  test("LaTeX"){
    Or(And(SAtomic(classOf[Integer]),
           Not(SAtomic(classOf[Long]))),
       Not(SEql(43))).toLaTeX

    Or(And(classOf[Integer],
                   Not(SAtomic(classOf[Long]))),
               Not(SEql(44))).toLaTeX
  }
}