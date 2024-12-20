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

class NotTestSuite extends AdjFunSuite {
  test("not canonicalizeOnce"){
    assert(Not(Sigma).canonicalizeOnce == Rte.notSigma)
    assert(Not(Singleton(STop)).canonicalizeOnce == Rte.notSigma)
    assert(Not(Star(Sigma)).canonicalizeOnce == EmptySet)
    assert(Not(EmptySeq).canonicalizeOnce == Rte.notEmptySeq)
    assert(Not(EmptySet).canonicalizeOnce == Star(Sigma))
    assert(Not(Singleton(SEmpty)).canonicalizeOnce == Star(Sigma))
    val x = Singleton(SEql("x"))
    val y = Singleton(SEql("y"))
    // Not(Not(r)) -> Not(r)
    assert(Not(Not(x)).canonicalizeOnce == x)

    // Not(And(a,b)) -> Or(Not(a),Not(b))
    assert(Not(And(x,y)).canonicalizeOnce == Or(Not(x),Not(y)))

    // Not(Or(a,b)) -> And(Not(a),Not(b))
    assert(Not(Or(x,y)).canonicalizeOnce == And(Not(x),Not(y)))
  }
}
