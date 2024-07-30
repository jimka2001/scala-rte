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

package bdd

import adjuvant.AdjFunSuite
import bdd.Bdd._
import org.scalatest.funsuite.AnyFunSuite

class AssignmentTestSuite extends AdjFunSuite {
  test("evaluate") {
    withNewBddHash {
      val bdd = And(1, Not(Or(2, 3, 4)))
      assert(true == bdd(Assignment(Set(1))))
      Set(1, 2, 3, 4).subsets().foreach { subset =>
        if (subset == Set(1))
          assert(bdd(Assignment(subset)) == true)
        else
          assert(bdd(Assignment(subset)) == false)
      }
    }
  }

  test("bitsToAssignment") {
    assert(Set() == Assignment(0).trueVariables)
    assert(Set(1) == Assignment(1).trueVariables)
    assert(Set(2) == Assignment(2).trueVariables)
    assert(Set(1, 2) == Assignment(3).trueVariables)
    assert(Set(2, 4) == Assignment(10).trueVariables)
    assert(Set(3, 4) == Assignment(12).trueVariables)
    assert(Set(1, 2, 3, 4, 5) == Assignment(31).trueVariables)
  }

}
