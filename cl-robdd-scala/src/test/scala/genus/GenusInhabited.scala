// Copyright (c) 2020 EPITA Research and Development Laboratory
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


package genus

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite


class GenusInhabited extends AnyFunSuite {
  class Abstract1
  class Abstract2
  trait Trait1
  trait Trait2
  trait Trait3

  test("inhabited intersection") {
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Abstract1]),
                SAtomic(classOf[Abstract2]),
                SAtomic(classOf[Trait2])
                ).inhabited.contains(false))
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Trait2]),
                SAtomic(classOf[Trait3])).inhabited.contains(true))
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Abstract1]),
                SAtomic(classOf[Trait2]),
                SAtomic(classOf[Trait3])).inhabited.contains(true))

    assert(SAnd(SAtomic(classOf[Trait1]),
                SOr(SAtomic(classOf[Abstract1]),
                    SAtomic(classOf[Abstract2])),
                SAtomic(classOf[Trait2])).inhabited.contains(true))
  }

}
