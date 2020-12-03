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

package clcompat

import cl.CLcompat._
import org.scalatest.funsuite.AnyFunSuite

class CLcompatTestSuite extends AnyFunSuite {
  type T= Int=>Nothing
  assert(42 == block{ret:T => ret(42)})
  assert(43 == block{ret:T => 43})
  assert(44 == block{ret1:T =>
    block{ret2:T =>
      ret1(44)
    }})

  assert(45 == block{ret1:T =>
    block{ret2:T =>
      ret1(45)
    }
    ret1(46)})

  assert(48 == block{ret1:T =>
    block{ret2:T =>
      ret2(47)
    }
    ret1(48)})

  assert(49 == block{ret1:T =>
    block{ret2:T =>
      ret1(49)
    }
    assert(false, "this line should never be reached")
  })
}
