// Copyright (c) 2020,21 EPITA Research and Development Laboratory
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

import adjuvant.AdjFunSuite
import org.scalatest.funsuite.AnyFunSuite

class GenusInhabited extends AdjFunSuite {
  class Abstract1
  class Abstract2
  trait Trait1
  trait Trait2
  trait Trait3
  class Test1 extends Abstract1 with Trait1 with Trait2 with Trait3

  test("inhabited SEql"){
    assert(SEql(3).inhabited.contains(true))
    assert(SEql(false).inhabited.contains(true))
    assert(SEql("hello").inhabited.contains(true))
  }
  test("inhabited SMember"){
    assert(SMember().inhabited.contains(false))
    assert(SMember(3).inhabited.contains(true))
    assert(SMember(false).inhabited.contains(true))
    assert(SMember("hello").inhabited.contains(true))
    assert(SMember("hello","world").inhabited.contains(true))
    assert(SMember("hello",3,false).inhabited.contains(true))
  }
  test("inhabited SNot"){
    assert(SNot(SEmpty).inhabited.contains(true))
    assert(SNot(STop).inhabited.contains(false))
    assert(SNot(SEql(3)).inhabited.contains(true))
    assert(SNot(SEql(false)).inhabited.contains(true))
    assert(SNot(SEql("hello")).inhabited.contains(true))
    assert(SNot(SMember()).inhabited.contains(true))
    assert(SNot(SMember(3)).inhabited.contains(true))
    assert(SNot(SMember(false)).inhabited.contains(true))
    assert(SNot(SMember("hello")).inhabited.contains(true))
    assert(SNot(SMember("hello","world")).inhabited.contains(true))
    assert(SNot(SMember("hello",3,false)).inhabited.contains(true))
  }
  test("inhabited union"){
    assert(SEmpty.inhabited.contains(false))
    assert(STop.inhabited.contains(true))
    assert(SOr(SEmpty,STop).inhabited.contains(true))
    assert(SOr(SMember(),SMember(1,2,3)).inhabited.contains(true))
    assert(SOr(SMember(2,3,4),SMember(1,2,3)).inhabited.contains(true))
  }
  test("inhabited intersection") {
    assert(SAnd(SEmpty,STop).inhabited.contains(false))
    assert(SAnd(SMember(),SMember(1,2,3)).inhabited.contains(false))
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
