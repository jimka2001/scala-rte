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


package typesystem

import org.scalatest._
import typesystem.TypeSystem._


class TypeSystemDisjoint extends FunSuite {

  test("disjoint EmptyType") {
    assert(atomicTypesSeq.forall(_.disjoint(EmptyType).get))
    assert(atomicTypesSeq.forall(EmptyType.disjoint(_).get))
    assert(EmptyType.disjoint(EmptyType).get)
    assert(EmptyType.disjoint(SuperType).get)
  }

  test("disjoint SuperType") {
    assert(atomicTypesSeq.forall(! SuperType.disjoint(_).get))
    assert(atomicTypesSeq.forall(! _.disjoint(SuperType).get))
    assert(SuperType.disjoint(EmptyType).get)
    assert(! SuperType.disjoint(SuperType).get)
  }
}
