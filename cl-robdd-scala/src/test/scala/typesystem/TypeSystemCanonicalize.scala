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
import typesystem.Type
import typesystem.Type

class TypeSystemCanonicalize extends FunSuite {

  class Test1
  class Test2 extends Test1
  trait Trait1
  trait Trait2 extends Trait1
  trait Trait3
  trait Trait4
  class Test3 extends Test2 with Trait2
  case class Test4()
  case class Test5() extends Test3
  class Test6
  class Test7

  //  Test2 < Test1
  // Trait2 < Trait1
  //  Test3 < Test2
  //  Test3 < Trait2
  test("disjoint AtomicType with inheritance"){
    println(AtomicType(classOf[Test7]).canonicalize)
  }
}
