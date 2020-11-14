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
import typesystem.Types._

class Test1

class Test2 extends Test1

trait Trait1

trait Trait2 extends Trait1

class Test3 extends Test2 with Trait2

class TypeSystemSubtypep extends FunSuite {
  val Long = classOf[java.lang.Long]
  val Integer = classOf[java.lang.Integer]
  val Number = classOf[java.lang.Number]

  test("subtypep Java types") {
    // superclass.isAssignableFrom(subclass)
    assert(Number.isAssignableFrom(Integer))
    assert(Number.isAssignableFrom(Long))
    assert(! Integer.isAssignableFrom(Number))
    assert(! Long.isAssignableFrom(Integer))
    assert(! Integer.isAssignableFrom(Long))

    assert(AtomicType(Integer).subtypep(AtomicType(Number)) == Some(true))
    assert(AtomicType(Long).subtypep(AtomicType(Long)) == Some(true))
    assert(AtomicType(Number).subtypep(AtomicType(Integer)) == Some(false))
    assert(AtomicType(Integer).subtypep(AtomicType(Long)) == Some(false))
    assert(AtomicType(Long).subtypep(AtomicType(Integer)) == Some(false))
  }

  test("subtypep Scala types") {
    assert(AtomicType(classOf[Test2]).subtypep(AtomicType(classOf[Test2])) == Some(true),
           "Test2 < Test2")
    assert(AtomicType(classOf[Test1]).subtypep(AtomicType(classOf[Test1])) == Some(true),
           "Test1 < Test1")
    assert(AtomicType(classOf[Test2]).subtypep(AtomicType(classOf[Test1])) == Some(true),
           "Test2 < Test1")
    assert(AtomicType(classOf[Test1]).subtypep(AtomicType(classOf[Test2])) == Some(false),
           "not Test1 < Test2")
  }

  test("subtypep Scala traits"){
    // every type is a subtype of itself
    assert(AtomicType(classOf[Trait1]).subtypep(AtomicType(classOf[Trait1])) == Some(true))
    assert(AtomicType(classOf[Trait2]).subtypep(AtomicType(classOf[Trait2])) == Some(true))
    assert(AtomicType(classOf[Test3]).subtypep(AtomicType(classOf[Test3])) == Some(true))


    assert(AtomicType(classOf[Test3]).subtypep(AtomicType(classOf[Trait1])) == Some(true))
    assert(AtomicType(classOf[Test3]).subtypep(AtomicType(classOf[Trait2])) == Some(true))
    assert(AtomicType(classOf[Test3]).subtypep(AtomicType(classOf[Test2])) == Some(true))
    assert(AtomicType(classOf[Test3]).subtypep(AtomicType(classOf[Test1])) == Some(true))
    assert(AtomicType(classOf[Trait2]).subtypep(AtomicType(classOf[Trait1])) == Some(true))

    assert(AtomicType(classOf[Trait1]).subtypep(AtomicType(classOf[Test3])) == Some(false))
    assert(AtomicType(classOf[Trait2]).subtypep(AtomicType(classOf[Test3])) == Some(false))
    assert(AtomicType(classOf[Test2]).subtypep(AtomicType(classOf[Test3])) == Some(false))
    assert(AtomicType(classOf[Test1]).subtypep(AtomicType(classOf[Test3])) == Some(false))
    assert(AtomicType(classOf[Trait1]).subtypep(AtomicType(classOf[Trait2])) == Some(false))
  }
}