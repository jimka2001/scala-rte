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

import Types._

class Test1

class Test2 extends Test1

trait Trait1

trait Trait2 extends Trait1

class Test3 extends Test2 with Trait2

class GenusSubtypep extends AnyFunSuite {
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

    assert(SAtomic(Integer).subtypep(SAtomic(Number)) == Some(true))
    assert(SAtomic(Long).subtypep(SAtomic(Long)) == Some(true))
    assert(SAtomic(Number).subtypep(SAtomic(Integer)) == Some(false))
    assert(SAtomic(Integer).subtypep(SAtomic(Long)) == Some(false))
    assert(SAtomic(Long).subtypep(SAtomic(Integer)) == Some(false))
  }

  test("subtypep Scala types") {
    assert(SAtomic(classOf[Test2]).subtypep(SAtomic(classOf[Test2])) == Some(true),
           "Test2 < Test2")
    assert(SAtomic(classOf[Test1]).subtypep(SAtomic(classOf[Test1])) == Some(true),
           "Test1 < Test1")
    assert(SAtomic(classOf[Test2]).subtypep(SAtomic(classOf[Test1])) == Some(true),
           "Test2 < Test1")
    assert(SAtomic(classOf[Test1]).subtypep(SAtomic(classOf[Test2])) == Some(false),
           "not Test1 < Test2")
  }

  test("subtypep Scala traits"){
    // every type is a subtype of itself
    assert(SAtomic(classOf[Trait1]).subtypep(SAtomic(classOf[Trait1])) == Some(true))
    assert(SAtomic(classOf[Trait2]).subtypep(SAtomic(classOf[Trait2])) == Some(true))
    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Test3])) == Some(true))


    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Trait1])) == Some(true))
    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Trait2])) == Some(true))
    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Test2])) == Some(true))
    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Test1])) == Some(true))
    assert(SAtomic(classOf[Trait2]).subtypep(SAtomic(classOf[Trait1])) == Some(true))

    assert(SAtomic(classOf[Trait1]).subtypep(SAtomic(classOf[Test3])) == Some(false))
    assert(SAtomic(classOf[Trait2]).subtypep(SAtomic(classOf[Test3])) == Some(false))
    assert(SAtomic(classOf[Test2]).subtypep(SAtomic(classOf[Test3])) == Some(false))
    assert(SAtomic(classOf[Test1]).subtypep(SAtomic(classOf[Test3])) == Some(false))
    assert(SAtomic(classOf[Trait1]).subtypep(SAtomic(classOf[Trait2])) == Some(false))
  }

  test("AtomicType subtype of union"){
    assert(SAtomic(classOf[Test3]).subtypep(SOr(SAtomic(classOf[Trait1]),
                                                SAtomic(classOf[Trait2]),
                                                SAtomic(classOf[Test1]))) == Some(true))

    assert(SAtomic(classOf[Trait2]).subtypep(SOr(SAtomic(classOf[Test2]),
                                                 SAtomic(classOf[Trait1]))) == Some(true))

    assert(SAtomic(classOf[Test1]).subtypep(SOr(SAtomic(classOf[Test2]),
                                                SAtomic(classOf[Trait1]))) != Some(true))

    // Test1 is disjoint from Integer and also from Long, so it is not a subtype of their union.
    assert(SAtomic(classOf[Test1]).subtypep(SOr(SAtomic(Integer),
                                                SAtomic(Long))) == Some(false))
  }
  test("AtomicType subtype of intersection"){

    //  Test2 < Test1
    // Trait2 < Trait1
    //  Test3 < Test2
    //  Test3 < Trait2
    class Test3 extends Test2 with Trait2
    assert(SAtomic(classOf[Test3]).subtypep(SAnd(SAtomic(classOf[Trait1]),
                                                 SAtomic(classOf[Trait2]),
                                                 SAtomic(classOf[Test1]))) == Some(true))

    assert(SAtomic(classOf[Trait2]).subtypep(SAnd(SAtomic(classOf[Test2]),
                                                  SAtomic(classOf[Trait1]))) != Some(true))

    assert(SAtomic(classOf[Test1]).subtypep(SAnd(SAtomic(classOf[Test2]),
                                                 SAtomic(classOf[Trait1]))) != Some(true))

    // Test1 is disjoint from Integer and also from Long, so it is not a subtype of their intersection.
    assert(SAtomic(classOf[Test1]).subtypep(SAnd(SAtomic(Integer),
                                                 SAtomic(Number),
                                                 SAtomic(Long))) == Some(false))
  }
  test("AtomicType subtype of intersection 2"){
    trait Trait1
    trait Trait2
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Trait2])).subtypep(SEql(1)).contains(false))
  }
  test("AtomicType subtype of member"){
    assert(SAtomic(classOf[Test1]).subtypep(SMember(1, 2, 3)) == Some(false))
  }
  test("AtomicType subtype of eql"){
    assert(SAtomic(classOf[Test1]).subtypep(SEql(1)) == Some(false))
  }
  test("AtomicType subtype of Not"){
    trait Abstract1

    assert(classOf[java.lang.Integer].disjoint(classOf[Abstract1]).contains(true))
    assert(classOf[Abstract1].subtypep(SNot(classOf[java.lang.Integer])).contains(true))
    assert(classOf[java.lang.Integer].subtypep(SNot(classOf[Abstract1])).contains(true))
    assert(! SNot(classOf[java.lang.Integer]).subtypep(classOf[Abstract1]).contains(true))
    assert(! SNot(classOf[Abstract1]).subtypep(classOf[java.lang.Integer]).contains(true))

    trait Abstract2 extends Abstract1

    assert(classOf[Abstract2].subtypep(classOf[Abstract1]).contains(true))
    assert(SNot(classOf[Abstract1]).subtypep(SNot(classOf[Abstract2])).contains(true))

    assert(SNot(classOf[Abstract2]).subtypep(classOf[Abstract1]).contains(false))
  }

  test("Union subtype of Union"){
    val union1 = SOr(SAtomic(Integer),
                     SAtomic(Long))
    // A subtype of A
    assert(union1.subtypep(union1).contains(true))
  }

  test("Union subtype of AtomicType"){
    val union1 = SOr(SAtomic(Integer),
                     SAtomic(Long))
    assert(SAtomic(Integer).subtypep(SAtomic(Number)).contains(true))
    assert(SAtomic(Integer).subtypep(SAtomic(Number)).contains(true))
    assert(union1.subtypep(SAtomic(Number)).contains(true))

    assert(SAtomic(Integer).subtypep(SAtomic(classOf[Test1])).contains(false))
    assert(union1.subtypep(SAtomic(classOf[Test1])).contains(false))
  }
}
