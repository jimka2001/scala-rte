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


class TypeSystemDisjoint extends FunSuite {

  test("disjoint EmptyType") {
    assert(atomicTypesSeq.forall(_.disjoint(EmptyType).get))
    assert(atomicTypesSeq.forall(EmptyType.disjoint(_).get))
    assert(EmptyType.disjoint(EmptyType).get)
    assert(EmptyType.disjoint(TopType).get)
  }

  test("disjoint SuperType") {
    assert(atomicTypesSeq.forall(! TopType.disjoint(_).get))
    assert(atomicTypesSeq.forall(! _.disjoint(TopType).get))
    assert(TopType.disjoint(EmptyType).get)
    assert(! TopType.disjoint(TopType).get)
  }

  test("disjoint AtomicType") {
    val numericType = AtomicType(classOf[java.lang.Number])

    assert(! numericType.disjoint(numericType).getOrElse(true))
    assert(! numericType.disjoint(intJavaType).getOrElse(true))
    assert(! numericType.disjoint(doubleJavaType).getOrElse(true))
    assert(! intJavaType.disjoint(numericType).getOrElse(true))
    assert(! doubleJavaType.disjoint(numericType).getOrElse(true))
    assert(numericType.disjoint(stringType).getOrElse(true))
    assert(stringType.disjoint(numericType).getOrElse(true))
  }

  test("disjoint Member") {
    val m1 = MemberType(1, 2, 3, 4)
    val m2 = MemberType(42, "test")

    assert(m1.disjoint(m2).getOrElse(false))
    assert(m2.disjoint(m1).getOrElse(false))
    assert(! m1.disjoint(intJavaType).getOrElse(true))
    assert(m1.disjoint(stringType).getOrElse(false))
    assert(! m2.disjoint(intJavaType).getOrElse(true))
    assert(! m2.disjoint(stringType).getOrElse(true))
    assert(! intJavaType.disjoint(m2).getOrElse(true))
    assert(! stringType.disjoint(m2).getOrElse(true))
  }

  test("disjoint union") {
    val m1 = MemberType(1, 2, 3, 4)
    val m2 = MemberType(42, "test")
    val u = UnionType(m1, m2)

    assert(! u.disjoint(m1).getOrElse(true))
    assert(! u.disjoint(m2).getOrElse(true))
    assert(! m1.disjoint(u).getOrElse(true))
    assert(! m2.disjoint(u).getOrElse(true))
  }

  test("disjoint intersection") {
    val m1 = MemberType(1, 2, 3, 4)
    val m2 = MemberType(3, 42, "test", 2)
    val m3 = MemberType(0)
    val inter = IntersectionType(m1, m2)
    val inter2 = IntersectionType(inter, m3)

    assert(! inter.disjoint(m1).getOrElse(true))
    assert(! inter.disjoint(m2).getOrElse(true))
    assert(! m1.disjoint(inter).getOrElse(true))
    assert(! m2.disjoint(inter).getOrElse(true))
    assert(inter2.disjoint(m1).getOrElse(false))
    assert(inter2.disjoint(m2).getOrElse(false))
    assert(m1.disjoint(inter2).getOrElse(false))
    assert(m2.disjoint(inter2).getOrElse(false))
  }

  test("disjoint unknown") {
    println(UnionType(intJavaType, stringType).disjoint(UnionType(charJavaType, booleanJavaType)))
    println(IntersectionType(UnionType(intJavaType, doubleJavaType), stringType).disjoint(stringType))
  }
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
    // one assignable from the other
    assert(AtomicType(classOf[Test2]).disjoint(AtomicType(classOf[Test1])) == Some(false))
    assert(AtomicType(classOf[Test1]).disjoint(AtomicType(classOf[Test2])) == Some(false))
    assert(AtomicType(classOf[Test3]).disjoint(AtomicType(classOf[Test2])) == Some(false))
    assert(AtomicType(classOf[Test3]).disjoint(AtomicType(classOf[Trait2])) == Some(false))
    assert(AtomicType(classOf[Trait1]).disjoint(AtomicType(classOf[Trait2])) == Some(false))

    // either is final
    assert(AtomicType(classOf[Test5]).disjoint(AtomicType(classOf[Test5])) == Some(false))
    assert(AtomicType(classOf[Test5]).disjoint(AtomicType(classOf[Test3])) == Some(false))
    assert(AtomicType(classOf[Test4]).disjoint(AtomicType(classOf[Test3])) == Some(true))
    assert(AtomicType(classOf[Test3]).disjoint(AtomicType(classOf[Test4])) == Some(true))

    // either is a trait
    assert(AtomicType(classOf[Trait2]).disjoint(AtomicType(classOf[Test1])) == Some(false))
    assert(AtomicType(classOf[Trait2]).disjoint(AtomicType(classOf[Trait3])) == Some(false))
    assert(AtomicType(classOf[Test4]).disjoint(AtomicType(classOf[Test5])) == Some(true))
    assert(AtomicType(classOf[Test4]).disjoint(AtomicType(classOf[Test4])) == Some(false))
    assert(AtomicType(classOf[Test1]).disjoint(AtomicType(classOf[Trait3])) == Some(false))
    assert(AtomicType(classOf[Trait4]).disjoint(AtomicType(classOf[Trait3])) == Some(false))

    // neither is trait nor superclass nor final
    assert(AtomicType(classOf[Test6]).disjoint(AtomicType(classOf[Test7])) == Some(true))
  }
}
