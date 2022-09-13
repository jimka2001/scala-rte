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

import Types._
import org.scalatest.funsuite.AnyFunSuite

class GenusDisjoint extends AnyFunSuite {

  test("discovered case 135") {
    SAtomic.withOpenWorldView {
      // SAnd(SNot(SAtomic(x)), SNot(SAtomic(y)), SNot(SMember(...)))
      // (and (not Float) (not Double) (not (member 1 2 3))) --> inhabited=true
      // (and (not Float) java.lang.Comparable) -> inhabited=true
      // (and (not Float) (not (member 1 2 3)) java.lang.Comparable) -> inhabited=true
      assert(SAtomic.isInterface(classOf[java.lang.Comparable[_]]))
      assert(SNot(SAtomic(classOf[Number])).inhabited.contains(true))
      assert(SNot(SMember(1, 2, 3)).inhabited.contains(true))
      assert(SAtomic(classOf[java.lang.Comparable[_]]).inhabited.contains(true))
      assert(SAnd(SNot(SAtomic(classOf[Number])),
                  SNot(SMember(1, 2, 3)),
                  SAtomic(classOf[java.lang.Comparable[_]]))
               .inhabited
               .contains(true))
      assert(SAnd(SNot(SAtomic(classOf[Number])), SNot(SMember(1, 2, 3)), SAtomic(classOf[java.lang.Comparable[_]]))
               .disjoint(SAtomic(classOf[java.lang.Comparable[_]]))
               .contains(false))
      assert(SAnd(SNot(SAtomic(classOf[java.lang.Comparable[_]])),
                  SNot(SAtomic(classOf[java.lang.CharSequence])),
                  SAtomic(classOf[java.math.BigDecimal]))
               .inhabited
               .contains(false))
      assert(SAnd(SNot(SAtomic(classOf[java.lang.CharSequence])),
                  SAtomic(classOf[java.math.BigDecimal]))
               .inhabited
               .contains(true))
      assert(SAtomic(classOf[java.math.BigDecimal]).inhabited.contains(true))
      assert(SAnd(SNot(SAtomic(classOf[java.lang.CharSequence])),
                  SAtomic(classOf[java.math.BigDecimal]))
               .disjoint(SAtomic(classOf[java.math.BigDecimal]))
               .contains(false))
    }
  }
  test("disjoint EmptyType") {
    assert(atomicTypesSeq.forall(_.disjoint(SEmpty).contains(true)))
    assert(atomicTypesSeq.forall(SEmpty.disjoint(_).contains(true)))
    assert(SEmpty.disjoint(SEmpty).contains(true))
    assert(SEmpty.disjoint(STop).contains(true))
  }

  test("disjoint SuperType") {
    assert(atomicTypesSeq.forall(STop.disjoint(_).contains(false)))
    assert(atomicTypesSeq.forall(_.disjoint(STop).contains(false)))
    assert(STop.disjoint(SEmpty).contains(true))
    assert(STop.disjoint(STop).contains(false))
  }

  test("disjoint AtomicType") {
    SAtomic.withOpenWorldView {
      val numericType = SAtomic(classOf[java.lang.Number])

      assert(!numericType.disjoint(numericType).getOrElse(true))
      assert(!numericType.disjoint(intJavaType).getOrElse(true))
      assert(!numericType.disjoint(doubleJavaType).getOrElse(true))
      assert(!intJavaType.disjoint(numericType).getOrElse(true))
      assert(!doubleJavaType.disjoint(numericType).getOrElse(true))
      assert(numericType.disjoint(stringType).getOrElse(true))
      assert(stringType.disjoint(numericType).getOrElse(true))
    }
  }

  test("disjoint Member") {
    val m1 = SMember(1, 2, 3, 4)
    val m2 = SMember(42, "test")

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
    val m1 = SMember(1, 2, 3, 4)
    val m2 = SMember(42, "test")
    val u = SOr(m1, m2)

    assert(! u.disjoint(m1).getOrElse(true))
    assert(! u.disjoint(m2).getOrElse(true))
    assert(! m1.disjoint(u).getOrElse(true))
    assert(! m2.disjoint(u).getOrElse(true))
  }

  test("disjoint intersection") {
    val m1 = SMember(1, 2, 3, 4)
    val m2 = SMember(3, 42, "test", 2)
    val m3 = SMember(0)
    val inter = SAnd(m1, m2)
    val inter2 = SAnd(inter, m3)

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
    SOr(intJavaType, stringType).disjoint(SOr(charJavaType, booleanType))
    SAnd(SOr(intJavaType, doubleJavaType), stringType).disjoint(stringType)
  }
  class Test1
  class Test2 extends Test1
  trait Trait1
  trait Trait2 extends Trait1
  trait Trait3
  trait Trait4
  class Test3 extends Test2 with Trait2 with Trait4
  case class Test4()
  case class Test5() extends Test3
  class Test6
  class Test7
  abstract class Abstract1
  abstract class Abstract2
  class Test8 extends Abstract1
  class Test9 extends Abstract1 with Trait1 with Trait2
  test("disjoint and with classes"){
    assert(SAtomic(classOf[Trait1]).inhabited.contains(true))
    assert(SAtomic(classOf[Trait2]).inhabited.contains(true))
    assert(SAtomic(classOf[Trait4]).inhabited.contains(true))
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Trait2]),
                SAtomic(classOf[Trait4])).inhabited.contains(true))
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Trait2]),
                SAtomic(classOf[Trait4]))
           .disjoint(SAtomic(classOf[Trait2])).contains(false))

    // (disjoint? (and A B C) D)   where B disjoint with D
    assert(SAtomic(classOf[Abstract1]).disjoint(SAtomic(classOf[Abstract2])).contains(true))
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Abstract1]),
                SAtomic(classOf[Trait2]))
             // disjoint because Abstract1 and Abstract2 are disjoint
           .disjoint(SAtomic(classOf[Abstract2])).contains(true))

    // (disjoint? (and B C) A)
    // (disjoint? (and String (not (member a b c 1 2 3))) java.lang.Comparable)
    assert(SAtomic(classOf[Abstract1]).subtypep(SOr(SAtomic(classOf[Abstract1]),
                                                    SAtomic(classOf[Abstract2]))).contains(true))

    assert(SAtomic(classOf[Abstract1]).inhabited.contains(true))
    assert(SAtomic(classOf[Trait1]).inhabited.contains(true))
    assert(SOr(SAtomic(classOf[Abstract1]),
               SAtomic(classOf[Abstract2])).inhabited.contains(true))
    assert(SAtomic(classOf[Trait2]).inhabited.contains(true))
    assert(SAnd(SAtomic(classOf[Trait1]),
                SOr(SAtomic(classOf[Abstract1]),
                    SAtomic(classOf[Abstract2])),
                SAtomic(classOf[Trait2])).inhabited.contains(true))
    assert(SAnd(SAtomic(classOf[Trait1]),
                SOr(SAtomic(classOf[Abstract1]),
                    SAtomic(classOf[Abstract2])),
                SAtomic(classOf[Trait2])).disjoint(SAtomic(classOf[Abstract1])).contains(false))
  }

  test("disjoint AtomicType with inheritance") {
    SAtomic.withOpenWorldView {
      // one assignable from the other
      assert(SAtomic(classOf[Test2]).disjoint(SAtomic(classOf[Test1])).contains(false))
      assert(SAtomic(classOf[Test1]).disjoint(SAtomic(classOf[Test2])).contains(false))
      assert(SAtomic(classOf[Test3]).disjoint(SAtomic(classOf[Test2])).contains(false))
      assert(SAtomic(classOf[Test3]).disjoint(SAtomic(classOf[Trait2])).contains(false))
      assert(SAtomic(classOf[Trait1]).disjoint(SAtomic(classOf[Trait2])).contains(false))

      // either is final
      assert(SAtomic(classOf[Test5]).disjoint(SAtomic(classOf[Test5])).contains(false))
      assert(SAtomic(classOf[Test5]).disjoint(SAtomic(classOf[Test3])).contains(false))
      assert(SAtomic(classOf[Test4]).disjoint(SAtomic(classOf[Test3])).contains(true))
      assert(SAtomic(classOf[Test3]).disjoint(SAtomic(classOf[Test4])).contains(true))

      // either is a trait
      assert(SAtomic(classOf[Trait2]).disjoint(SAtomic(classOf[Test1])).contains(false))
      assert(SAtomic(classOf[Trait2]).disjoint(SAtomic(classOf[Trait3])).contains(false))
      assert(SAtomic(classOf[Test4]).disjoint(SAtomic(classOf[Test5])).contains(true))
      assert(SAtomic(classOf[Test4]).disjoint(SAtomic(classOf[Test4])).contains(false))
      assert(SAtomic(classOf[Test1]).disjoint(SAtomic(classOf[Trait3])).contains(false))
      assert(SAtomic(classOf[Trait4]).disjoint(SAtomic(classOf[Trait3])).contains(false))

      // neither is trait nor superclass nor final
      assert(SAtomic(classOf[Test6]).disjoint(SAtomic(classOf[Test7])).contains(true))
    }
  }
  test("boolean disjoint"){
    val b = SAtomic(classOf[Boolean])

    assert(SMember(true,false).disjoint(oddType).contains(true))
    assert(b.disjoint(oddType).contains(true))
  }
}
