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

import adjuvant.MyFunSuite
import genus.RandomType.interestingValues
import genus.Types._
import org.scalatest.funsuite.AnyFunSuite

class GenusTypep extends MyFunSuite {
  test("SAtomic types"){
    for{ v <- interestingValues }
      assert(SAtomic(v.getClass).typep(v))
  }
  test("AtomicType any and nothing") {
    val newEmpty = SAtomic(classOf[Nothing])
    val newSuper = SAtomic(classOf[Any])

    assert(SEmpty == newEmpty)
    assert(SEmpty == nothingType())
    assert(STop == newSuper)
    assert(STop == anyType())
  }

  test("typep of any") {
    assert(anyType().typep("test"))
    assert(anyType().typep(42))
    assert(anyType().typep(-1.5))
    assert(anyType().typep(List(1, 2, 3)))
    assert(anyType().typep(classOf[Any]))
  }

  test("typep of nothing") {
    assert(! nothingType().typep("test"))
    assert(! nothingType().typep(42))
    assert(! nothingType().typep(-1.5))
    assert(! nothingType().typep(List(1, 2, 3)))
    assert(! nothingType().typep(classOf[Any]))
  }

  test("typep of java int") {
    assert(intJavaType().typep(42))
    assert(! intJavaType().typep("test"))
  }

  test("typep of string") {
    assert(stringType().typep("test"))
    assert(! stringType().typep(42))
  }

  test("typep of list int") {
    assert(listAnyType().typep(List(1, 2, 3)))
    // Because of type erasure
    assert(listAnyType().typep(List[Double](1.1, 0.42)))
    assert(listAnyType().typep(List[Any]("test", 2, "coucou")))
  }

  test("typep of a UnionType") {
    val unionIntType = SOr(intJavaType(), intType())
    val unionAnyString = SOr(stringType(), anyType(), nothingType())
    val unionStringUnitBoolean = SOr(stringType(), unitRuntimeType(), booleanType())
    val unionCharInt = SOr(charJavaType(), intJavaType())

    val x: Unit = {}

    assert(unionIntType.typep(42))
    assert(! unionIntType.typep(0.42))
    assert(unionAnyString.typep(x))
    assert(unionAnyString.typep(42))
    assert(unionAnyString.typep("test"))
    assert(unionStringUnitBoolean.typep(true))
    assert(unionStringUnitBoolean.typep("test"))
    assert(unionStringUnitBoolean.typep(x))
    assert(! unionStringUnitBoolean.typep(42))
    assert(! unionStringUnitBoolean.typep(List(1, 2, 3)))
    assert(unionCharInt.typep(42))
    assert(unionCharInt.typep('t'))
    assert(! unionCharInt.typep("test"))
  }

  test("typep of an IntersectionType") {
    val interAnyRefList = SAnd(anyRefType(), listAnyType())
    val interIntStringUnit = SAnd(intJavaType(), stringType(), unitRuntimeType())
    val interAnyRefListNothing = SAnd(anyRefType(), listAnyType(), nothingType())
    val interStringAny = SAnd(stringType(), anyType())
    val interNumericJavaInt = SAnd(numericType(), intJavaType())

    val javaString: java.lang.String = "coucou"

    assert(interAnyRefList.typep(List(1, 2, 3)))
    assert(! interAnyRefList.typep(javaString))
    assert(! interIntStringUnit.typep(42))
    assert(! interIntStringUnit.typep("test"))
    assert(! interAnyRefListNothing.typep(List(1, 2, 3)))
    assert(interStringAny.typep("test"))
    assert(! interStringAny.typep(42))
    assert(interNumericJavaInt.typep(42))
    assert(! interNumericJavaInt.typep(0.42))
  }

  test("typep EqlType and Union") {
    val t = SOr(stringType(), SEql(42))

    assert(t.typep("test"))
    assert(t.typep(42))
    assert(! t.typep(1))
  }

  test("typep even and odd") {
    val even = List(0, 2, 4, 6, 10, 42, 100, 550, -42)
    val odd = List(1, 3, 7, -5, 9, 43, 101)

    assert(even.forall(evenType().typep(_)))
    assert(odd.forall(oddType().typep(_)))
    assert(odd.forall(! evenType().typep(_)))
    assert(even.forall(! oddType().typep(_)))
    assert(! oddType().typep("test"))
    assert(! evenType().typep("test"))
    assert((even ++ odd).forall(SOr(oddType(), evenType()).typep(_)))
    assert((even ++ odd).forall(! SAnd(oddType(), evenType()).typep(_)))
  }

  test("typep prime type") {
    val primes = List(2, 3, 5, 7, 11, 13, 79, 97, 983, 991, 521)
    val notPrimes = List(4, 6,  8, 9, 42, 867, 913)

    assert(primes.forall(primeType().typep(_)))
    assert(notPrimes.forall(! primeType().typep(_)))
  }

  test("typep member") {
    val all = Seq(1, 2, 3, 4, 42, "test")
    val m1 = SMember(1, 2, 3, 4)
    val m2 = SMember(42, "test", 1, 2)
    val u = SOr(m1, m2)
    val n = SAnd(m1, m2)

    assert(m1.typep(1) && m1.typep(2))
    assert(m1.typep(3) && m1.typep(4))
    assert(m2.typep(42) && m2.typep("test"))
    assert(! m1.typep(0) && ! m1.typep(5))
    assert(! m2.typep(40) && ! m2.typep("tzet"))
    assert(all.forall(u.typep))
    assert(n.typep(1) && n.typep(2))
    assert(! n.typep("test") && ! n.typep(42))
  }

  test("typep not") {
    val m1 = SMember(1, 2, 3)
    val m_ = SNot(m1)

    assert(! m_.typep(1) && ! m_.typep(2) && ! m_.typep(3))
    assert(m_.typep("test") && m_.typep(0))
  }
}
