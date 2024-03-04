package heavybool

import adjuvant.MyFunSuite
import heavybool.HeavyBool._
import heavybool.Relations.{isEquivalence, isSymmetric, isTransitive, isReflexive}

class RelationsSuite extends MyFunSuite {
  test("symmetric") {
    assert(existsM("a < 12", LazyList.range(1, 20)) { (a: Int) => HeavyBool(a < 12) }.toBoolean)
    assert(!isSymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(!isSymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isSymmetric(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
  }
  test("transitive") {
    assert(isTransitive(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(isTransitive(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isTransitive(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
  }
  test("reflexive"){
    assert(isReflexive(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
    assert(isReflexive(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(!isReflexive(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)

  }
  test("equivalence") {
    assert(!isEquivalence(LazyList.range(1, 20), (a: Int, b: Int) => a < b).toBoolean)
    assert(!isEquivalence(LazyList.range(1, 20), (a: Int, b: Int) => a <= b).toBoolean)
    assert(isEquivalence(LazyList.range(1, 20), (a: Int, b: Int) => a == b).toBoolean)
  }
  
}
