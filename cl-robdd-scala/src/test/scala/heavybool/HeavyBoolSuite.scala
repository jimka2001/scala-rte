package heavybool

import adjuvant.MyFunSuite
import HeavyBool._

class HeavyBoolSuite extends MyFunSuite {
  test("and") {
    forallM[Int](
      LazyList.range(1,10,3),
      (n: Int) => HeavyBool(n % 2 != 0,
                            List(Map("forall" -> true)))
    ) &&
       existsM[Int](LazyList.range(1,10,3),
      (n: Int) => HeavyBool(n % 2 != 0,
                            List(Map("exists" -> true))))
  }

  test("or") {
    forallM[Int](
      LazyList.range(1,10,3),
      (n: Int) => HeavyBool(n % 2 != 0, List(Map("forall" -> true)))
    ) ||
       existsM[Int](LazyList.range(1,10,3), (n: Int) =>
        HeavyBool(n % 2 != 0, List(Map("exists" -> true))))
  }

  test("forall"){
    assert(HTrue == forallM(LazyList(1,2,3), (x:Int) =>
      (if (x > 0)
        HeavyBool(true, List(Map("reason" -> "works")))
      else
        HeavyBool(false, List(Map("reason" -> "fails"))))))

    val result = forallM(LazyList(1,2,3), (x:Int) =>
      (if (x > 10)
        HeavyTrue(List(Map("reason" -> "works")))
      else
        HeavyFalse(List(Map("reason" -> "fails")))))
    assert(result.toBoolean == false)
    assert(result.because.head("witness") == 1)
  }
}
