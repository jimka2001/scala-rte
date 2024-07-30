package genus

import adjuvant.AdjFunSuite


class GenusRandomTypeTestSuite extends AdjFunSuite {
  test("type A") {
    val data = for {depth <- Range(0, 3)
                    _ <- Range(0, num_random_tests)}
      yield RandomType.randomType(depth, Some(false)).inhabited
    assert(data.contains(None) && data.contains(Some(true)) && !data.contains(Some(false)))
  }
  test("type B") {
    val data = for {depth <- Range(0, 3)
                    _ <- Range(0, num_random_tests)}
      yield RandomType.randomType(depth, Some(true)).inhabited
    assert(data.contains(None) && data.contains(Some(true)) && data.contains(Some(false)))
  }
  test("type C") {
    SAtomic.withOpenWorldView {
      val data = for {depth <- Range(0, 3)
                      _ <- Range(0, num_random_tests)}
        yield RandomType.randomType(depth, Some(false)).inhabited
      assert(data.contains(None) && data.contains(Some(true)) && !data.contains(Some(false)))
    }
  }
  test("type D") {
    SAtomic.withOpenWorldView {
      val data = for {depth <- Range(0, 3)
                      _ <- Range(0, num_random_tests)}
        yield RandomType.randomType(depth, Some(true)).inhabited
      assert(data.contains(None) && data.contains(Some(true)) && data.contains(Some(false)))
    }
  }
}
