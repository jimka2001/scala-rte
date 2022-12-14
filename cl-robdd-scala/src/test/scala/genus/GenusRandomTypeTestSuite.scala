package genus

import org.scalatest.funsuite.AnyFunSuite

class GenusRandomTypeTestSuite extends AnyFunSuite {
  test("type A") {
    val data = for {depth <- Range(0, 3)
                    _ <- Range(0, 10000)}
      yield RandomType.randomType(depth, Some(false)).inhabited
    assert(data.contains(None) && data.contains(Some(true)) && !data.contains(Some(false)))
  }
  test("type B") {
    val data = for {depth <- Range(0, 3)
                    _ <- Range(0, 10000)}
      yield RandomType.randomType(depth, Some(true)).inhabited
    assert(data.contains(None) && data.contains(Some(true)) && data.contains(Some(false)))
  }
  test("type C") {
    SAtomic.withOpenWorldView {
      val data = for {depth <- Range(0, 3)
                      _ <- Range(0, 10000)}
        yield RandomType.randomType(depth, Some(false)).inhabited
      assert(data.contains(None) && data.contains(Some(true)) && !data.contains(Some(false)))
    }
  }
  test("type D") {
    SAtomic.withOpenWorldView {
      val data = for {depth <- Range(0, 3)
                      _ <- Range(0, 10000)}
        yield RandomType.randomType(depth, Some(true)).inhabited
      assert(data.contains(None) && data.contains(Some(true)) && data.contains(Some(false)))
    }
  }
}
