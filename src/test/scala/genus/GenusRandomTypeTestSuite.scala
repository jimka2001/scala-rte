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
    val types = for {depth <- Range(0, 3)
                    _ <- Range(0, num_random_tests)}
      yield RandomType.randomType(depth, Some(true))
    val data = types.map(_.inhabited)
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
  test("sanity check random type"){

    import RandomType.randomType
    (0 to 10).foreach { i =>
      val t = randomType(6)
      val c = t.canonicalize()
      assert(c.typep(42) || SNot(c).typep(42))
    }
  }

  test("sanity check") {
    import NormalForm._
    val a = 2
    val t = SAtomic(classOf[Int])

    //println("type of a = " + a.getClass)
    //println("class of Int = " + classOf[Int])

    assert(t.typep(a))
    class Abstract1
    class Abstract2
    trait Trait1
    trait Trait2
    trait Trait3
    val t0 = SOr(SAtomic(classOf[Abstract1]),
                SAtomic(classOf[Abstract2]))
    val t1 = SAnd(SAtomic(classOf[Trait1]),
                  t0,
                  SAtomic(classOf[Trait2]))
    val t2 = SAnd(SAtomic(classOf[Trait1]),
                  SNot(t0),
                  SAtomic(classOf[Trait2]))
    assert(t1 == SAnd(SEmpty, t0, SEmpty))
    assert(t2 == SAnd(SEmpty, SNot(t0), SEmpty))
    assert(SEmpty == t2.canonicalize(nf = Some(Dnf)))
    assert(SEmpty == t1.canonicalize())
    assert(SEmpty == t1.canonicalize(nf = Some(Dnf)))
    assert(STop == SNot(t1).canonicalize(nf = Some(Dnf)))


    assert(SEmpty == (t1 || t2))
    assert(SEmpty == (t1 && t2))
    assert(SEmpty == (t1 ^^ t2))
    assert(SEmpty == (t1 - t2))
    assert(STop == (!t1))

    assert((SAtomic(classOf[String]) || SAtomic(classOf[Integer])).typep("hello"))
    assert((SAtomic(classOf[String]) || SAtomic(classOf[Integer])).typep(12))
    assert(!((SAtomic(classOf[String]) || SAtomic(classOf[Integer])).typep(12.32)))
  }


  test("test179") {
    import genus.SAtomic.{withClosedWorldView, withOpenWorldView}

    class A
    trait B
    trait C
    class D extends A with B with C
    withClosedWorldView {
      val rte = SAnd(SAtomic(classOf[D]), SNot(SAnd(SAtomic(classOf[B]), SAtomic(classOf[C]))))

      println("xxxxxx -> " + rte.inhabited)
      assert(Some(false) == rte.inhabited)
    }
  }

  test("test192") {
    val one:Any = 1
    case class Box(value: Any)
    assert(false == SAtomic(classOf[scala.runtime.RichInt]).typep(1))
    assert(SAtomic(classOf[Int]).typep(Box(1).value))
    assert(SAtomic(classOf[java.lang.Integer]).typep(Box(1).value))
    assert(SAtomic(classOf[java.lang.Integer]).typep(1)) // works
    assert(SAtomic(classOf[Integer]).typep(1)) // works
    assert(classOf[java.lang.Integer].isInstance(1)) // works
    assert(false == classOf[Int].isInstance(1))
    assert(1.isInstanceOf[Int])
    // fails
    //assert(1.isInstanceOf[Any])
    assert((1:Any).isInstanceOf[Any])
    assert(one.isInstanceOf[Int])
    assert(one.isInstanceOf[Any])
    assert(classOf[Any].isInstance(1))
    assert(classOf[java.lang.Object].isInstance(1))
  }

}
