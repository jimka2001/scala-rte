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
  test("sanity check") {
    import NormalForm._
    import RandomType.randomType
    val a = 2
    val t = SAtomic(classOf[Int])

    println("type of a = " + a.getClass)
    println("class of Int = " + classOf[Int])

    println(t.typep(a))
    class Abstract1
    class Abstract2
    trait Trait1
    trait Trait2
    trait Trait3
    val t1 = SAnd(SAtomic(classOf[Trait1]),
                  SOr(SAtomic(classOf[Abstract1]),
                      SAtomic(classOf[Abstract2])),
                  SAtomic(classOf[Trait2]))
    val t2 = SAnd(SAtomic(classOf[Trait1]),
                  SNot(SOr(SAtomic(classOf[Abstract1]),
                           SAtomic(classOf[Abstract2]))),
                  SAtomic(classOf[Trait2]))
    println(t1)
    println(t2)
    println(t2.canonicalize(nf = Some(Dnf)))
    println(t1.canonicalize())
    println(t1.canonicalize(nf = Some(Dnf)))
    println(SNot(t1).canonicalize(nf = Some(Dnf)))
    (0 to 10).foreach { i =>
      val t = randomType(6)
      println(s"$i:" + t)
      println("   " + t.canonicalize())
    }

    println(t1 || t2)
    println(t1 && t2)
    println(t1 ^^ t2)
    println(t1 - t2)
    println(!t1)

    println(SAtomic(classOf[String]) || SAtomic(classOf[Integer]))
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
    }
  }

  test("test192") {
    case class Box(value: Any)
    println(SAtomic(classOf[scala.runtime.RichInt]).typep(1))
    println(SAtomic(classOf[Int]).typep(Box(1).value))
    println(SAtomic(classOf[java.lang.Integer]).typep(Box(1).value))
    println(SAtomic(classOf[java.lang.Integer]).typep(1)) // works
    println(SAtomic(classOf[Integer]).typep(1)) // works
    println(classOf[java.lang.Integer].isInstance(1)) // works
    println(classOf[Int].isInstance(1))
    println(1.isInstanceOf[Int])
    println(1.isInstanceOf[Any])
    println(classOf[Any].isInstance(1))
    println(classOf[java.lang.Object].isInstance(1))
  }

}
