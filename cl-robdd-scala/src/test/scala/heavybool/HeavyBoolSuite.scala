package heavybool

import adjuvant.MyFunSuite
import HeavyBool._

class HeavyBoolSuite extends MyFunSuite {
  test("and") {
    forallM("n",
            LazyList.range(1,10,3)){
      (n: Int) => HeavyBool(n % 2 != 0,
                            List(Map("forall" -> true)))} &&
      existsM("n", LazyList.range(1,10,3)){
        (n: Int) => HeavyBool(n % 2 != 0,
                              List(Map("exists" -> true)))}
  }

  test("or") {
    forallM(  "n", LazyList.range(1,10,3)) {
      (n: Int) => HeavyBool(n % 2 != 0, List(Map("forall" -> true)))
    } || existsM("n", LazyList.range(1,10,3)){ (n: Int) =>
      HeavyBool(n % 2 != 0, List(Map("exists" -> true)))
    }}

  test("forall"){
    assert(HTrue == forallM("x", LazyList(1,2,3)){ (x:Int) =>
      (if (x > 0)
        HeavyBool(true, List(Map("reason" -> "works")))
      else
        HeavyBool(false, List(Map("reason" -> "fails"))))
    })

    val result = forallM("x", LazyList(1,2,3)){ (x:Int) =>
      (if (x > 10)
        HeavyTrue(List(Map("reason" -> "works")))
      else
        HeavyFalse(List(Map("reason" -> "fails"))))}
    assert(result.toBoolean == false)
    assert(result.because.head("witness") == 1)
  }

  def testCayleyTables(n:Int):Unit = {
    import heavybool.Magma.{allUnitalCayleyTables, cayleyTable, genLazyFinite}
    for {add <- allUnitalCayleyTables(n)
         str = cayleyTable(genLazyFinite(n-1), add)
         } println(str)
  }

  test("Cayley Tables"){
    testCayleyTables(2)
    testCayleyTables(3)
  }

  test("more tests") {
    for {p <- List(2, 3, 5, 7, 11)
         g = new MultiplicationModP(p)
         } {
      def inv(a: Int): Option[Int] = {
        (1 until p).find(b => (a * b) % p == 1)
      }

      assertM(g.isGroup(1, inv))
      assert(g.isGroup(1, inv).toBoolean)
    }
  }


  test("test mod p") {
    for {p <- 2 to 10
         add = new AdditionModP(p)
         mult = new MultiplicationModP(p)
         } {
      assertM(add.isMonoid(0))

      add.isGroup(0, (a: Int) => Some((p - a) % p)).annotate(s"addition mod $p") match {
        case hb@HeavyFalse(_) => println(hb)
        case _ => println(s"$add is a group")
      }
      mult.isGroup(1, a => (1 until p).find(b => (a * b) % p == 1)).annotate(s"multiplication mod $p") match {
        case hb@HeavyFalse(_) => println(hb)
        case _ => println(s"$mult is a group")
      }
    }
  }

  test("logic") {
    val x = List(Map("reason" -> "x"))
    val y = List(Map("reason" -> "y"))
    assert((HeavyTrue(x) && HeavyTrue(y)) == HeavyTrue(y))
    assert((HeavyTrue(x) || HeavyTrue(y)) == HeavyTrue(x))
    assert((HeavyFalse(x) && HeavyFalse(y)) == HeavyFalse(x))
    assert((HeavyFalse(x) || HeavyFalse(y)) == HeavyFalse(y))
  }

  test("exists")  {
    def f(p: Int): HeavyBool = {
      val g = new MultiplicationModP(p)

      def inv(a: Int): Option[Int] = {

        (1 until p).find(b => (a * b) % p == 1)
      }
      g.isGroup(1, inv)
    }
    println(existsM("f", LazyList.from(3 to 3))(f))
    println(existsM("p", LazyList.from(2 to 10))(p => !f(p)))
    println(existsM("f", LazyList.from(2 to 10))(f))
  }

  test("count groups"){
    import heavybool.Magma.{countGroups}
    countGroups(2)
    countGroups(3)
    //countGroups(4)
  }

  test("find groups"){
    import heavybool.Magma.{findGroupsM}
    findGroupsM(2)
    findGroupsM(3)
    findGroupsM(4)
  }
}
