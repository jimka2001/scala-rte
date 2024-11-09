package rte

object sanityTest {
  def check1() = {
    import rte.RteImplicits._
    import scala.language.implicitConversions
    import rte.Rte.rteIfThenElse
    val int = classOf[Int]
    val str = classOf[String]
    val f = rteIfThenElse(Seq(
      Star(int) -> (() => {
        println("case 1")
        1
      }),
      Star(str) -> (() => {
        println("case 2")
        2
      }),
      Cat(Star(int), Star(str)) -> (() => {
        println("case 3")
        3
      }),
      Cat(int, str) -> (() => {
        println("case impossible")
        0
      })),
                          () => {
                            println("default case")
                            4
                          }, handleUnreachable = (rte => println(s"unsatisfiable rte: $rte")))
    f(List(1, 2, 3, 4, 5))
    f(List("one", "two", "three", "four"))
    f(List("one", "two", 3, 4))
    f(List(1, 2, "three", "four"))
  }
  def check2() = {
    val data = List(1,2,3)
    val it = data.iterator
    print(it.foreach(println))
    print(it.foreach(println))
  }
  def main(argv: Array[String]): Unit = {
    //check1()
    check2()
  }
}


object sanityTest2 {
  def main(argv: Array[String]): Unit = {
    for {depth <- 5 to 7
         _ <- 1 to 2000
         rt = Rte.randomRte(depth)
         } {
      rt.toDfa()
    }
  }
}