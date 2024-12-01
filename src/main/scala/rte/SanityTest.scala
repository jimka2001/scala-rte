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

object sanityTest3 {

  val data:Seq[Any] = Seq("M", 0.1, 0.3F, 4.5, // "M" designates measurements,
                          //                      1 or more double or float
                          "C", 1, 5, 7, 8, // "C" designates counts,
                          //                      1 or more ints, all positive
                          "C", 2, 5, 3,
                          "M", 0.5, 1.2F
                          )
  val I:Rte = Atomic(classOf[Int])
  val DF:Rte = Atomic(classOf[Float]) | Atomic(classOf[Double])

  def positive(x:Any):Boolean = {
    x match {
      case a:Int => a > 0
      case _ => false
    }
  }

  val IPos:Rte = I & Satisfies(positive, "IPos")
  val M:Rte = Eql("M")
  val C:Rte = Eql("C")

  val Mclause:Rte = M ++ DF.+
  val Cclause:Rte = C ++ IPos.+

  def main(argv:Array[String]):Unit = {

    val pattern1:Rte = ( Mclause | Cclause).*

    pattern1.contains(data)

  }
}