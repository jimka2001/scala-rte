package demos.scalaio2024

import rte.{Atomic, Eql, Rte}

object Example1 {
  val data = Seq("C", 100, 200, 300,
                 "M", 10.0, 20.0,
                 "M",
                 "C", 1, 2, 3,
                 "C",-1, -3, -7, -8)

  val F:Rte = Atomic(classOf[Double]) | Atomic(classOf[Float])
  val I:Rte = Atomic(classOf[Int])
  val keyM:Rte = Eql("M")
  val keyC:Rte = Eql("C")

  val re:Rte = ((keyC ++ I.*) | (keyM ++ F.*)).*
  println(re.contains(data))

  def main(argv:Array[String]):Unit = {

  }
}
