package demos.functionalscala2024

import rte.{Atomic, Rte}
import xymbolyco.GraphViz.dfaView

object Example2 {
  val data = Seq(13, 2.0, 6.0, 4, "a", "an", "the", -5, 2.0, 3.0, 4.0, 7, 8.0)

  val D:Rte = Atomic(classOf[Double])
  val I:Rte = Atomic(classOf[Int])
  val S:Rte = Atomic(classOf[String])

  val re:Rte = (I ++ (D.+ | S.+)).+

  dfaView(re.toDfa(), title="example 2", showSink=true)
  println(re.contains(data, verbose=true))

  def main(argv:Array[String]):Unit = {

  }
}
