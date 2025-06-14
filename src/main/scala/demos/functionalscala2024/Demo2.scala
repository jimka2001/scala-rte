package demos.functionalscala2024

import genus._
import rte.RteImplicits.tdToRte
import rte.{Rte, Xor}
import xymbolyco.GraphViz.dfaView

object Demo2 {
  val data1 = Seq("XY", 1, 0.5F, 2, 0.8F, 5, 1.2F, 7, 0.4F, // 1 or more x,y where x is integer and y is float
                  "M", 0.1, 0.3, 4.5, // 1 or more double or float all positive
                  "C", 1, 5, 7, 8, // 1 ore more ints, all positive
                  "C", 2, 5, 3,
                  "M", 0.5, 1.2
                  )
  val I: SimpleTypeD = SAtomic(classOf[Int])
  val D:SimpleTypeD = SAtomic(classOf[Double])
  val F:SimpleTypeD = SAtomic(classOf[Float])
  val DF:SimpleTypeD = F || D

  def positive(x:Any):Boolean = {
    x match {
      case a:Int => a > 0
      case _ => false
    }
  }
  val IPos: SimpleTypeD = SAnd(I, SSatisfies(positive, "IPos"))
  val M: SimpleTypeD = SEql("M")
  val C: SimpleTypeD = SEql("C")
  val XY: SimpleTypeD = SEql("XY")
  val XYclause:Rte = XY ++ (I ++ DF).+
  val Mclause:Rte = M ++ DF.+
  val Cclause:Rte = C ++ IPos.+

  val pattern1:Rte = (XYclause | Mclause | Cclause).*
  val pattern2:Rte = ((XY ++ (IPos ++ DF).+) | (M ++ DF.+) | (C ++ IPos.+)).*

  val labels = Seq(I, M, C, XY)
  println(pattern1.contains(data1))
  println(pattern2.contains(data1))
  val diff = Xor(pattern2 ,pattern1)
  val dfa = diff.toDfa(exitValue=true)
  dfaView(pattern1.toDfa(), title="pattern 1", showSink=false, abbrev=true, givenLabels=labels)
  dfaView(pattern2.toDfa(), title="pattern 2", showSink=false, abbrev=true, givenLabels=labels)
  dfaView(dfa, title="xor", showSink=false, abbrev=true, givenLabels=labels)

  println(dfa.spanningTrace)

  println(pattern1.contains(Seq("XY", -1, 1.0)))
  println(pattern2.contains(Seq("XY", -1, 1.0)))

  def main(argv:Array[String]):Unit = {

  }
}
