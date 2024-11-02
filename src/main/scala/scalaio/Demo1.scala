package scalaio

import genus._
import rte.RteImplicits.tdToRte
import rte.{Rte, Xor}
import xymbolyco.GraphViz.dfaView

object Demo1 {
  val data = Seq("M", 0.1, 0.3, 4.5, // 1 or more double or float all positive
                 "C", 1, 5, 7, 8, // 1 or more ints, all positive
                 "C", 2, 5, 3,
                 "M", 0.5, 1.2
                 )
  val I: SimpleTypeD = SAtomic(classOf[Int])
  val DF:SimpleTypeD = SAtomic(classOf[Float]) || SAtomic(classOf[Double])

  def positive(x:Any):Boolean = {
    x match {
      case a:Int => a > 0
      case _ => false
    }
  }
  val IPos: SimpleTypeD = SAnd(I, SSatisfies(positive, "IPos"))
  val M: SimpleTypeD = SEql("M")
  val C: SimpleTypeD = SEql("C")

  val Mclause:Rte = M ++ DF.+
  val Cclause:Rte = C ++ IPos.+

  val pattern1:Rte = ( Mclause | Cclause).*
  val pattern2:Rte = ((M ++ DF.+) | (C ++ I.+)).*

  val pattern_xor = Xor(pattern1, pattern2)

  val labels = Seq(I, M, C)

  def main(argv:Array[String]):Unit = {

    println("Pattern 1 contains data1? --> "+pattern1.contains(data))
    dfaView(pattern1.toDfa(), title="Pattern 1", showSink=false, abbrev=true, givenLabels=labels)

    println("Pattern 2 contains data1? --> " + pattern2.contains(data))
    dfaView(pattern2.toDfa(), title="Pattern 2", showSink=false, abbrev=true, givenLabels=labels)

    println()

    val diff_dfa = pattern_xor.toDfa()
    dfaView(diff_dfa, title="Symmetric Difference", showSink=false, abbrev=true, givenLabels=labels)

    diff_dfa.spanningTrace match {
      case None => ()
      case Some(Right(e)) =>
        println("Spanning types: --> " + e)
        println("Spanning trace: --> " + e.map {
        (td: SimpleTypeD) => td.sampleValues.headOption
      })
    }
    println()

    println("Pattern 1 contains? --> " + pattern1.contains(Seq("C", 0)))
    println("Pattern 2 contains? --> " + pattern2.contains(Seq("C", 0)))
  }
}
