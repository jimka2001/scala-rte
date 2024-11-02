package scalaio

import rte.{Rte, Xor, Atomic, Satisfies, Eql}
import xymbolyco.GraphViz.dfaView

object Demo1 {
  val data:Seq[Any] = Seq("M", 0.1, 0.3, 4.5, // "M" designates measurements, 1 or more double or float
                          "C", 1, 5, 7, 8, // "C" designates counts, 1 or more ints, all positive
                          "C", 2, 5, 3,
                          "M", 0.5, 1.2
                          )
  val I: Rte = Atomic(classOf[Int])
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
    println("Pattern 1 contains data1? --> " + pattern1.contains(data))
    dfaView(pattern1.toDfa(), title="Pattern 1", showSink=false, abbrev=true)

    val pattern2:Rte = ((M ++ DF.+) | (C ++ I.+)).*
    println("Pattern 2 contains data1? --> " + pattern2.contains(data))
    dfaView(pattern2.toDfa(), title="Pattern 2", showSink=false, abbrev=true)

    println()

    val pattern_xor:Rte = Xor(pattern1, pattern2)
    val diff_dfa = pattern_xor.toDfa()
    dfaView(diff_dfa, title="Symmetric Difference", showSink=false, abbrev=true)

    println("Spanning types: --> " + diff_dfa.spanningTrace)
    println("Spanning trace: --> " + diff_dfa.witness)
    println()

    println("Pattern 1 contains? --> " + pattern1.contains(Seq("C", 0)))
    println("Pattern 2 contains? --> " + pattern2.contains(Seq("C", 0)))
  }
}
