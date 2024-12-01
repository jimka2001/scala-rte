package scalaio

import rte.{Rte, Xor, Atomic, Satisfies, Eql}
import xymbolyco.GraphViz.dfaView

object Demo1 {

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

  val pattern1:Rte = ( Mclause | Cclause).*
  val pattern2:Rte = ((M ++ DF.+) | (C ++ I.+)).*

  def main(argv:Array[String]):Unit = {

    val m1 = pattern1.contains(data, verbose=true)
    assert(pattern1.contains(data,verbose=true))
    println("Pattern 1 contains data1? --> " + m1)

    dfaView(pattern1.toDfa(), title="Pattern 1", showSink=false, abbrev=true)

    val m2 = pattern2.contains(data)

    println("Pattern 2 contains data1? --> " + m2)

    dfaView(pattern2.toDfa(), title="Pattern 2", showSink=false, abbrev=true)
    
    val pattern_xor:Rte = Xor(pattern1, pattern2)
    val diff_dfa = pattern_xor.toDfa()
    dfaView(diff_dfa, title="Symmetric Difference", showSink=false, abbrev=true)

    val trace = diff_dfa.spanningTrace
    val witness = diff_dfa.witness
    println("Spanning types: --> " + trace)
    println("Spanning trace: --> " + witness)

    val m1a = pattern1.contains(Seq("C", 0))
    val m2a = pattern2.contains(Seq("C", 0))
    println("Pattern 1 contains? --> " + m1a)
    println("Pattern 2 contains? --> " + m2a)
  }
}
