package functionalscala2024

import rte._
import xymbolyco.GraphViz.dfaView

object Demo1 {

  val data:Seq[Any] = Seq("M", 0.1, 0.3F, 4.5, // "M" designates measurements,
                          //                      1 or more double or float
                          "C", 1, 5, 7, 8, // "C" designates counts,
                          //                      1 or more integers, all positive
                          "C", 2, 5, 3,
                          "M", 0.5, 1.2F
                          )
  val I:Rte = Atomic(classOf[Int])
  val DF:Rte = Atomic(classOf[Float]) | Atomic(classOf[Double])

  def positive(x:Any):Boolean = {
    x match {
      case a:Int => a > 0
      case a:Double => a > 0.0
      case a:Float => a > 0.0
      case _ => false
    }
  }

  val Pos:Rte = Satisfies(positive, "Pos")
  val IPos:Rte = I & Pos
  val M:Rte = Eql("M")
  val C:Rte = Eql("C")

  val Mclause:Rte = M ++ DF.+
  val Cclause:Rte = C ++ IPos.+

  val pattern1:Rte = ( Mclause | Cclause).*
  val pattern2:Rte = ((M ++ DF.+) | (C ++ I.+)).*


  def main(argv:Array[String]):Unit = {

    val m1 = pattern1.contains(data, verbose=false)
    val m2 = pattern2.contains(data)

    println("Pattern 1 contains data? --> " + m1)
    println("Pattern 2 contains data? --> " + m2)

    dfaView(pattern1.toDfa(), title="Pattern 1")
    dfaView(pattern2.toDfa(), title="Pattern 2")
    
    val pattern_xor:Rte = Xor(pattern1, pattern2)
    val diff_dfa = pattern_xor.toDfa()
    dfaView(diff_dfa, title="Symmetric Difference", abbrev=true)

    val trace = diff_dfa.spanningTrace
    val witness = diff_dfa.witness
    println("Spanning types: --> " + trace)
    println("Spanning trace: --> " + witness)

    val m1a = pattern1.contains(Seq("C", 0))
    val m2a = pattern2.contains(Seq("C", 0))
    println(s"Pattern 1 contains $witness? --> " + m1a)
    println(s"Pattern 2 contains $witness? --> " + m2a)

    // Question: Is pattern1 a subset of pattern2?
    val isSubset = (pattern1 & !pattern2).toDfa()
    dfaView(isSubset, title="Subset Query")
    println("Spanning types: --> " + isSubset.spanningTrace)

    // Question: Is pattern1 a subset of pattern2?
    val isSubset = (pattern1 & !pattern2).toDfa()
    dfaView(isSubset, title="Subset Query")
    println("Spanning types: --> " + isSubset.spanningTrace)
  }
}
