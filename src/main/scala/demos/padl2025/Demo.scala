package demos.padl2025

import rte._
import xymbolyco.GraphViz.dfaView

object Demo {

  val data:Seq[Any] = Seq("Lorem", "ipsum", 1, 2, 3, "dolor", 10, 20)
  val S:Rte = Atomic(classOf[String])
  val I:Rte = Atomic(classOf[Int])

  def positive(x:Any):Boolean = {
    x match {
      case a:Int => a > 0
      case _ => false
    }
  }

  val Pos:Rte = Satisfies(positive, "Pos")


  val pattern1:Rte = S ++ (S | I).+
  val pattern2:Rte = S ++ I.* ++ (S | I & Pos).+


  def main(argv:Array[String]):Unit = {

    val m1 = pattern1.contains(data, verbose=false)
    val m2 = pattern2.contains(data)

    println("Pattern 1 contains data? --> " + m1)
    println("Pattern 2 contains data? --> " + m2)

    dfaView(pattern1.toDfa(), title="Pattern 1", abbrev=true)
    dfaView(pattern2.toDfa(), title="Pattern 2", abbrev=true)

    val pattern_xor:Rte = Xor(pattern1, pattern2)
    val diff_dfa = pattern_xor.toDfa()
    dfaView(diff_dfa, title="Symmetric Difference", abbrev=true)



    // Question: Is pattern1 a subset of pattern2?
    val isSubset12 = (pattern1 & !pattern2).toDfa()
    dfaView(isSubset12, title="Subset P1 < P2 Query", abbrev=true)
    println("P1 < P2 Spanning types: --> " + isSubset12.spanningTrace)
    println("P1 < P2 Spanning trace: --> " + isSubset12.witness)


    // Question: Is pattern1 a subset of pattern2?
    val isSubset21 = (pattern2 & !pattern1).toDfa()
    dfaView(isSubset21, title="Subset P2 < P1 Query", abbrev=true)
    println("P2 < P1 Spanning types: --> " + isSubset21.spanningTrace)
    println("P2 < P1 Spanning trace: --> " + isSubset21.witness)

  }
}
