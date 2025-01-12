package demos.padl2025

import rte._
import xymbolyco.GraphViz.dfaView

object Demo2 {

  val data:Seq[Any] = Seq("fred", 1, 2, 3, 4,
                          "john", -1, -2, -3,
                          "alice",
                          "anne", 10, 20,30
                          )
  val S:Rte = Atomic(classOf[String])
  val I:Rte = Atomic(classOf[Int])

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
  val INeg:Rte = I & Not(Pos)

  val pattern1:Rte = Pos ++ ((S ++ IPos.+) | (S ++ INeg.+)).*
  val pattern2:Rte = Pos ++ ((S ++ IPos.*) | (S.? ++ INeg.*)).+


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
    val isSubset12 = (pattern1 & !pattern2).toDfa()
    dfaView(isSubset12, title="Subset P1 < P2 Query")
    println("Spanning types: --> " + isSubset12.spanningTrace)
    println("Spanning trace: --> " + isSubset12.witness)


    // Question: Is pattern1 a subset of pattern2?
    val isSubset21 = (pattern2 & !pattern1).toDfa()
    dfaView(isSubset21, title="Subset P2 < P1 Query")
    println("Spanning types: --> " + isSubset21.spanningTrace)
    println("Spanning trace: --> " + isSubset21.witness)

  }
}
