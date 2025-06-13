package demos.padl2025

import rte._
import xymbolyco.GraphViz.dfaView
import xymbolyco.{Satisfiable,Indeterminate}

object Demo {

  def large(n:Any):Boolean = {
    n match {
      case n: Int => n > 128
      case _ => false
    }
  }

  def even(n:Any):Boolean = {
    n match {
      case n: Int => n % 2 == 0
      case _ => false
    }
  }

  val data:Seq[Any] = Seq("Lorem", "ipsum", 2, 4, 8, "dolor", 10, 20)

  val S:Rte = Atomic(classOf[String])
  val I:Rte = Atomic(classOf[Int])
  val Even:Rte = Satisfies(even, "Even")
  val Large:Rte = Satisfies(large, "Large")
  val N:Rte = Atomic(classOf[Number])

  val pattern1:Rte = (S ++ Even.*).*
  val pattern2:Rte = (S ++ (I & Even).*).*
  val pattern3:Rte = (S ++ (I & Even & Large).*).*
  val pattern4:Rte = ((S | N).+ ++ (I & Even).*).+ & (S ++ N).+ & !S

  def main(argv:Array[String]):Unit = {

    val m1 = pattern1.contains(data)
    val m2 = pattern2.contains(data)
    val m3 = pattern3.contains(data)

    println("Pattern 1 matches data? --> " + m1)
    println("Pattern 2 matches data? --> " + m2)
    println("Pattern 3 matches data? --> " + m3)

    dfaView(pattern1.toDfa(), title="Pattern 1", abbrev=true)
    dfaView(pattern2.toDfa(), title="Pattern 2", abbrev=true)
    dfaView(pattern3.toDfa(), title="Pattern 3", abbrev=true)

    val pairs = List((pattern1, "P1"),
                     (pattern2, "P2"),
                     (pattern3, "P3"),
                     (pattern4, "P4"))
    for{ (p,pl) <- pairs
         (q,ql) <- pairs
         if p != q
         diff = (p - q).toDfa()
         label = s"Query $pl - $ql"
         } {
      println("--------------------")
      println(s"$pl = $p")
      println(s"$ql = $q")
      dfaView(diff, title=label, abbrev=true, dotFileCB=(println))
      println(label)

      diff.spanningTrace match {
        case None =>
          println(s"   Subtype: $pl < $ql")
          println(s"   All Elements in $ql are in $ql")
          println("   Spanning types: --> " + diff.spanningTrace)
        case Some((Satisfiable,_)) =>
          println(s"   NOT Subtype: $pl !< $ql")
          println(s"   Elements in $pl and not in $ql")
          println("   Spanning types: --> " + diff.spanningTrace)
        case Some((Indeterminate, _)) =>
          println(s" DONT KNOW Subtype: $pl, $ql")
          println(s"   MAYBE Elements in $pl and not in $ql")
          println("   Spanning types: --> " + diff.spanningTrace)
        case _ => ()
      }
      println("   Spanning trace: --> " + diff.witness)

    }

  }
}
