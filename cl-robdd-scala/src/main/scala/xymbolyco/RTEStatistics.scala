package xymbolyco

import genus.{SAtomic, SimpleTypeD}
import javax.management.openmbean.SimpleType
import rte._
import xymbolyco.Profiling.check
import adjuvant.GnuPlot._

case class RTEStatistics(
                          rte: Rte
                        ) {

  lazy val numberOfTransitions: Map[(Boolean, String, Boolean, String), Int] = createMap(rte)

  def createMap[Σ, L, E](rte_raw: Rte): Map[(Boolean, String, Boolean, String), Int] = {
    val list = for {(can, rte) <- Seq((false, rte_raw), (true, rte_raw.canonicalize))
                    (constr, dfa_tmp) <- Seq(("Thompson", Thompson.constructThompsonDfa(rte, true))
                                             , ("Brz", rte.toDfa()))
                    m <- Seq(false, true)
                    dfa: Dfa[Any, SimpleTypeD, Boolean] = if (m) Minimize.minimize(Minimize.trim(dfa_tmp)) else dfa_tmp
                    labels: List[SimpleTypeD] = dfa.protoDelta.toList.map(_._2)
                    grouped: Map[Option[Boolean], List[SimpleTypeD]] = labels.groupBy(lab => lab.inhabited)
                    (w, inh) <- Seq(("indeterminate", None),
                                    ("satisfiable", Some(true)),
                                    ("unsatisfiable", Some(false)))
                    } yield {
      if (grouped.contains(inh)) {
        ((can, constr, m, w), grouped(inh).size)
      } else ((can, constr, m, w), 0)
    }
    list.toMap
  }
}

object Statistics {
  //this function fills up an Array of Arrays of Integers, which count transitions
  //each subarray will count transitions and divide them into 6 categories
  //sigma DFA non minimised : undecidable, unsatisfiable, satisfiable
  //sigma DFA minimised : undecidable, unsatisfiable, satisfiable
  //each top level array will be for different types of DFAs
  // first off the thompson built sigma DFA, then the brzozowski built sigma DFA
  // then the thompson built sigma DFA after the RTE has been canonicalized
  // then the brzozowski built sigma DFA after the RTE has been canonicalized
  def RTEstats(rte: Rte): Array[Array[Int]] = {
    // val transitions = Array.fill(4)(Array.fill(6)(0))
    val rtecanonicalized = rte.canonicalize
    val data = check(rte, 1, 1)
    val dataCanonicalize = check(rtecanonicalized, 1, 1)

    for{(f,k1,k2) <- Array((data, "dfa_thompson", "min_thompson"),
                           (data, "dfa_brzozowski", "min_brzozowski"),
                           (dataCanonicalize, "dfa_thompson", "min_thompson"),
                           (dataCanonicalize, "dfa_brzozowski", "min_brzozowski"))
        d1 = f(k1).protoDelta.toList.groupBy(tr => tr._2.inhabited).withDefaultValue(List())
        d2 = f(k2).protoDelta.toList.groupBy(tr => tr._2.inhabited).withDefaultValue(List())
        } yield {
      Array(d1(None).size, d1(Some(false)).size, d1(Some(true)).size,
            d2(None).size, d2(Some(false)).size, d2(Some(true)).size)
    }

//    var mylist = data("dfa_thompson").protoDelta.toList
//    for (i <- mylist.indices) {
//      val temp = mylist(i)._2.inhabited
//      if (temp.isEmpty) {
//        transitions(0)(0) += 1
//      }
//      else if (temp.contains(false)) {
//        transitions(0)(1) += 1
//      }
//      else if (temp.contains(true)) {
//        transitions(0)(2) += 1
//      }
//    }
//    mylist = data("min_thompson").protoDelta.toList
//    for (i <- mylist.indices) {
//      val temp = mylist(i)._2.inhabited
//      if (temp.isEmpty) {
//        transitions(0)(3) += 1
//      }
//      else if (temp.contains(false)) {
//        transitions(0)(4) += 1
//      }
//      else if (temp.contains(true)) {
//        transitions(0)(5) += 1
//      }
//    }
//
//    mylist = data("dfa_brzozowski").protoDelta.toList
//    for (i <- mylist.indices) {
//      val temp = mylist(i)._2.inhabited
//      if (temp.isEmpty) {
//        transitions(1)(0) += 1
//      }
//      else if (temp.contains(false)) {
//        transitions(1)(1) += 1
//      }
//      else if (temp.contains(true)) {
//        transitions(1)(2) += 1
//      }
//    }
//    mylist = data("min_brzozowski").protoDelta.toList
//    for (i <- mylist.indices) {
//      val temp = mylist(i)._2.inhabited
//      if (temp.isEmpty) {
//        transitions(1)(3) += 1
//      }
//      else if (temp.contains(false)) {
//        transitions(1)(4) += 1
//      }
//      else if (temp.contains(true)) {
//        transitions(1)(5) += 1
//      }
//    }
//    mylist = dataCanonicalize("dfa_thompson").protoDelta.toList
//    for (i <- mylist.indices) {
//      val temp = mylist(i)._2.inhabited
//      if (temp.isEmpty) {
//        transitions(2)(0) += 1
//      }
//      else if (temp.contains(false)) {
//        transitions(2)(1) += 1
//      }
//      else if (temp.contains(true)) {
//        transitions(2)(2) += 1
//      }
//    }
//    mylist = dataCanonicalize("min_thompson").protoDelta.toList
//    for (i <- mylist.indices) {
//      val temp = mylist(i)._2.inhabited
//      if (temp.isEmpty) {
//        transitions(2)(3) += 1
//      }
//      else if (temp.contains(false)) {
//        transitions(2)(4) += 1
//      }
//      else if (temp.contains(true)) {
//        transitions(2)(5) += 1
//      }
//    }
//    mylist = dataCanonicalize("dfa_brzozowski").protoDelta.toList
//    for (i <- mylist.indices) {
//      val temp = mylist(i)._2.inhabited
//      if (temp.isEmpty) {
//        transitions(3)(0) += 1
//      }
//      else if (temp.contains(false)) {
//        transitions(3)(1) += 1
//      }
//      else if (temp.contains(true)) {
//        transitions(3)(2) += 1
//      }
//    }
//    mylist = dataCanonicalize("min_brzozowski").protoDelta.toList
//    for (i <- mylist.indices) {
//      val temp = mylist(i)._2.inhabited
//      if (temp.isEmpty) {
//        transitions(3)(3) += 1
//      }
//      else if (temp.contains(false)) {
//        transitions(3)(4) += 1
//      }
//      else if (temp.contains(true)) {
//        transitions(3)(5) += 1
//      }
//    }
//    transitions
  }

}

object mystats {
  //creates a GNUPLOT for a given type of DFA ( brzmin,brz,thmp,thmp min) can also work with any function of the form
  // rte=> Int, the function has to be given into parameters, number of RTEs per depth and amount of depths
  def statisticSizePerDepth(number: Int = 30000, depth: Int = 5, f: Rte => Double = brz): Unit = {
    //sequence that will contain all curves
    var myseq: Seq[(String, Seq[(Double, Double)])] = Seq()
    for (j <- Range(0, depth)) {
      //using a map to count DFAs (creating couples of size->count, incrementing at each DFA
      val mymap: Map[Double, Double] = Map().withDefaultValue(0)
      val data = Range(0, number).foldLeft(mymap) { (acc, _) =>
        val fr = f(Rte.randomRte(depth + 1))
        acc + (fr -> (acc(fr) + 1))
      }
      //converting map to sequence of doubles, then adding the sequence of doubles to curve sequence
      val mytempseq: Seq[(Double, Double)] = data.toSeq.sorted
      myseq :+= ("Depth :".concat(j.toString), mytempseq)
    }
    //build graph with each curve
    gnuPlot(myseq)(Set("png"), "", "", "Number of States per Sigma-DFA",
                   false, "Proportion of sigma DFAs", false,
                   false, "DFAsizeperdepth", "linespoints",
                   "horizontal bmargin", _ => (), false, false)
  }

  def statisticSizePerDFA(num: Int = 30000, depth: Int = 5): Unit = {
    //sequence that will contain all the curves
    var myseq: Seq[(String, Seq[(Double, Double)])] = Seq()
    //seq of strings to name the curves
    val fnames = Seq("thompson", "thompson_min", "brzozowski", "brzozowski min")
    //seq of functions to create each curve
    val myfuncseq: Seq[Rte => Double] = Seq(thmp, thmpmin, brz, brzmin)
    for (i <- Range(0, 4)) {
      val f = myfuncseq(i)
      //builds map of double->double, rte, counts all values
      val mymap: Map[Double, Double] = Map().withDefaultValue(0)
      val data = Range(0, num * depth).foldLeft(mymap) { (acc, x) =>
        val fr = f(Rte.randomRte(x / num + 1))
        acc + (fr -> (acc(fr) + 1))
      }
      //adds curve to sequence of curves
      myseq :+= (fnames(i), data.toSeq.sorted)
    }
    //creates gnuplot
    gnuPlot(myseq)(Set("png"), title = "", comment = "", xAxisLabel = "Number of States per Sigma-DFA",
                   xLog = true, yAxisLabel = "Proportion of sigma DFAs", yLog = true,
                   grid = false, outputFileBaseName = "DFAsizeperdepth", plotWith = "linespoints",
                   key = "horizontal bmargin", _ => (), verbose = false, view = false)
  }

  // GnuFileCB   Seq(String).! to execute with console
  //functions to use as arguments for statisticsizeperdepth
  def brz(rte: Rte): Double = {
    rte.toDfa(42).Q.size
  }

  def brzmin(rte: Rte): Double = {
    Minimize.minimize(Minimize.trim(rte.toDfa(42))).Q.size
  }

  def thmp(rte: Rte): Double = {
    Thompson.constructThompsonDfa(rte, 42).Q.size
  }

  def thmpmin(rte: Rte): Double = {
    Minimize.minimize(Minimize.trim(Thompson.constructThompsonDfa(rte, 42))).Q.size
  }
}