package demos.scalaio2025

import scala.util.Random

object RteTree {
  import demos.scalaio2025.CsvLine.{statisticsResource,writeCsvStatistic}
  import demos.scalaio2025.Random.{treeSplitRteGaussian,treeSplitRteInvGaussian,treeSplitRteLinear,combRte}
  import demos.scalaio2025.Random.flajoletRteBySize
  val random = new Random

  val algos = Seq("tree-split-linear", "tree-split-gauss", "tree-split-inv-gauss", "flajolet", "comb")

  def csv(algo:String, prefix:String=""):String = {
    statisticsResource + prefix + algo + ".csv"
  }

  val genRte = Map("tree-split-inv-gauss" -> treeSplitRteInvGaussian _,
                   "tree-split-gauss" -> treeSplitRteGaussian _,
                   "tree-split-linear" -> treeSplitRteLinear _,
                   "flajolet" -> flajoletRteBySize _,
                   "comb" -> combRte _,
                   )

  def genCsvBySize(num_repetitions: Int,
                   algo:String,
                   prefix:String = "",
                   lot:Int = 6,
                   minLeaf:Int=1<<6,
                   maxLeaf:Int=1<<7): Unit = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    for {r <- 0 until num_repetitions
         futures = (0 until lot).map((_) => Future {
           val size = random.between(minLeaf, maxLeaf+1)
           println(s"r=$r size=$size algo=$algo")
           writeCsvStatistic(genRte=() => genRte(algo)(size), prefix=algo, csvFileName=csv(algo,prefix))
         })
         combined = Future.sequence(futures)} {

      Await.result(combined, Duration.Inf)
    }
  }
}

object TestBalance {
  import rte.{Sigma, EmptySet, Or, EmptySeq}
  import rte.Rte.rteViewAst

  def main(argv:Array[String]):Unit = {
    val rte1 = Or(Sigma, Or(EmptySet,Or(Sigma,EmptySeq)))
    println(rte1.measureBalance())
    println(rte1.linearize().length)
    rteViewAst(rte1, "testing")

    val rte2 = Or(Sigma, Or(Sigma, Or(Sigma, Or(Sigma, Or(Sigma,EmptySeq)))))
    println(rte2.measureBalance())
    println(rte2.linearize().length)
    rteViewAst(rte2, "testing2")
  }
}

object ViewAstInterestingSize {
  def main(argv:Array[String]):Unit = {
    ViewAst.sup(36, 5)
  }
}

object ViewAst {

  import rte.Rte
  import genus.SimpleTypeD
  import xymbolyco.Dfa
  import rte.Rte.rteViewAst
  import xymbolyco.GraphViz.dfaView
  import xymbolyco.Minimize.minimize

  def genRteDfaPair(algo: String, leaves: Int, states: Int): (Rte, Dfa[Any, SimpleTypeD, Boolean]) = {
    val rte = RteTree.genRte(algo)(leaves)
    val dfa = minimize(rte.toDfa())
    if (dfa.Qids.size < states)
      genRteDfaPair(algo, leaves, states)
    else
      (rte, dfa)
  }

  def sup(depth: Int, minStates:Int): Unit = {
    // retry until num states >= minStates
    for {algo <- RteTree.algos
         (rte, dfa) = genRteDfaPair(algo, 1 << depth, minStates)
         } {
      rteViewAst(rte, title = algo, dotFileCB = (str) => println(s"RTE $str"))
      println(rte.measureBalance())
      println(rte.linearize().length)
      dfaView(dfa, title = algo, abbrev = true, dotFileCB = (str) => println(s"DFA $str"))
    }
  }

  def main(argv: Array[String]): Unit = {
    val depth: Int = if (argv.length == 0) 4 else argv(0).toInt
    sup(depth, 1)
  }
}

