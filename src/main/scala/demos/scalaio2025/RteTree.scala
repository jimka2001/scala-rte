package demos.scalaio2025

import scala.util.Random

object RteTree {
  import demos.scalaio2025.CsvLine.{statisticsResource,writeCsvStatistic,readCsvLines,readAllCsvLines}
  import demos.scalaio2025.Random.{randomNaiveRteBySizeMid,randomNaiveRteBySizeEdge}
  import demos.scalaio2025.Random.randomTotallyBalancedRteBySize
  val random = new Random

  val algos = Seq("naive-mid", "naive-edge", "balanced")

  val csv = Map("naive-mid" -> (statisticsResource + "naive-mid.csv"),
                "naive-edge" -> (statisticsResource + "naive-edge.csv"),
                "balanced" -> (statisticsResource + "balanced.csv" ))

  val genRte = Map("naive-edge" -> randomNaiveRteBySizeEdge _,
                   "naive-mid" -> randomNaiveRteBySizeMid _,
                   "balanced" -> randomTotallyBalancedRteBySize _
                   )

  def genCsvBySize(num_repetitions: Int,
                   algo:String,
                   minLeaf:Int=1<<6,
                   maxLeaf:Int=1<<7): Unit = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    for {r <- 0 to num_repetitions
         futures = (0 to 5).map((_) => Future {
           val size = random.between(minLeaf, maxLeaf)
           println(s"r=$r size=$size")
           writeCsvStatistic(genRte=() => genRte(algo)(size), prefix=algo, csvFileName=csv(algo))
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

object ViewAst {

  import rte.Rte.rteViewAst
  import xymbolyco.GraphViz.dfaView
  import xymbolyco.Minimize.minimize

  def main(argv: Array[String]): Unit = {
    val depth: Int = if (argv.length == 0) 4 else argv(0).toInt
    for {algo <- RteTree.algos
         rte = RteTree.genRte(algo)(1 << depth)
         dfa = minimize(rte.toDfa())
         } {
      rteViewAst(rte, title = algo)
      println(rte.measureBalance())
      println(rte.linearize().length)
      dfaView(dfa, title = algo, abbrev=true)
    }
  }
}
