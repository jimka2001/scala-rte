package demos.scalaio2025

import rte.Rte.{randomRte, randomTotallyBalancedRte, rteViewAst, rteViewDfa}

object Scalaio2025 {

  def main(argv:Array[String]):Unit = {
    val rte1 = randomTotallyBalancedRte(0.75F, 4)
    rteViewDfa(rte1, "dfa-balanced")
    rteViewAst(rte1, title="ast-balanced")

    val rte2 = randomRte(4)
    rteViewDfa(rte2, "dfa-classic")
    rteViewAst(rte2, title="ast-classic")
  }
}
