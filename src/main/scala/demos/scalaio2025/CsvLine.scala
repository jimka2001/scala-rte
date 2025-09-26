package demos.scalaio2025


case class CsvLine(depth: Int,
                   node_count: Int,
                   state_pre_count:Int,
                   transition_pre_count:Int,
                   state_count: Int,
                   transition_count: Int,
                   shortest:Int,
                   longest:Int,
                   total:Int,
                   probability: Double = 0.0F) {
  def imbalance():Double = {
    imbalanceFactor(node_count, total)
  }
}