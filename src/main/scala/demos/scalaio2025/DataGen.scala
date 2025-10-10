package demos.scalaio2025



// tested
object GenCsvFlajolet {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length < 1) 50 else argv(0).toInt

    RteTree.genCsvBySize(num_repetitions, algo="flajolet")
  }
}

// testing
object GenCsvTreeSplitEdge {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvBySize(num_repetitions, algo="tree-split-edge")
  }
}

object GenCsvTreeSplitLinear {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvBySize(num_repetitions, algo="tree-split-linear")
  }
}

object GenCsvTreeSplitMid {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvBySize(num_repetitions, algo="tree-split-mid")
  }
}

object GenCsvComb {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvBySize(num_repetitions, algo="comb")
  }
}



object GenCsvFixedLeafCount {
  import demos.scalaio2025.RteTree

  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 1 else argv(0).toInt
    val leaf_count:Int = if (argv.length <= 1) 64 else argv(1).toInt
    val algos:Seq[String] = if (argv.length <= 2) demos.scalaio2025.RteTree.algos else argv.drop(2).toSeq
    for {algo <- algos}
      RteTree.genCsvBySize(num_repetitions,
        algo = algo,
        prefix = s"${leaf_count}-",
        lot = 1,
        minLeaf = leaf_count,
        maxLeaf = leaf_count)
  }
}

