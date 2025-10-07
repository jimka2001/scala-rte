package demos.scalaio2025



// tested
object GenCsvBalanced {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length < 1) 50 else argv(0).toInt

    RteTree.genCsvBySize(num_repetitions, algo="balanced")
  }
}

// testing
object GenCsvNaiveEdge {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvBySize(num_repetitions, algo="naive-edge")
  }
}

object GenCsvNaiveMid {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvBySize(num_repetitions, algo="naive-mid")
  }
}
object GenCsvCrazy {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvBySize(num_repetitions, algo="crazy")
  }
}

object GenCsvFixedLeafCount {
  import demos.scalaio2025.RteTree.algos

  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 1 else argv(0).toInt
    val leaf_count:Int = if (argv.length <= 1) 64 else argv(1).toInt
    for {algo <- algos}
      RteTree.genCsvBySize(num_repetitions,
        algo = algo,
        prefix = s"${leaf_count}-",
        lot = 1,
        minLeaf = leaf_count,
        maxLeaf = leaf_count)
  }
}

