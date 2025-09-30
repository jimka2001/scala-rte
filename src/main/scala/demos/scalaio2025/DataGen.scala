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

