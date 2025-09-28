package demos.scalaio2025

object DataGen {
  def main(argv: Array[String]): Unit = {
  }
}

// tested
object GenCsvBalanced {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 100 else argv(0).toInt
    RteTree.genCsvBySize(num_repetitions, algo="naive")
  }
}

// testing
object GenCsvNaive {
  def main(argv: Array[String]): Unit = {
    val num_repetitions:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvBySize(num_repetitions, algo="naive")
  }
}

