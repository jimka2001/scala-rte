package demos.scalaio2025

object DataGen {
  def main(argv: Array[String]): Unit = {
  }
}

// tested
object GenCsvBalanced {
  def main(argv: Array[String]): Unit = {
    val limit:Int = if (argv.length == 0) 100 else argv(0).toInt
    RteTree.genCsvBalanced(limit)
  }
}

// testing
object GenCsvNaive {
  def main(argv: Array[String]): Unit = {
    val limit:Int = if (argv.length == 0) 50 else argv(0).toInt
    RteTree.genCsvNaive(limit)
  }
}

// tested
object GenCsvTuned {
  def main(argv: Array[String]): Unit = {
    val limit:Int = if (argv.length == 0) 10 else argv(0).toInt
    RteTree.genCsvTuned(limit)
  }
}

// testing
object GenCsvTunedME {
  def main(argv: Array[String]): Unit = {
    val limit:Int = if (argv.length == 0) 10 else argv(0).toInt
    RteTree.genCsvTunedME(limit)
  }
}



