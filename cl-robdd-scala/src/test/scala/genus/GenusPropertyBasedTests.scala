package genus

import org.scalacheck.Gen
import genus.SimpleTypeD
import genus.GenusSpecifications

object GenusPropertyBasedTests extends App {
  // TODO: Create arbitrary generator for trees => properties
  val genusSample = GenusSpecifications.naiveGenGenus.sample.get
  println(genusSample)
  val dot = genusSample.toDot()
  // println(System.getProperty("user.dir") + "/src/test/scala/genus/dot/naiveGenGenus.dot")
  // TODO: Write dot in a file
  // TOOD: display dot
}
