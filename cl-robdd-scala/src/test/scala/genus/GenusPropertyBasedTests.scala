package genus

import genus.SimpleTypeD
import genus.GenusSpecifications

object GenusPropertyBasedTests extends App {
  val genusSample = GenusSpecifications.naiveGenGenus.sample.get
  val genusSample.toDot()
}
