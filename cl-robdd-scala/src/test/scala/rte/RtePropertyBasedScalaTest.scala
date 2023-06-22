package rte

import adjuvant.GnuPlot.gnuPlot
import adjuvant.MyFunSuite
import org.scalacheck.Prop.{classify, forAll}
import org.scalacheck.{Arbitrary, Properties, Test}
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import rte.RteSpecifications.naiveGenRte
import xymbolyco.mystats.{brz, brzmin, thmp, thmpmin}
import xymbolyco.{GraphViz, RTEStatistics, mystats}
import scala.collection.immutable.{Map, Set}


import scala.Predef.->

object RtePropertyBasedScalaTest extends App {
    // This is a copy of the statisticSizePerfDFAfromF in the xymbolico package, adapted to work with a generator.

    mystats.statisticSizePerDFAfromGen(5, 5, 10, 1, naiveGenRte(5))
//  val rteList = for {
//    _ <- 0 until 5
//  } yield naiveGenRte(25).sample.get
//  for (g <- rteList) {
//    println(g.toLaTeX())
//    // FIXME: plot the dfa ( see below v )
//    GraphViz.dfaView(g.toDfa(),abbrev=true,title="rt1")
//  }
}

// Standalone PBT tests
object RteSpecification extends Properties("Rte") {
  val size = 10
  val botlzmann = new RteBoltzmann(size)
  implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(botlzmann.RteBoltzmannGen())

  override def overrideParameters(p: Test.Parameters): Test.Parameters = p.withMinSuccessfulTests(10000)

  // Test for classifying the type of input. The input will be generated for all properties, so it is not a fact about the repartition of the input, rather a hint
  property("test") = forAll { (r: Rte) =>
    // FIXME: stats about rte generation (See xymbolyco/RteStatistics)
    val dfa = r.toDfa()
//    print(dfa.toString)
    true
  }
}

// PBT linked with the testsuite
class RtePBT extends MyFunSuite with Matchers with Configuration {
  val depth = 10
  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 100000, workers = 4)
  implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(naiveGenRte(depth))

  test("test") {
    forAll { (r: Rte) =>
      val dfa = r.toDfa()
//      print(dfa.toString)
      true
    }
  }
}