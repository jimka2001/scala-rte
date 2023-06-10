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


// Main entrypoint
object RtePropertyBasedScalaTest extends App {
    // This is a copy of the statisticSizePerfDFAfromF in the xymbolico package, adapted to work with a generator.
    def statisticSizePerDFAfromGen(num: Int = 30000, minsize: Int = 5, maxsize: Int = 20, sizevar: Int = 5): Unit = {
    //sequence that will contain all the curves
    var myseq: Seq[(String, Seq[(Double, Double)])] = Seq()
    //seq of strings to name the curves
    val fnames = Seq("thompson", "thompson_min", "brzozowski", "brzozowski min")
    //seq of functions to create each curve
    val myfuncseq: Seq[Rte => Double] = Seq(thmp, thmpmin, brz, brzmin)
    for (i <- Range(0, 4)) {
      val f = myfuncseq(i)
      //builds map of double->double, rte, counts all values
      val mymap: Map[Double, Double] = Map().withDefaultValue(0)
      val data = Range(0, (num * (maxsize - minsize)) / sizevar).foldLeft(mymap) { (acc, x) =>
        val rte = naiveGenRte(((x / num) * sizevar) + minsize).sample.get
        val fr = f(rte)
        acc + (fr -> (acc(fr) + 1)) // FIXME: Error
      }
      //adds curve to sequence of curves
      myseq :+= (fnames(i), data.toSeq.map(a => (a._1, (a._2 * 100) / num * ((maxsize - minsize) / sizevar))).sorted)
    }
    //creates gnuplot
    gnuPlot(myseq)(Set("png"), title = "", comment = "", xAxisLabel = "Number of States per Sigma-DFA from a Random DFA",
                   xLog = true, yAxisLabel = "Proportion of sigma DFAs", yLog = true,
                   grid = false, outputFileBaseName = "DFAsizeperdepth", plotWith = "linespoints",
                   key = "horizontal bmargin", _ => (), verbose = false, view = false)
  }
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
  implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(naiveGenRte(10))

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