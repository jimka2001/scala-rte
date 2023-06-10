package rte

import adjuvant.MyFunSuite
import org.scalacheck.Prop.{classify, forAll}
import org.scalacheck.{Arbitrary, Properties, Test}
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import rte.RteSpecifications.naiveGenRte


object RtePropertyBasedScalaTest extends App {
  val rteList = for {
    _ <- 0 until 10
  } yield naiveGenRte(25).sample.get
  for (g <- rteList)
    println(g.toString())
}

object RteSpecification extends Properties("Rte") {
  implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(naiveGenRte(10))

  override def overrideParameters(p: Test.Parameters): Test.Parameters = p.withMinSuccessfulTests(10000)

  // Test for classifying the type of input. The input will be generated for all properties, so it is not a fact about the repartition of the input, rather a hint
  property("test") = forAll { (r: Rte) =>
    val dfa = r.toDfa()
    print(dfa.toString)
    true
  }
}

class RtePBT extends MyFunSuite with Matchers with Configuration {
  val depth = 10
  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 100000, workers = 4)
  implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(naiveGenRte(depth))

  test("test") {
    forAll { (r: Rte) =>
      val dfa = r.toDfa()
      print(dfa.toString)
      true
    }
  }
}