package genus

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink, Test}
import org.scalacheck.Prop.{classify, collect, forAll, forAllNoShrink, propBoolean}
import genus.GenusSpecifications.naiveGenGenus
import genus.GenusSpecifications.shrinkGenus
import genus.NormalForm.Dnf
import genus.RandomType.{Class1X, Class2X, interestingValues}


object GenusPropertyBasedTests extends App {
  val genusList = for {
    _ <- 0 until 10
  } yield naiveGenGenus(25).sample.get
  for (g <- genusList)
    println(g.toString())
}

object GenusSpecification extends Properties("Genus") {
  implicit lazy val arbitraryGen: Arbitrary[SimpleTypeD] = Arbitrary(naiveGenGenus(20))

  override def overrideParameters(p: Test.Parameters): Test.Parameters = p.withMinSuccessfulTests(100)

  // Test for classifying the type of input. The input will be generated for all properties, so it is not a fact about the repartition of the input, rather a hint
  property("FOR CLASSIFICATION PURPOSES") = forAll { (t: SimpleTypeD) =>
    classify(t.inhabited == None, "undecidable") {
      classify(t.inhabited == Some(true), "inhabited") {
        classify(t.inhabited == Some(false), "not habited") {
          true
        }
      }
    }
  }

  // From GenusCanonicalize.scala
  property("DNF Inverse") = forAll { (t: SimpleTypeD) =>
    val dnf = t.canonicalize(Some(Dnf))
    val inverse = SNot(dnf)

    (t - dnf).inhabited != Some(true) && (t || inverse) == STop || (!(t || inverse) == SEmpty) || (!(t || inverse)).inhabited != Some(true)
  }

  property("Verify CNF") = forAll { (t: SimpleTypeD) =>
    import NormalForm._
    def baseCase(td: SimpleTypeD): Boolean = {
      td match {
        case _: TerminalType => true
        case SNot(_: TerminalType) => true
        case _ => false
      }
    }

    def isCnf(td: SimpleTypeD): Boolean = {
      td match {
        case td if baseCase(td) => true
        case SOr(tds@_*) => tds.forall(baseCase)
        case SAnd(tds@_*) =>
          val ors = tds.collect { case td: SOr => td }
          val others = tds diff ors
          others.forall(td => baseCase(td)) && ors.forall { case SOr(tds@_*) => tds.forall(baseCase) }
        case _ => false
      }
    }

    isCnf(t.canonicalize(Some(Cnf)))
  }

  property("Verify DNF") = forAll { (t: SimpleTypeD) =>
    import NormalForm._
    def baseCase(td: SimpleTypeD): Boolean = {
      td match {
        case _: TerminalType => true
        case SNot(_: TerminalType) => true
        case _ => false
      }
    }

    def isDnf(td: SimpleTypeD): Boolean = {
      td match {
        case td if baseCase(td) => true
        case SAnd(tds@_*) => tds.forall(baseCase)
        case SOr(tds@_*) =>
          val ands = tds.collect { case td: SAnd => td }
          val others = tds diff ands
          others.forall(td => baseCase(td)) && ands.forall { case SAnd(tds@_*) => tds.forall(baseCase) }
        case _ => false
      }
    }

    isDnf(t.canonicalize(Some(Dnf)))
  }

  property("DNF/CNF check_type") = forAll { (t: SimpleTypeD) =>
    import NormalForm._
    val cnf = t.canonicalize(Some(Cnf))
    val dnf = t.canonicalize(Some(Dnf))

    val interestingValues: Vector[Any] = Vector(
      -1, -1, 0, 1, 2, 3, 4, 5, 6,
      1L, 0L, -1L, 1000L, 1000000L,
      3.14, 2.17, -math.sqrt(2),
      3.14d, 2.17d,
      3.14f, 2.17f,
      'a', 'b', 'c',
      true, false,
      "a", "b", "c", "d", "",
      new Class1X,
      new Class2X
    )

    // TODO: Generate randomly values passed to typep
    t.typep(interestingValues) == cnf.typep(interestingValues) && t.typep(interestingValues) == dnf.typep(interestingValues)
  }

  // Second test of "combo conversion9"
  // Conditions:
  //  - Assumes no duplicates, or no SOr(A, SNot(A))/SAnd(A, SNot(A))
  property("AB!C + A!BC + A!B!C -> AB!C + A!BC + A!C") = forAll { (A: SimpleTypeD, B: SimpleTypeD, C: SimpleTypeD) =>
    (A != B
      && B != C
      && A != C
      && A != SNot(B)
      && A != SNot(C)
      ) ==> (SOr(SAnd(A, B, SNot(C)), SAnd(A, SNot(B), C), SAnd(A, SNot(B), SNot(C))).conversion9() == SOr(SAnd(A, B, SNot(C)), SAnd(A, SNot(B), C), SAnd(A, SNot(C))))
  }

  // TODO: Add more tests
  // Stopped at GenusCanonicalize.scala line 400~
}
