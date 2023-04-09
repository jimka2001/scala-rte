package genus

import adjuvant.MyFunSuite
import genus.GenusSpecification.arbitraryGen
import genus.GenusSpecifications.naiveGenGenus
import genus.GenusSpecifications.shrinkGenus
import genus.NormalForm.Dnf
import genus.RandomType.{Class1X, Class2X}
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

// Most of the information needed to use scalatest with scalacheck is in this page: https://www.scalatest.org/user_guide/generator_driven_property_checks
// With this method, we can integrate ScalaCheck and Scalatest. But we lose clear information about what is the original argument (before shrinking), what number of tests are passing etc.
class GenusPBT extends MyFunSuite with Matchers with Configuration {
  val depth = 10
  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 10000, workers = 4)

   test("DNF Inverse") {
     forAll { (t: SimpleTypeD) =>
       val dnf = t.canonicalize(Some(Dnf))
       val inverse = SNot(dnf)

       (t - dnf).inhabited != Some(true) && (t || inverse) == STop || (!(t || inverse) == SEmpty) || (!(t || inverse)).inhabited != Some(true) should be (true)
     }
   }

  test("Verify CNF") {
    forAll { (t: SimpleTypeD) =>
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

      isCnf(t.canonicalize(Some(Cnf))) should be (true)
    }
  }

  test("Verify DNF") {
    forAll { (t: SimpleTypeD) =>
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

      isDnf(t.canonicalize(Some(Dnf))) should be (true)
    }
  }

  test("DNF/CNF check_type") {
    forAll { (t: SimpleTypeD) =>
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
      t.typep(interestingValues) should be (cnf.typep(interestingValues))
      t.typep(interestingValues) should be (dnf.typep(interestingValues))
    }
  }

  // Second test of "combo conversion9"
  // TODO: What kind of conditions does Jim need to satisfy this test ? How can I implement this ?
  // TODO: This seems to run for a long time / infinitely sometimes. Why ?
  test("AB!C + A!BC + A!B!C -> AB!C + A!BC + A!C") {
    forAll { (A: SimpleTypeD, B: SimpleTypeD, C:SimpleTypeD) =>
      SOr(SAnd(A, B, SNot(C)), SAnd(A, SNot(B), C), SAnd(A, SNot(B), SNot(C))).conversion9() should be (SOr(SAnd(A, B, SNot(C)), SAnd(A, SNot(B), C), SAnd(A, SNot(C))))
    }
  }

}
