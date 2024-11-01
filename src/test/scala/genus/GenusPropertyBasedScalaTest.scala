// copyright (c) 2021 epita research and development laboratory
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package genus

import adjuvant.AdjFunSuite
import genus.GenusSpecification.arbitraryGen
import genus.GenusSpecifications.shrinkGenus
import genus.NormalForm.Dnf
import genus.RandomType.{Class1X, Class2X, getInterestingValues}
import org.scalacheck.Prop.propBoolean
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

// Most of the information needed to use scalatest with scalacheck is in this page: https://www.scalatest.org/user_guide/generator_driven_property_checks
// With this method, we can integrate ScalaCheck and Scalatest. But we lose clear information about what is the original argument (before shrinking), what number of tests are passing etc.
class GenusPBT extends AdjFunSuite with Matchers with Configuration {
  val depth = 10
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 100000, workers = 4)

  test("DNF Inverse") {
    forAll { (t: SimpleTypeD) =>
      val dnf = t.canonicalize(Some(Dnf))
      val inverse = SNot(dnf)

      (t - dnf).inhabited != Some(true) && (t || inverse) == STop || (!(t || inverse) == SEmpty) || (!(t || inverse)).inhabited != Some(true)
    }
  }

  test("Verify CNF") {
    forAll { (t: SimpleTypeD) =>
      import NormalForm._
      def baseCase(td: SimpleTypeD): Boolean = {
        td match {
          case _: STerminal => true
          case SNot(_: STerminal) => true
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

      isCnf(t.canonicalize(Some(Cnf))) should be(true)
    }
  }

  test("Verify DNF") {
    forAll { (t: SimpleTypeD) =>
      import NormalForm._
      def baseCase(td: SimpleTypeD): Boolean = {
        td match {
          case _: STerminal => true
          case SNot(_: STerminal) => true
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
  }

  test("DNF/CNF check_type") {
    forAll { (t: SimpleTypeD) =>
      import NormalForm._
      val cnf = t.canonicalize(Some(Cnf))
      val dnf = t.canonicalize(Some(Dnf))

      val interestingValues: Set[Any] = getInterestingValues(true)

      // TODO: Generate randomly values passed to typep
      t.typep(interestingValues) should be(cnf.typep(interestingValues))
      t.typep(interestingValues) should be(dnf.typep(interestingValues))
    }
  }

  // Second test of "combo conversion9"
  // TODO: What kind of conditions does Jim need to satisfy this test ? How can I implement this ?
  // TODO: This seems to run for a long time / infinitely sometimes. Why ?
  test("AB!C + A!BC + A!B!C -> AB!C + A!BC + A!C") {
    forAll { (A: SimpleTypeD, B: SimpleTypeD, C: SimpleTypeD) =>
      (A != B
        && B != C
        && A != C
        && A != SNot(B)
        && A != SNot(C)
        ) ==> (SOr(SAnd(A, B, SNot(C)), SAnd(A, SNot(B), C), SAnd(A, SNot(B), SNot(C))).conversion9() == SOr(SAnd(A, B, SNot(C)), SAnd(A, SNot(B), C), SAnd(A, SNot(C))))
    }
  }

}
