package genus

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

import genus.GenusSpecifications.naiveGenGenus
import genus.NormalForm.Dnf

object GenusPropertyBasedTests extends App {
  val genusList = for {
    n <- 0 until 10
  } yield naiveGenGenus.sample.get
  for (g <- genusList)
    println(g)
}

object GenusSpecification extends Properties("Genus") {
  implicit lazy val arbitraryGen: Arbitrary[SimpleTypeD] = Arbitrary(naiveGenGenus)

  property("DNF Inverse") = forAll { (t: SimpleTypeD) =>
    val dnf = t.canonicalize(Some(Dnf))
    val inverse = SNot(dnf)

    (t - dnf).inhabited != Some(true) &&
    (t || inverse) == STop || (!(t || inverse) == SEmpty) || (!(t || inverse)).inhabited != Some(true)
  }
}
