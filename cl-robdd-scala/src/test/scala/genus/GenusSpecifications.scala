package genus

import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Cogen, Gen}

object GenusSpecifications {
  def naiveGenGenus: Gen[SimpleTypeD] = {
    def genInternalNode(left: SimpleTypeD, right: SimpleTypeD) = Gen.frequency(
      (1, SNot(right)), // Arbitrary choice of right
      (1, SAnd(left, right)),
      (1, SOr(left, right))
    )

    def genLeaf() =  {
      // FIXME: Fix predicate generator
//      def createPredicate[Any, Boolean](seed0: Seed, cogen: Cogen[Any], gen: Gen[Boolean]): Any => Boolean =
//        n => {
//          val seed1 = cogen.perturb(seed0, n)
//          gen.run(seed1)._1
//        }

      def genPredicate: Gen[AnyVal => Boolean] = {
        def predicate(a: AnyVal, b: Boolean): AnyVal => Boolean = ((a: AnyVal) => b)
        for {
          anyval <- Arbitrary.arbitrary[AnyVal]
          bool <- Arbitrary.arbitrary[Boolean]
        } yield predicate(anyval, bool)
      }

      implicit lazy val arbPredicate = Arbitrary(genPredicate)

      Gen.frequency(
        (1, SAtomic(Arbitrary.arbitrary[AnyVal].getClass)),
        (1, SEql(Arbitrary.arbitrary[AnyVal])),
        (1, SEql(Arbitrary.arbitrary[AnyVal])),
        (1, SMember(Gen.listOf[Any])),
        (1, SSatisfies(arbPredicate)) // <- Aled
        (1, STop),
        (1, SEmpty),
      )
    }

    for {
      left <- naiveGenGenus
      right <- naiveGenGenus
      v <- Gen.oneOf(genInternalNode(left, right), genLeaf())
    } yield v
  }
}
