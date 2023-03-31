package genus

import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Cogen, Gen}

object GenusSpecifications {
  def naiveGenGenus: Gen[SimpleTypeD] = Gen.lzy { Gen.sized { size =>

      lazy val genLeaf = {
        //      def createPredicate[Any, Boolean](seed0: Seed, cogen: Cogen[Any], gen: Gen[Boolean]): Any => Boolean =
        //        n => {
        //          val seed1 = cogen.perturb(seed0, n)
        //          gen.run(seed1)._1
        //        }

        // def genPredicate: Gen[AnyVal => Boolean] = {
        //   def predicate(a: AnyVal, b: Boolean): AnyVal => Boolean = ((a: AnyVal) => b)

        //   for {
        //     anyval <- Arbitrary.arbitrary[AnyVal]
        //     bool <- Arbitrary.arbitrary[Boolean]
        //   } yield predicate(anyval, bool)
        // }

        // implicit lazy val arbPredicate = Arbitrary(genPredicate)

        // TODO: Generate predicates to be able to generate SAtomic types
        // TODO: Generate SMembers
        Gen.frequency(
          (1, SAtomic(Arbitrary.arbitrary[AnyVal].getClass)),
          (1, SEql(Arbitrary.arbitrary[AnyVal])),
          (1, SEql(Arbitrary.arbitrary[AnyVal])),
          // (1, SMember(Gen.listOf[Any])), // <- smol aled
          // (1, SSatisfies(arbPredicate)) // <- Aled
          (1, STop),
          (1, SEmpty),
        )
      }

      lazy val genInternalNode = {
        lazy val genAnd = for {
          left <- naiveGenGenus
          right <- naiveGenGenus
        } yield SAnd(left, right)

        lazy val genOr = for {
          left <- naiveGenGenus
          right <- naiveGenGenus
        } yield SOr(left, right)

        lazy val genNot = for {
          arg <- naiveGenGenus
        } yield SNot(arg)

        Gen.frequency(
          1 -> Gen.lzy(genAnd),
          1 -> Gen.lzy(genOr),
          1 -> Gen.lzy(genNot)
        )
      }

      Gen.oneOf(genInternalNode, genLeaf)
    }
  }
}
