package genus

import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}

import scala.collection.immutable.Stream.cons

object GenusSpecifications {
  def naiveGenGenus: Gen[SimpleTypeD] = Gen.lzy {
    Gen.sized { size =>
      lazy val genLeaf = {
        // Generate a predicate and its string representation
        def genPredicate: Gen[(Any => Boolean, String)] = {
          def predicate(a: Any, b: Boolean): Any => Boolean = ((a: Any) => b)

          for {
            anyval <- Arbitrary.arbitrary[AnyVal]
            // bool <- Arbitrary.arbitrary[Boolean]
          } yield (predicate(anyval, true), anyval + " => " + true)
        }

        implicit lazy val arbPredicate = Arbitrary(genPredicate)

        // Choose between one of the 6 terminal types
        Gen.frequency(
          (5, SAtomic(Arbitrary.arbitrary[AnyVal].getClass)),
          (5, SEql(Arbitrary.arbitrary[AnyVal].sample.get)),
          (5, SMember(Gen.listOfN[AnyVal](5, Arbitrary.arbitrary[AnyVal]).sample.get)),
          (5, {
            val predicate = arbPredicate.arbitrary.sample.get
            SSatisfies(predicate._1, predicate._2)
          }),
          (1, STop),
          (1, SEmpty),
        )
      }

      lazy val genInternalNode = {
        // TODO: Change genAnd and genOr to have 0 or more (>=2) parameters
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

        // Choose between one of the 3 non terminal types
        Gen.frequency(
          1 -> Gen.lzy(genAnd),
          1 -> Gen.lzy(genOr),
          1 -> Gen.lzy(genNot)
        )
      }

      Gen.oneOf(genInternalNode, genLeaf)
    }
  }

  implicit def shrinkGenus: Shrink[genus.SimpleTypeD] = Shrink {
    // TODO: Try shrinking with canonicalize() ?
    // TODO: Implement Shrinking:
    //  - If sons are TerminalType, remove one of them
    //  - If you have no sons, remove yourself
    // https://stackoverflow.com/questions/42581883/scalacheck-shrink
    case SNot(s) => shrink(s).map(SNot(_))
  }
}
