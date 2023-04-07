package genus

import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}

import genus.STop
import genus.SEmpty

object GenusSpecifications {
  implicit lazy val arbitraryGen: Arbitrary[SimpleTypeD] = Arbitrary(naiveGenGenus)
  def naiveGenGenus: Gen[SimpleTypeD] = Gen.lzy {
    // TODO: Upgrade current AnyValgen with other types
    // TODO: Check implementation of RandomType
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
          (5, SMember(Gen.listOfN[AnyVal](20, Arbitrary.arbitrary[AnyVal]).sample.get)),
          (5, {
            val predicate = arbPredicate.arbitrary.sample.get
            SSatisfies(predicate._1, predicate._2)
          }),
          (1, STop),
          (1, SEmpty),
        )
      }

      lazy val genInternalNode = {
        // TODO: Support generation of >= 2 parameters for SAnd and SOr
        lazy val genListGenus = Gen.listOfN[SimpleTypeD](2, Arbitrary.arbitrary[SimpleTypeD](arbitraryGen))

        lazy val genAnd = SAnd(genListGenus.sample.get :_*)
        lazy val genOr = SOr(genListGenus.sample.get :_*)
        lazy val genNot = SNot(naiveGenGenus.sample.get)

        // Choose between one of the 3 non terminal types
        Gen.frequency(
          2 -> Gen.lzy(genAnd),
          2 -> Gen.lzy(genOr),
          1 -> Gen.lzy(genNot)
        )
      }

      Gen.oneOf(genInternalNode, genLeaf)
    }
  }

  //  Shrinks according to the following strategy
  //    - Try STop and SEmpty
  //    - Try removing 1 different child at each iteration
  //    - Try to shrink 1 different child at each iteration
  //    - If a SCombination only has 1 child, try with the child
  // Implementation similar to this Ast Shrinker (https://stackoverflow.com/questions/42581883/scalacheck-shrink)
  implicit def shrinkGenus: Shrink[genus.SimpleTypeD] = Shrink {
    case t: SCombination => {
      var s: Stream[SimpleTypeD] = SEmpty #:: STop #:: Stream.empty

      // If 1 child and is TerminalType, put the child in stream
      if (t.tds.size == 1) t.tds(0) #:: s else {

        // Append to the stream the SimpleTypeD with 1 child removed at each iteration
        for {
          i <- 0 until t.tds.size
        } s = (t.create(t.tds.take(i) ++ t.tds.drop(i + 1)) #:: s)

        // Append to the stream the SimpleTypeD with 1 child shrunk at each iteration
        for {
          i <- 0 until t.tds.size
        } s = (shrink(t.tds(i)).map(e => t.create(t.tds.take(i) ++ t.tds.drop(i + 1).appended(e)))) #::: s

        s
      }
    }

    // Try STop, SEmpty, or the shrinked child
    case t: SNot => {
      SEmpty #:: STop #:: shrink(t)
    }

    // Try STop, SEmpty, or shrink the content
    case t: SEql => {
      SEmpty #:: STop #:: (shrink(t.a).map(SEql(_)))
    }

    // Try STop, SEmpty, or shrink the content
    case t: SMember => {
      SEmpty #:: STop #:: (SMember(shrink(t.xs)) #:: Stream.empty)
    }

    // Try STop, SEmpty, or shrink the content
    case t: SSatisfies => {
      SEmpty #:: STop #:: (shrink(t.f).map(SSatisfies(_, t.printable)))
    }

    case t: SAtomic => {
      println("Fais au moins semblant d'essayer")
      SEmpty #:: STop #:: Stream.empty
    }

    // For other cases, nothing to shrink
    case t => {
      Stream.empty
    }
  }

  // TODO: Make the Shrinker from conversion9 property shrink all 3 values in the tuple
}
