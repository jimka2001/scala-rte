package genus

import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}

import genus.STop
import genus.SEmpty

object GenusSpecifications {
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
          2 -> Gen.lzy(genAnd),
          2 -> Gen.lzy(genOr),
          1 -> Gen.lzy(genNot)
        )
      }

      Gen.oneOf(genInternalNode, genLeaf)
    }
  }

  // Implementation similar to this Ast Shrinker (https://stackoverflow.com/questions/42581883/scalacheck-shrink)
  // Sorry for using Stream even tho it is deprecated, it's ScalaCheck's fault
  //
  // TODO: Vulgarize strategy
  //  Shrinks according to the following strategy
  //    - Put STop and SEmpty at the top of the Stream
  //    - Append to the stream the SimpleTypeD with 1 child removed at each iteration
  //    - Append to the stream the SimpleTypeD with 1 child shrunk at each iteration
  //    - If 1 child and is TerminalType, put the child in stream
  implicit def shrinkGenus: Shrink[genus.SimpleTypeD] = Shrink {
    case t: SCombination => {
      var s: Stream[SimpleTypeD] = STop #:: SEmpty #:: Stream.empty

      // If 1 child and is TerminalType, put the child in stream
      if (t.tds.size == 1) s = t.tds(0) #:: s else {

      // Append to the stream the SimpleTypeD with 1 child removed at each iteration
      // TODO: return a t.zero if t.zero one of the children is t.zero
      for {
        i <- 0 until t.tds.size
      } if (t.tds(i) == t.zero) s = t.zero #:: Stream.Empty else s = (t.create(t.tds.take(i) ++ t.tds.drop(i + 1)) #:: s)

      // TODO: Finish this
        // Append to the stream the SimpleTypeD with 1 child shrunk at each iteration
        //      for {
        //        i <- 0 until t.tds.size
        //      } if (t.tds(i) == t.zero) s = (t.zero #:: Stream.empty) else s = shrink(t.tds(i)).map(_ => t.create(t.tds.take(i) ++ t.tds.drop(i + 1)).appended(_)) #:: Stream.empty
      }

      println(s"shrunk values: ${s.mkString(",")}")
      s
    }
                                                                              // Old
//    case t: SAnd => {
//      // Reconstruct SAnd while omitting the ith child
//      // If one of the child is SEmpty, we can simplify the SAnd by a SEmpy because the combination is not satisfiable
//      var s: Stream[SimpleTypeD] = Stream.empty
//
//      // TODO: does it work like I think it does ?
//      for {
//        c <- t.tds
//      } shrink(c)
//
//      // FIXME: short circuit if SEmpty found
//      for {
//        i <- 0 until t.tds.size
//      } if (t.tds(i) == SEmpty) s = (SEmpty #:: Stream.empty) else s = (SAnd(t.tds.take(i) ++ t.tds.drop(i + 1): _*) #:: s)
//
//      println(s"shrunk values: ${s.mkString(",")}")
//      s
//    }
//
//    case t: SOr => {
//      // Reconstruct SOr while omitting the ith child
//      // If one of the child is STop, we can simplify the SOr by a STop because the combination is always satisfiable
//      var s: Stream[SimpleTypeD] = Stream.empty
//
//      for {
//        c <- t.tds
//      } shrink(c)
//
//      // FIXME: short circuit if STop found
//      for {
//        i <- 0 until t.tds.size
//      } if (t.tds(i) == STop) s = (STop #:: Stream.empty) else s = (SOr(t.tds.take(i) ++ t.tds.drop(i + 1): _*) #:: s)
//      println(s"shrunk values: ${s.mkString(",")}")
//      s
//    }

    case t: SNot => {
      val s = STop #:: SEmpty #:: (if (t.s == SEmpty) STop #:: Stream.empty else if (t.s == STop) SEmpty #:: Stream.empty else shrink(t))
      println(s"shrunk values: ${s.mkString(",")}")
      s
    }

    case t: SEql => {
      val s = STop #:: SEmpty #:: (shrink(t.a).map(SEql(_)))
      println(s"shrunk values: ${s.mkString(",")}")
      s
    }

    case t: SMember => {
      val s = STop #:: SEmpty #:: (SMember(shrink(t.xs)) #:: Stream.empty)
      println(s"shrunk values: ${s.mkString(",")}")
      s
    }

    case t: SSatisfies => {
      val s = STop #:: SEmpty #:: (shrink(t.f).map(SSatisfies(_, t.printable)))
      println(s"shrunk values: ${s.mkString(",")}")
      s
    }

    case t => {
      println(s"shrunk ${t} into the void")
      Stream.empty
    }
  }
}
