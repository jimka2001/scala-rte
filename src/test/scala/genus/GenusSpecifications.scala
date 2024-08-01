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

import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}

// Definition of the SimpleTypeD Generator and Shrinker
object GenusSpecifications {
  val childrenNum = 5

  // Generate a predicate and its string representation
  def genPredicate: Gen[(Any => Boolean, String)] = {
    def predicate(_a: Any, b: Boolean): Any => Boolean = (a: Any) => b

    for {
      anyval <- Arbitrary.arbitrary[AnyVal]
    } yield (predicate(anyval, b = true), s"$anyval => true")
  }

  // Generate a TerminalType as a leaf of the SimpleTypeD
  def genLeaf: Gen[SimpleTypeD] = {
    Gen.lzy {
      // Choose between one of the 6 terminal types
      // STop and SEmpty have a lesser coefficient as an attempt to improve the semantic of the generated SimpleTypeD.
      // Since SAnd(..., SEmpty, ...) is equivalent to SEmpty, and SOr(..., STop, ...) is equivalent to STop.
      Gen.frequency(
        (5, SAtomic(Arbitrary.arbitrary[AnyVal].getClass)),
        (5, SEql(Arbitrary.arbitrary[AnyVal].sample.get)),
        (5, SMember(Gen.listOfN[AnyVal](childrenNum, Arbitrary.arbitrary[AnyVal]).sample.get)),
        (5, {
          val predicate = genPredicate.sample.get
          SSatisfies(predicate._1, predicate._2)
        }),
        (1, STop),
        (1, SEmpty),
      )
    }
  }

  // Generate a SCombination or SNot as an internal node of the SimpleTypeD
   def genInternalNode(depth: Int): Gen[SimpleTypeD] = Gen.lzy {
     val newDepth = depth - 1 // Maximal depth of the generated subtrees
     implicit lazy val arbitraryGen: Arbitrary[SimpleTypeD] = Arbitrary(naiveGenGenus(newDepth))
     lazy val listGenus = Gen.listOfN[SimpleTypeD](childrenNum, Arbitrary.arbitrary[SimpleTypeD](arbitraryGen)).sample.get

    // Choose between one of the 3 non terminal types
    Gen.frequency(
      2 -> Gen.lzy(SAnd(listGenus: _*)),
      2 -> Gen.lzy(SOr(listGenus: _*)),
      1 -> Gen.lzy(SNot(naiveGenGenus(newDepth).sample.get))
    )
  }

  // Generate a SimpleTypeD with a maximum depth of depth
  def naiveGenGenus(depth: Int): Gen[SimpleTypeD] = Gen.lzy {
    if (depth <= 1) Gen.lzy(genLeaf) else
    Gen.frequency(
      (1, Gen.lzy(genInternalNode(depth - 1))),
      (1, Gen.lzy(genLeaf)),
    )
  }

  //  Shrinks according to the following strategy
  //    - Try STop and SEmpty
  //    - Try removing 1 different child at each iteration
  //    - Try to shrink 1 different child at each iteration
  //    - If a SCombination only has 1 child, try with the child
  // Implementation similar to this Ast Shrinker (https://stackoverflow.com/questions/42581883/scalacheck-shrink)
  implicit def shrinkGenus: Shrink[genus.SimpleTypeD] = Shrink {
    case t: SCombination =>
      var s: LazyList[SimpleTypeD] = SEmpty #:: STop #:: LazyList.empty

      // If 1 child and is TerminalType, put the child in stream
      if (t.tds.size == 1) t.tds(0) #:: s else {

        // Append to the stream the SimpleTypeD with 1 child removed at each iteration
        for {
          i <- 0 until t.tds.size
        } s = t.create(t.tds.take(i) ++ t.tds.drop(i + 1)) #:: s

        // Append to the stream the SimpleTypeD with 1 child shrunk at each iteration
        for {
          i <- 0 until t.tds.size
        } s = shrink(t.tds(i)).map(e => t.create(t.tds.take(i) ++ t.tds.drop(i + 1).appended(e))) #::: s

        s
      }

    // Try STop, SEmpty, or the child
    case t: SNot =>
      SEmpty #:: STop #:: t.s #:: LazyList.empty

    // Try STop, SEmpty, or shrink the content
    case t: SEql =>
      SEmpty #:: STop #:: shrink(t.a).map(SEql(_))

    // Try STop, SEmpty, or shrink the content
    case t: SMember =>
      SEmpty #:: STop #:: SMember(shrink(t.xs)) #:: LazyList.empty

    // Try STop, SEmpty, or shrink the content
    case t: SSatisfies =>
      SEmpty #:: STop #:: shrink(t.f).map(SSatisfies(_, t.printable))

    case t: SAtomic =>
      SEmpty #:: STop #:: LazyList.empty

    case t =>
      LazyList.empty
  }
}
