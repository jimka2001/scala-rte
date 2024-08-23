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

package rte

import genus.GenusSpecifications.naiveGenGenus
import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}
import scala.language.implicitConversions
// This object contains the definition of the Rte Generator and Shrinker
object RteSpecifications {
  val depthSimpleTypeD = 1  // depth of the rte found as child of singleton
  val childLimit = 5        // limit to the number of child of an internal node

  // Generate a STerminal as a leaf of the Rte
  def genLeaf = {
    // Choose between one of the 6 terminal types
    Gen.frequency(
      (1, EmptySet),
      (1, EmptyWord),
      (1, Sigma),
      (1, Singleton(Gen.lzy(naiveGenGenus(depthSimpleTypeD)).sample.get)),
    )
  }

  // Generate a Combination or Not as an internal node of the Rte
  def genInternalNode(depth: Int) = Gen.lzy {
    val newDepth = depth - 1
    implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(naiveGenRte(newDepth))
    lazy val listRte = Gen.listOfN[Rte](childLimit, Arbitrary.arbitrary[Rte](arbitraryGen)).sample.get

    lazy val genAnd = And(listRte: _*)
    lazy val genOr = Or(listRte: _*)
    lazy val genNot = Not(naiveGenRte(newDepth).sample.get)
    lazy val genCat = Cat(listRte: _*)
    lazy val genStar = Star(naiveGenRte(newDepth).sample.get)

    // Choose between one of the 3 non terminal types
    Gen.frequency(
      2 -> Gen.lzy(genAnd),
      2 -> Gen.lzy(genOr),
      2 -> Gen.lzy(genStar),
      2 -> Gen.lzy(genCat),
      2 -> Gen.lzy(genNot)
    )
  }

  // Generate a Rte with a maximum depth of depth
  def naiveGenRte(depth: Int): Gen[Rte] = Gen.lzy {
    if (depth <= 1) Gen.lzy(genLeaf) else
      Gen.frequency(
        (5, Gen.lzy(genInternalNode(depth - 1))),
        (5, Gen.lzy(genLeaf)),
      )
  }

  //  Shrinks according to the following strategy
  //    - Try Sigma (the whole alphabet) and EmptySet
  //    - Try removing 1 different child at each iteration
  //    - Try to shrink 1 different child at each iteration
  //    - If a Combination only has 1 child, try with the child
  // Implementation similar to this Ast Shrinker (https://stackoverflow.com/questions/42581883/scalacheck-shrink)
  // FIXME: Refactor subshrinkers with common behaviour
  implicit def shrinkRte(rte:Rte): LazyList[Rte] = rte match {
    case t: Combination => {
      var s: LazyList[Rte] = EmptySet #:: Sigma #:: LazyList.empty

      // If 1 child and is STerminal, put the child in stream
      if (t.operands.size == 1) t.operands(0) #:: s else {

        // Append to the stream the Rte with 1 child removed at each iteration
        for {
          i <- 0 until t.operands.size
        } s = (t.create(t.operands.take(i) ++ t.operands.drop(i + 1)) #:: s)

        // Append to the stream the Rte with 1 child shrunk at each iteration
        for {
          i <- 0 until t.operands.size
        } s = (shrink(t.operands(i)).to(LazyList).map(e => t.create(t.operands.take(i) ++ t.operands.drop(i + 1).appended(e)))) #::: s

        s
      }
    }

    case t: Cat => {
      var s: LazyList[Rte] = EmptySet #:: Sigma #:: LazyList.empty

      // If 1 child and is STerminal, put the child in stream
      if (t.operands.size == 1) t.operands(0) #:: s else {

        // Append to the stream the Rte with 1 child removed at each iteration
        for {
          i <- 0 until t.operands.size
        } s = (t.create(t.operands.take(i) ++ t.operands.drop(i + 1)) #:: s)

        // Append to the stream the Rte with 1 child shrunk at each iteration
        for {
          i <- 0 until t.operands.size
        } s = (shrink(t.operands(i)).to(LazyList).map(e => t.create(t.operands.take(i) ++ t.operands.drop(i + 1).appended(e)))) #::: s

        s
      }
    }

    // Try Sigma, EmptySet, or the shrinked child
    case t:Not => {
      EmptySet #:: Sigma #:: t.operand #:: LazyList.empty
    }

    // Try Sigma, EmptySet, or shrink the content
    case t: Star => {
      EmptySet #:: Sigma #:: t.operand #:: LazyList.empty
    }

    // For other cases, nothing to shrink
    case t => {
      LazyList.empty
    }
  }
}
