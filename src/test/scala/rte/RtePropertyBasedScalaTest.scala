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

import org.scalacheck.Arbitrary
import xymbolyco.GraphViz

object RtePropertyBasedScalaTest extends App {
  def classifyRte(n: Int, size: Int, stdDepth: Int)= {
    val boltzmann = new RteBoltzmann(size, stdDepth)
    implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(boltzmann.RteBoltzmannGen())


    // Classification of the generated output
    var empty = 0
    var satisfiable = 0
    var indeterminate = 0

    for {
      i <- Range(0, n)
    } {
      val rte = boltzmann.RteBoltzmannGen().sample.get
      val dfa = rte.toDfa(42)

      if (dfa.vacuous().getOrElse(false))
        empty += 1
      else dfa.spanningPath match {
        case Some(Left(_)) => indeterminate += 1
        case Some(Right(_)) => satisfiable += 1
      }
    }

    empty = empty * 100 / n
    satisfiable = satisfiable * 100 / n
    indeterminate = indeterminate * 100 / n

    Console.println(s"classification of ${n} RTEs of approximate size ${size}")
    Console.println(s"empty=${empty}%")
    Console.println(s"satisfiable=${satisfiable}%")
    Console.println(s"indeterminate=${indeterminate}%")
    Console.println(s"total=${indeterminate + empty + satisfiable}% (the difference between the total and 100% is due to floating point imprecisions")
  }

  def viewRte(n: Int, size: Int, stdDepth: Int) = {
    val boltzmann = new RteBoltzmann(size, stdDepth)
    implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(boltzmann.RteBoltzmannGen())

    for {
      i <- Range(0, n)
    } {
      val rte = boltzmann.RteBoltzmannGen().sample.get
      val dfa = rte.toDfa(42)
      GraphViz.dfaView(dfa)
    }
  }

  def printRte(n: Int, size: Int, stdDepth: Int) = {
    val boltzmann = new RteBoltzmann(size, stdDepth)
    implicit lazy val arbitraryGen: Arbitrary[Rte] = Arbitrary(boltzmann.RteBoltzmannGen())

    for {
      i <- Range(0, n)
    } {
      val rte = boltzmann.RteBoltzmannGen().sample.get
      Console.println(rte.toMachineReadable())
    }
  }

  viewRte(10, 25, 1)
//  printRte(100, 25, 1)
//  classifyRte(100, 25, 1)
}