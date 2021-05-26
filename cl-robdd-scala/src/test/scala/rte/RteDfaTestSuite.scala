// Copyright (c) 2021 EPITA Research and Development Laboratory
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

import org.scalatest.funsuite.AnyFunSuite

class RteDfaTestSuite extends AnyFunSuite {

  test("rte to dfa") {
    //import xymbolyco.GraphViz._

    for {depth <- 5 to 7
         _ <- 1 to 1000
         rt = Rte.randomRte(depth)
         } {
      rt.toDfa()
      //dfaView(sdfa, abbreviateTransitions=true)
    }
  }
  test("rte to png") {
    import xymbolyco.GraphViz._

    for {depth <- 5 to 6
         rep <- 1 to 3
         rt = Rte.randomRte(depth).canonicalize
         } {

      dfaToPng(rt.toDfa(), s"depth=$depth,rep=$rep", abbreviateTransitions=true)
      // dfaView(rt.toDfa(), s"depth=$depth,rep=$rep", abbreviateTransitions=true)
    }
  }
  test("dfa minimize") {
    for {depth <- 5 to 6
         rep <- 1 to 1000
         dfa = Rte.randomRte(depth).toDfa()
         } {
      xymbolyco.Minimize.minimize(dfa)
    }
  }
  test("dfa trim"){
    for {depth <- 5 to 6
         rep <- 1 to 1000
         dfa = Rte.randomRte(depth).toDfa()
         } {
      xymbolyco.Minimize.removeNonAccessible(dfa)
      xymbolyco.Minimize.removeNonCoAccessible(dfa)
      xymbolyco.Minimize.trim(dfa)
    }
  }
}