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

import adjuvant.AdjFunSuite
import org.scalatest.funsuite.AnyFunSuite

//noinspection RedundantDefaultArgument
class RteDfaTestSuite extends AdjFunSuite {

  test("dfa discovered case 29"){
    val rte1 = Not(Sigma)
    val rte2 = Star(Not(Sigma))
    val dfa1 = rte1.toDfa(true)
    val dfa2 = rte2.toDfa(true)
    // sequences not in Sigma, thus in Not(Sigma)
    assert(dfa1.simulate(Seq()).contains(true))
    assert(dfa1.simulate(Seq(1,1)).contains(true))
    assert(dfa1.simulate(Seq(1,1,1)).contains(true))

    // sequences in Not(Sigma) must also be in Star(Not(Sigma))
    assert(dfa2.simulate(Seq()).contains(true))
    assert(dfa2.simulate(Seq(1,1)).contains(true))
    assert(dfa2.simulate(Seq(1,1,1)).contains(true))
  }

  test("rte to dfa") {
    //import xymbolyco.GraphViz._

    for {depth <- 5 to 6
         rep <- 1 to num_random_tests/10
         rt = Rte.randomRte(depth)
         } {
      //println(List(depth,rep))
      rt.toDfa()
    }
  }

  test("rte to png") {
    import xymbolyco.GraphViz._

    for {depth <- 5 to 6
         rep <- 1 to 3
         rt = Rte.randomRte(depth).canonicalize
         } {

      dfaToPng(rt.toDfa(), title=s"depth=$depth,rep=$rep")
    }
  }
  test("dfa minimize") {
    for {depth <- 4 to 5
         _ <- 1 to num_random_tests/10
         dfa = Rte.randomRte(depth).toDfa()
         } {
      xymbolyco.Minimize.minimize(dfa)
    }
  }
  test("dfa trim") {
    for {depth <- 5 to 6
         _ <- 1 to 10
         dfa = Rte.randomRte(depth).toDfa()
         } {
      xymbolyco.Minimize.removeNonAccessible(dfa)
      xymbolyco.Minimize.removeNonCoAccessible(dfa)
      xymbolyco.Minimize.trim(dfa)
    }
  }
  test("dfa accessible") {
    val dfa = new xymbolyco.Dfa[Any,genus.SimpleTypeD,Boolean](Qids = Set(0, 1, 2, 3, 4),
                                q0id = 0,
                                Fids = Set(2),
                                protoDelta = Set((0, genus.SEql(1), 1),
                                                 (0, genus.SEql(-1), 4),
                                                 (1, genus.SEql(1), 2),
                                                 (3, genus.SEql(-1), 2)),
                                labeler = new xymbolyco.GenusLabeler,
                                fMap = Map(2 -> true))

    assert(dfa.simulate(Seq(1,1)).contains(true),"did not match Seq(1,1)")
    assert(! dfa.simulate(Seq(-1,1)).contains(true),"did not fail to match Seq(-1,1)")
    assert(! dfa.simulate(Seq(1,-1)).contains(true),"did not fail to match Seq(1,-1)")

    val dfa1 = xymbolyco.Minimize.removeNonAccessible(dfa)
    assert(dfa1.q0id == 0)
    assert(dfa1.Qids == Set(0,1,2,4))
    assert(dfa1.Fids == Set(2))
    val dfa2 = xymbolyco.Minimize.removeNonCoAccessible(dfa)
    assert(dfa2.q0id == 0)
    assert(dfa2.Qids == Set(0,1,2,3))
    assert(dfa2.Fids == Set(2))
    

    assert( dfa1.simulate(Seq(1,1)).contains(true),"did not match Seq(1,1)")
    assert( dfa2.simulate(Seq(1,1)).contains(true),"did not match Seq(1,1)")

    assert(! dfa1.simulate(Seq(-1,1)).contains(true),"did not fail to match Seq(-1,1)")
    assert(! dfa2.simulate(Seq(-1,1)).contains(true),"did not fail to match Seq(-1,1)")

    assert(! dfa1.simulate(Seq(1,-1)).contains(true),"did not fail to match Seq(1,-1)")
    assert(! dfa2.simulate(Seq(1,-1)).contains(true),"did not fail to match Seq(1,-1)")
  }
  test("dfa accessible 2") {
    val dfa = new xymbolyco.Dfa[Any,genus.SimpleTypeD,Boolean](Qids = Set(0, 1, 2, 3, 4, 5, 6, 7,8, 9, 10),
                                                               q0id = 0,
                                                               Fids = Set(2),
                                                               protoDelta = Set((0, genus.SEql(1), 1),
                                                                                (0, genus.SEql(-1), 4),
                                                                                (0, genus.SEql(-2), 6),
                                                                                (1, genus.SEql(1), 2),
                                                                                (3, genus.SEql(-1), 2),
                                                                                (8, genus.SEql(-1), 5),
                                                                                (6, genus.SEql(-1), 7),
                                                                                (1, genus.SEql(-1), 9),
                                                                                (9, genus.SEql(-1), 10),
                                                                                (10, genus.SEql(-1), 9),
                                                                                (5, genus.SEql(-1), 2)),
                                                               labeler = new xymbolyco.GenusLabeler,
                                                               fMap = Map(2 -> true))

    assert(dfa.simulate(Seq(1,1)).contains(true),"did not match Seq(1,1)")
    assert(! dfa.simulate(Seq(-1,1)).contains(true),"did not fail to match Seq(-1,1)")
    assert(! dfa.simulate(Seq(1,-1)).contains(true),"did not fail to match Seq(1,-1)")

    val dfa1 = xymbolyco.Minimize.removeNonAccessible(dfa)
    assert(dfa1.q0id == 0)
    assert(dfa1.Qids == Set(0,1,2,4,6,7,9,10))
    assert(dfa1.Fids == Set(2))
    val dfa2 = xymbolyco.Minimize.removeNonCoAccessible(dfa)
    assert(dfa2.q0id == 0)
    assert(dfa2.Qids == Set(0,1,2,3,5,8))
    assert(dfa2.Fids == Set(2))


    assert( dfa1.simulate(Seq(1,1)).contains(true),"did not match Seq(1,1)")
    assert( dfa2.simulate(Seq(1,1)).contains(true),"did not match Seq(1,1)")

    assert(! dfa1.simulate(Seq(-1,1)).contains(true),"did not fail to match Seq(-1,1)")
    assert(! dfa2.simulate(Seq(-1,1)).contains(true),"did not fail to match Seq(-1,1)")

    assert(! dfa1.simulate(Seq(1,-1)).contains(true),"did not fail to match Seq(1,-1)")
    assert(! dfa2.simulate(Seq(1,-1)).contains(true),"did not fail to match Seq(1,-1)")
  }
}
