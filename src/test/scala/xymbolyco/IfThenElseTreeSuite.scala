// Copyright (©) 2022 EPITA Research and Development Laboratory
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


package xymbolyco
import adjuvant.AdjFunSuite
import genus.{SEql, STop, SimpleTypeD}
import xymbolyco.GraphViz.dfaView
import xymbolyco.Minimize.complete


class IfThenElseTreeSuite extends AdjFunSuite {
  test("test 6") {
    val t1: SimpleTypeD = STop
    val dfa = Dfa(Qids = Set(0, 1, 2),
                  q0id = 0,
                  Fids = Set(0),
                  protoDelta = Set((0, t1, 0)),
                  labeler = GenusLabeler(),
                  fMap = Map(0 -> 4))
    val maybe: Option[State[Any, SimpleTypeD, Int]] = dfa.q0.successor(42)
    assert(maybe.nonEmpty)
    maybe.map(q => assert(q.id == 0))
  }

  test("test 19") {
    val t1: SimpleTypeD = SEql(42)
    val t2: SimpleTypeD = SEql(43)
    val dfa = Dfa(Qids = Set(0, 1, 2),
                  q0id = 0,
                  Fids = Set(0),
                  protoDelta = Set((0, t1, 1),
                                   (0, t2, 2)),
                  labeler = GenusLabeler(),
                  fMap = Map(0 -> 4))

    dfa.q0.successor(42).map(q => assert(q.id == 1))
    dfa.q0.successor(43).map(q => assert(q.id == 2))
    assert(dfa.q0.successor(44).isEmpty)
  }
  test("test 36") {
    val t1: SimpleTypeD = SEql(42)
    val t2: SimpleTypeD = SEql(43)
    val dfa = complete(Dfa(Qids = Set(0, 1, 2),
                  q0id = 0,
                  Fids = Set(0),
                  protoDelta = Set((0, t1, 1),
                                   (0, t2, 2),
                                   (2, t1, 0),
                                   (1, t2, 0)),
                  labeler = GenusLabeler(),
                  fMap = Map(0 -> 4)))

    assert(dfa.simulate(Seq(42,43)).contains(4))
    assert(dfa.simulate(Seq(43,42)).contains(4))
    assert(dfa.simulate(Seq(42,43,43,42)).contains(4))
    assert(dfa.simulate(Seq(43,42,42,0)).isEmpty)
  }
}
