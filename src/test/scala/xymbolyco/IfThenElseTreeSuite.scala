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
import genus.Types.oddType
import genus.{SAtomic, SEql, SNot, STop, SimpleTypeD}
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

  test("ite 2"){
    val N = SAtomic(classOf[Number])
    val I = SAtomic(classOf[Int])
    val odd = oddType()
    val transitions:Set[(SimpleTypeD, Int)] = Set((I && odd, 1),
                                                  (I && !odd , 2),
                                                  (!I && odd, 3),
                                                  (!I && !odd, 4))
    val ite = IfThenElseTree[SimpleTypeD,Int](List(I, odd),transitions)
    println(ite)
    println(List("hello", 1, 2, 1.0).map(ite.apply))
  }

  test("ite 1"){
    val N = SAtomic(classOf[Number])
    val I = SAtomic(classOf[Int])

    val transitions:Set[(SimpleTypeD, Int)] = Set((N && !I, 1),
                                                  (I, 2),
                                                  (!N, 3))

    val ite = IfThenElseTree[SimpleTypeD,Int](List(N,I), transitions)
    //println(ite)
    ite match {
      case ite@IfThenElseNode(_,_) =>
        assert(! ite.ifTrueEvaluated)
        assert(! ite.ifFalseEvaluated)
      case _ => fail(s"ite=$ite")
    }


    assert(ite(100) == Some(2))
    ite match {
      case ite@IfThenElseNode(_, _) =>
        assert(ite.ifTrueEvaluated)
        assert(!ite.ifFalseEvaluated)
      case _ => fail(s"ite=$ite")
    }
    //println(ite)

    ite match {
      case ite@IfThenElseNode(tds,transitions) =>
        assert(tds == List(N,I))
        assert(transitions == Set((N && !I,1),
                                  (I,2),
                                  (!N,3)))
        ite.ifTrue match {
          case ite@IfThenElseNode(tds,transitions) =>
            assert(tds == List(I))
            //println(ite)
            assert(ite.ifTrueEvaluated)
            assert(! ite.ifFalseEvaluated)
            assert(transitions == Set((!I,1),
                                      (I,2)))
            ite.ifTrue match {
              case IfThenElseFound(dest) => {
                assert(dest == 2)
              }
              case _ => fail("wrong format 1")
            }

          case _ => fail("wrong format case 2")
        }
      case _ => fail("wrong format case 3")
    }

    assert(ite(1.1) == Some(1))
    ite match {
      case ite@IfThenElseNode(_, _) =>
        assert(ite.ifTrueEvaluated)
        assert(!ite.ifFalseEvaluated)
      case _ => fail(s"ite=$ite")
    }
    //println(ite)

    ite match {
      case ite@IfThenElseNode(_,transitions) =>
        assert(transitions == Set((N && !I,1),
                                  (I,2),
                                  (!N,3)))
        ite.ifTrue match {
          case ite@IfThenElseNode(_,transitions) =>
            //println(ite)
            assert(ite.ifTrueEvaluated)
            assert(ite.ifFalseEvaluated)
            assert(transitions == Set((!I,1),
                                      (I,2)))
            ite.ifTrue match {
              case IfThenElseFound(dest) => {
                assert(dest == 2)
              }
              case _ => fail("wrong format 4b")
            }
            ite.ifFalse match {
              case IfThenElseFound(dest) =>
                assert(dest == 1)
              case _ => fail("wrong format 5b")
            }
          case _ => fail("wrong format case 6b")
        }
      case _ => fail("wrong format case 7b")
    }

    assert(ite("hello") == Some(3))
    //println(ite)
    ite match {
      case ite@IfThenElseNode(_, _) =>
        assert(ite.ifTrueEvaluated)
        assert(ite.ifFalseEvaluated)
      case _ => fail(s"ite=$ite")
    }
    ite match {
      case ite@IfThenElseNode(_,transitions) =>
        assert(transitions == Set((N && !I,1),
                                  (I,2),
                                  (!N,3)))
        ite.ifFalse match {
          case ite@IfThenElseFound(dest) =>
            assert(dest == 3)
          case _ => fail(s"wrong format 8c: ite.ifFalse = ${ite.ifFalse}")
        }
        ite.ifTrue match {
          case ite@IfThenElseNode(_,transitions) =>
            //println(ite)
            assert(ite.ifTrueEvaluated)
            assert(ite.ifFalseEvaluated)
            assert(transitions == Set((!I,1),
                                      (I,2)))
            ite.ifTrue match {
              case IfThenElseFound(dest) => {
                assert(dest == 2)
              }
              case _ => fail("wrong format 9c")
            }
            ite.ifFalse match {
              case IfThenElseFound(dest) =>
                assert(dest == 1)
              case _ => fail("wrong format 10c")
            }
          case _ => fail("wrong format case 11c")
        }
      case _ => fail("wrong format case 12c")
    }
  }
}
