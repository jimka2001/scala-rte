/*
 * Copyright (c) 2019 EPITA Research and Development Laboratory
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package dfa

import org.scalatest.funsuite.AnyFunSuite


class DfaTestSuite extends AnyFunSuite {
  test("build bdd dfa") {
    import bdd._

    Bdd.withNewBddHash {
      val t1 = Bdd(1) // fixnum
      val t2 = Bdd(2) // integer
      val t3 = Bdd(3) // number
      val t4 = Bdd(4) // string
      val t6 = And(Not(t1), t3) // !fixnum & number
      val t7 = And(Not(t2), t3) // !integer & number
      val t9 = Bdd(9) // symbol

      val dfa = new Dfa[Bdd, String](Set(0, 1, 2, 4, 5, 6, 7, 8),
                                     0,
                                     Set(4, 5, 6, 7),
                                     Set((0, t1, 1),
                                         (0, t4, 2),
                                         (0, t9, 8),
                                         (1, t6, 4),
                                         (1, t1, 5),
                                         (1, t7, 6),
                                         (2, t3, 7),
                                         (8, t7, 6),
                                         (8, t2, 7)),
                                     (a: Bdd, b: Bdd) => Or(a, b),
                                     Map(4 -> "clause-2",
                                         5 -> "clause-1",
                                         6 -> "clause-3",
                                         7 -> "clause-3"))
      assert(dfa.F.size == 4)
      assert(dfa.q0.id == 0)
      assert(dfa.F.map(_.id) == Set(4, 5, 6, 7))
    }
  }
  test("build bdd dfa 2") {
    import bdd._

    Bdd.withNewBddHash {
      val t1 = Bdd(1) // fixnum
      val t2 = Bdd(2) // integer
      val t3 = Bdd(3) // number
      val t4 = Bdd(4) // string
      val t6 = And(Not(t1), t3) // !fixnum & number
      val t7 = And(Not(t2), t3) // !integer & number
      val t9 = Bdd(9) // symbol

      val dfa = new Dfa[Bdd, String](Set(0, 1, 2, 4, 5, 6, 7, 8),
                                     0,
                                     Set(4, 5, 6, 7),
                                     Set((0, t1, 1),
                                         (0, t4, 2),
                                         (0, t9, 8), // mergable
                                         (0, t4, 8), // mergable
                                         (1, t6, 4),
                                         (1, t1, 5),
                                         (1, t7, 6),
                                         (2, t3, 7),
                                         (8, t7, 6),
                                         (8, t2, 7)),
                                     (a: Bdd, b: Bdd) => Or(a, b),
                                     Map(4 -> "clause-2",
                                         5 -> "clause-1",
                                         6 -> "clause-3",
                                         7 -> "clause-3"))
      assert(dfa.F.size == 4)
      assert(dfa.q0.id == 0)
      assert(dfa.F.map(_.id) == Set(4, 5, 6, 7))
    }
  }
  test("build set dfa") {

    val t1 = Set(1) // fixnum
    val t2 = Set(1, 2) // integer
    val t3 = Set(1, 2, 3) // number
    val t4 = Set(4) // string
    val t6 = t3.diff(t1) // !fixnum & number
    val t7 = t3.diff(t2) // !integer & number
    val t9 = Set(9) // symbol

    val dfa = new Dfa[Set[Int], String](Set(0, 1, 2, 4, 5, 6, 7, 8),
                                        0,
                                        Set(4, 5, 6, 7),
                                        Set((0, t1, 1),
                                            (0, t4, 2),
                                            (0, t9, 8), // mergable
                                            (0, t4, 8), // mergable
                                            (1, t6, 4),
                                            (1, t1, 5),
                                            (1, t7, 6),
                                            (2, t3, 7),
                                            (8, t7, 6),
                                            (8, t2, 7)),
                                        (a: Set[Int], b: Set[Int]) => a.union(b),
                                        Map(4 -> "clause-2",
                                            5 -> "clause-1",
                                            6 -> "clause-3",
                                            7 -> "clause-3"))
    assert(dfa.F.size == 4)
    assert(dfa.q0.id == 0)
    assert(dfa.F.map(_.id) == Set(4, 5, 6, 7))
    assert(dfa.findState(0).transitions.size == 3)
    assert(dfa.findState(0).transitions.exists(tr => tr.label == t9.union(t4)))
    assert(dfa.exitValue(dfa.findState(5)) == "clause-1")
    assert(dfa.exitValue(dfa.findState(6)) == "clause-3")
    assert(dfa.exitValue(dfa.findState(4)) == "clause-2")
    assert(dfa.exitValue(dfa.findState(7)) == "clause-3")
  }

  test("minimize dfa") {

    val t1 = Set("a1") // fixnum
    val t2 = Set("a1", "b2") // integer
    val t3 = Set("a1", "b2", "c3") // number
    val t4 = Set("d4") // string
    val t6 = t3.diff(t1) // !fixnum & number
    val t7 = t3.diff(t2) // !integer & number
    val t9 = Set("e9") // symbol

    val dfa = new Dfa[Set[String], String](Set(0, 1, 2, 4, 5, 6, 7, 8),
                                        0,
                                        Set(4, 5, 6, 7),
                                        Set((0, t1, 1),
                                            (0, t4, 2),
                                            (0, t9, 8), // mergable
                                            (0, t4, 8), // mergable
                                            (1, t6, 4),
                                            (1, t1, 5),
                                            (1, t7, 6),
                                            (2, t3, 7),
                                            (8, t7, 6),
                                            (8, t2, 7)),
                                        (a: Set[String], b: Set[String]) => a.union(b),
                                        Map(4 -> "clause-2",
                                            5 -> "clause-1",
                                            6 -> "clause-3",
                                            7 -> "clause-3"))
    val minDfa = Minimize.minimize(dfa)
    assert(minDfa.F.size == 3)
    assert(minDfa.F.map(q => minDfa.exitValue(q)) == Set("clause-1","clause-2","clause-3"))
    assert(minDfa.Q.size == 6)
  }

  test("minimize dfa 2") {

    val t1 = Set("a1") // fixnum
    val t2 = Set("a1", "b2") // integer
    val t3 = Set("a1", "b2", "c3") // number
    val t4 = Set("d4") // string
    val t6 = t3.diff(t1) // !fixnum & number
    val t7 = t3.diff(t2) // !integer & number

    val dfa = new Dfa[Set[String], String](Set(0, 1, 2, 4, 5, 6, 7),
                                           0,
                                           Set(4, 5, 6, 7),
                                           Set((0, t1, 1),
                                               (0, t4, 2),
                                               (1, t6, 4),
                                               (1, t1, 5),
                                               (1, t7, 6),
                                               (2, t3, 7)),
                                           (a: Set[String], b: Set[String]) => a.union(b),
                                           Map(4 -> "clause-2",
                                               5 -> "clause-1",
                                               6 -> "clause-3",
                                               7 -> "clause-3"))
    val minDfa = Minimize.minimize(dfa)
    assert(minDfa.F.size == 3)
    assert(minDfa.F.map(q => minDfa.exitValue(q)) == Set("clause-1","clause-2","clause-3"))
    assert(minDfa.Q.size == 6)
  }

  test("render dfa") {

    val t1 = Set("a1") // fixnum
    val t2 = Set("a1", "b2") // integer
    val t3 = Set("a1", "b2", "c3") // number
    val t4 = Set("d4") // string
    val t6 = t3.diff(t1) // !fixnum & number
    val t7 = t3.diff(t2) // !integer & number

    val dfa = new Dfa[Set[String], String](Set(0, 1, 2, 4, 5, 6, 7),
                                           0,
                                           Set(4, 5, 6, 7),
                                           Set((0, t1, 1),
                                               (0, t4, 2),
                                               (1, t6, 4),
                                               (1, t1, 5),
                                               (1, t7, 6),
                                               (2, t3, 7)),
                                           (a: Set[String], b: Set[String]) => a.union(b),
                                           Map(4 -> "clause-2",
                                               5 -> "clause-1",
                                               6 -> "clause-3",
                                               7 -> "clause-3"))
    assert(dfa.Q.size == 7)
    Render.dfaToPng(dfa,"test render")
    //Render.dfaView(dfa,"test render")
    val minDfa = Minimize.minimize(dfa)
    Render.dfaToPng(minDfa, "test render minimized")
    //Render.dfaView(minDfa,"test render minimized")
  }

  test("simulate dfa"){
    import Simulate._

    val t1 = Set("a1") // fixnum
    val t2 = Set("a1", "b2") // integer
    val t3 = Set("a1", "b2", "c3") // number
    val t4 = Set("d4") // string
    val t6 = t3.diff(t1) // !fixnum & number
    val t7 = t3.diff(t2) // !integer & number

    val dfa = new Dfa[Set[String], String](Set(0, 1, 2, 4, 5, 6, 7),
                                           0,
                                           Set(4, 5, 6, 7),
                                           Set((0, t1, 1), // a1
                                               (0, t4, 2), // d4
                                               (1, t6, 4), // b2 c3 == !(a1) & (a1 b2 c3)
                                               (1, t1, 5), // a1
                                               (1, t7, 6), // c3 == !(a1 b2) & (a1 b2 c3)
                                               (2, t3, 7)), // a1 b2 c3
                                           (a: Set[String], b: Set[String]) => a.union(b),
                                           Map(4 -> "clause-2",
                                               5 -> "clause-1",
                                               6 -> "clause-3",
                                               7 -> "clause-3"))
    def accept(s:String,label:Set[String]):Boolean = {
        label.contains(s)
    }
    assert(simulate(dfa,accept)(List("a1","a1")) == Some("clause-1"))
    assert(simulate(dfa,accept)(List()) == None)
    assert(simulate(dfa,accept)(Array("a1", "b2")) == Some("clause-2"))
    assert(simulate(dfa,accept)(Array("a1", "b2","b2")) == None)
  }
}
