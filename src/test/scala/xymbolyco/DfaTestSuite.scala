/*
 * Copyright (c) 2019,21 EPITA Research and Development Laboratory
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

package xymbolyco

import adjuvant.AdjFunSuite
import org.scalatest.funsuite.AnyFunSuite
import xymbolyco.Minimize.trim

class DfaTestSuite extends AdjFunSuite {

  import bdd._

  class StringLabelerT1 extends Labeler[String,Set[String]] {
    def member(x:String,s:Set[String]):Boolean = s.contains(x)
    def combineLabels(a:Set[String],b:Set[String]):Set[String] = a.union(b)
    override def universal(a:Set[String]):Boolean = false

    override def inhabited(a: Set[String]): Option[Boolean] = {
      Some(a.nonEmpty)
    }
  }
  class IntLabelerT1 extends Labeler[Int,Set[Int]] {
    def member(x:Int,s:Set[Int]):Boolean = s.contains(x)
    def combineLabels(a:Set[Int],b:Set[Int]):Set[Int] = a.union(b)
    override def universal(a:Set[Int]):Boolean = false
    override def inhabited(a: Set[Int]): Option[Boolean] = {
      Some(a.nonEmpty)
    }
  }

  class BddLabelerT2 extends Labeler[Int,Bdd] {
    def member(x:Int,b:Bdd):Boolean = ???
    def combineLabels(a:Bdd,b:Bdd):Bdd = Or(a, b)
    override lazy val universe:Bdd = BddTrue
    override def subtractLabels(a:Bdd,others:Seq[Bdd]):Bdd = {
      bdd.AndNot.apply(a::others.toList)
    }
    override def inhabited(a:Bdd):Option[Boolean] = {
      Some(a != BddFalse)
    }
  }

  test("build bdd dfa") {

    Bdd.withNewBddHash {
      val t1 = Bdd(1) // fixnum
      val t2 = Bdd(2) // integer
      val t3 = Bdd(3) // number
      val t4 = Bdd(4) // string
      val t6 = And(Not(t1), t3) // !fixnum & number
      val t7 = And(Not(t2), t3) // !integer & number
      val t9 = Bdd(9) // symbol

      val dfa = Dfa[Int, Bdd, String](Qids = Set(0, 1, 2, 4, 5, 6, 7, 8),
                                          q0id = 0,
                                          Fids = Set(4, 5, 6, 7),
                                          protoDelta = Set((0, t1, 1),
                                                           (0, t4, 2),
                                                           (0, t9, 8),
                                                           (1, t6, 4),
                                                           (1, t1, 5),
                                                           (1, t7, 6),
                                                           (2, t3, 7),
                                                           (8, t7, 6),
                                                           (8, t2, 7)),
                                          new BddLabelerT2(),
                                          fMap = Map(4 -> "clause-2",
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

      val dfa = Dfa[Int, Bdd, String](Set(0, 1, 2, 4, 5, 6, 7, 8),
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
                                          new BddLabelerT2,
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

    val dfa = Dfa[Int, Set[Int], String](Set(0, 1, 2, 4, 5, 6, 7, 8),
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
                                         new IntLabelerT1,
                                         Map(4 -> "clause-2",
                                             5 -> "clause-1",
                                             6 -> "clause-3",
                                             7 -> "clause-3"))
    assert(dfa.F.size == 4)
    assert(dfa.q0.id == 0)
    assert(dfa.F.map(_.id) == Set(4, 5, 6, 7))
    assert(dfa.idToState(0).transitions.size == 3)
    assert(dfa.idToState(0).transitions.exists(tr => tr.label == t9.union(t4)))
    assert(dfa.exitValue(dfa.idToState(5)) == "clause-1")
    assert(dfa.exitValue(dfa.idToState(6)) == "clause-3")
    assert(dfa.exitValue(dfa.idToState(4)) == "clause-2")
    assert(dfa.exitValue(dfa.idToState(7)) == "clause-3")
  }

  test("simulate set dfa") {
    val dfa = Dfa[Int, Set[Int], Int](Set(0,1,2,3),
                                         0,
                                         Set(1,2),
                                         Set((0, Set(1), 1),
                                             (0, Set(2), 2),
                                             (2, Set(1,2), 3),
                                             (1, Set(0), 0)
                                             ),
                                         new IntLabelerT1,
                                         Map(1 -> 42,
                                             2 -> 43))
    assert(dfa.simulate(Seq[Int]()) == None)
    assert(dfa.simulate(Seq(1)) == Some(42))
    assert(dfa.simulate(Seq(2)) == Some(43))
    assert(dfa.simulate(Seq(1,0,2)) == Some(43))
    assert(dfa.simulate(Seq(1,0,2,0)) == None)
  }

  test("minimize dfa") {

    val t1 = Set("a1") // fixnum
    val t2 = Set("a1", "b2") // integer
    val t3 = Set("a1", "b2", "c3") // number
    val t4 = Set("d4") // string
    val t6 = t3.diff(t1) // !fixnum & number
    val t7 = t3.diff(t2) // !integer & number
    val t9 = Set("e9") // symbol

    val dfa = Dfa[String, Set[String], String](Set(0, 1, 2, 4, 5, 6, 7, 8),
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
                                                   new StringLabelerT1,
                                                   Map(4 -> "clause-2",
                                                       5 -> "clause-1",
                                                       6 -> "clause-3",
                                                       7 -> "clause-3"))
    val minDfa = Minimize.minimize(dfa)
    assert(minDfa.F.size == 3)
    assert(minDfa.F.map(q => minDfa.exitValue(q)) == Set("clause-1", "clause-2", "clause-3"))
    assert(minDfa.Q.size == 6)
  }

  test("minimize dfa 2") {

    val t1 = Set("a1") // fixnum
    val t2 = Set("a1", "b2") // integer
    val t3 = Set("a1", "b2", "c3") // number
    val t4 = Set("d4") // string
    val t6 = t3.diff(t1) // !fixnum & number
    val t7 = t3.diff(t2) // !integer & number

    val dfa = Dfa[String, Set[String], String](Set(0, 1, 2, 4, 5, 6, 7),
                                                   0,
                                                   Set(4, 5, 6, 7),
                                                   Set((0, t1, 1),
                                                       (0, t4, 2),
                                                       (1, t6, 4),
                                                       (1, t1, 5),
                                                       (1, t7, 6),
                                                       (2, t3, 7)),
                                                   new StringLabelerT1,
                                                   Map(4 -> "clause-2",
                                                       5 -> "clause-1",
                                                       6 -> "clause-3",
                                                       7 -> "clause-3"))
    val minDfa = Minimize.minimize(dfa)
    assert(minDfa.F.size == 3)
    assert(minDfa.F.map(q => minDfa.exitValue(q)) == Set("clause-1", "clause-2", "clause-3"))
    assert(minDfa.Q.size == 6)
  }

  test("render dfa") {
    val t1 = Set("a1") // fixnum
    val t2 = Set("a1", "b2") // integer
    val t3 = Set("a1", "b2", "c3") // number
    val t4 = Set("d4") // string
    val t6 = t3.diff(t1) // !fixnum & number
    val t7 = t3.diff(t2) // !integer & number

    val sdfa = Dfa[String, Set[String], String](Set(0, 1, 2, 4, 5, 6, 7),
                                                   0,
                                                   Set(4, 5, 6, 7),
                                                   Set((0, t1, 1),
                                                       (0, t4, 2),
                                                       (1, t6, 4),
                                                       (1, t1, 5),
                                                       (1, t7, 6),
                                                       (2, t3, 7)),
                                                   new StringLabelerT1,
                                                   Map(4 -> "clause-2",
                                                       5 -> "clause-1",
                                                       6 -> "clause-3",
                                                       7 -> "clause-3"))
    assert(sdfa.Q.size == 7)
    xymbolyco.GraphViz.dfaToPng(sdfa, "test render", abbrev = false)
    //Render.dfaView(dfa,"test render")
    val minDfa = Minimize.minimize(sdfa)
    xymbolyco.GraphViz.dfaToPng(minDfa, "test render minimized", abbrev = false)
    //Render.dfaView(minDfa,"test render minimized")
  }

  test("simulate dfa") {

    val t1 = Set("a1") // fixnum
    val t2 = Set("a1", "b2") // integer
    val t3 = Set("a1", "b2", "c3") // number
    val t4 = Set("d4") // string
    val t6 = t3.diff(t1) // !fixnum & number
    val t7 = t3.diff(t2) // !integer & number

    val dfa = Dfa[String, Set[String], String](Set(0, 1, 2, 4, 5, 6, 7),
                                                   0,
                                                   Set(4, 5, 6, 7),
                                                   Set((0, t1, 1), // a1
                                                       (0, t4, 2), // d4
                                                       (1, t6, 4), // b2 c3 == !(a1) & (a1 b2 c3)
                                                       (1, t1, 5), // a1
                                                       (1, t7, 6), // c3 == !(a1 b2) & (a1 b2 c3)
                                                       (2, t3, 7)), // a1 b2 c3
                                                   new StringLabelerT1,
                                                   Map(4 -> "clause-2",
                                                       5 -> "clause-1",
                                                       6 -> "clause-3",
                                                       7 -> "clause-3"))

    assert(dfa.simulate(List("a1", "a1")).contains("clause-1"))
    assert(dfa.simulate(List()).isEmpty)
    assert(dfa.simulate(Vector("a1", "b2")).contains("clause-2"))
    assert(dfa.simulate(Vector("a1", "b2", "b2")).isEmpty)
  }
  test("sxp"){
    import genus._
    val t0 = SAtomic(classOf[String])
    val dfa1 = locally {
      val t1 = SEql(0)
      val t2 = SOr(SAnd(SNot(t1), SNot(SMember(-1, 0, 1))),
                   SMember(-1, 1))
      val t3 = STop
      Dfa(Qids = Set(0, 1, 2),
          q0id = 0,
          Fids = Set(2),
          protoDelta = Set((0, t0, 0),
                           (0, t1, 2),
                           (0, t2, 1),
                           (2, t3, 1),
                           (1, t3, 1)),
          labeler = GenusLabeler(),
          fMap = Map(2 -> 4))
    }
    val dfa2 = locally{
      val t1 = SEql(-1)
      Dfa(Qids= Set(0,2),
              q0id = 0,
              Fids=Set(2),
              protoDelta = Set((0,t0,0),
                               (0,t1,2)),
              labeler=GenusLabeler(),
              fMap=Map(2 -> 3)
              )
    }
    val s = Minimize.sxp[Any,SimpleTypeD,Int](trim(dfa1),trim(dfa2),
                         (a:Boolean,b:Boolean) => (a || b))
    assert(s.Fids.size >= 2)
  }

  test("sxp 2"){
    import genus._
    val dfa1 = locally {
      val t0:SimpleTypeD = SEql(-1)
      Dfa(Qids = Set(0, 1),
              q0id = 0,
              Fids = Set(1),
              protoDelta = Set((0, t0, 1)),
              labeler=GenusLabeler(),
              fMap=Map(1 -> 10))
    }
    val dfa2 = locally{
      val t0:SimpleTypeD = SEql(-2)
      Dfa(Qids= Set(0,1),
              q0id = 0,
              Fids=Set(1),
              protoDelta = Set((0,t0,1)),
              labeler=GenusLabeler(),
              fMap=Map(1 -> 20)
              )
    }
    val s = Minimize.sxp(dfa1,dfa2,
                         (a:Boolean,b:Boolean) => a || b)
    assert(s.Fids.size >= 2)
    s.simulate(Seq(-1)).contains(10)
    s.simulate(Seq(-2)).contains(20)
    s.simulate(Seq(0)).isEmpty
  }

  test("spanning path 0"){
    import rte.{And,Not,Star,Singleton}
    import genus.SAtomic
    // this test checks the normal behavior of rteCase
    val int = Singleton(SAtomic(classOf[Int]))
    val str = Singleton(SAtomic(classOf[String]))
    val dfa = And(Star(str), Not(Star(int))).toDfa(1)
    assert( dfa.spanningPath != None)
  }

  test("spanning path"){
    import genus.{SAnd, SAtomic, SEql, SOr, SSatisfies, SimpleTypeD}
    import rte.{Or, Rte, Singleton, Xor}
    val data1 = Seq("XY", 1, 0.5F, 2, 0.8F, 5, 1.2F, 7, 0.4F, // 1 or more x,y where x is integer and y is float
                    "M", 0.1, 0.3, 4.5, // 1 or more double or float all positive
                    "C", 1, 5, 7, 8, // 1 ore more ints, all positive
                    "C", 2, 5, 3,
                    "M", 0.5, 1.2
                    )
    val D:SimpleTypeD = SAtomic(classOf[Double])
    val F:SimpleTypeD = SAtomic(classOf[Float])
    val DF:Rte = Singleton(SOr(F, D))

    def positive(x:Any):Boolean = {
      x match {
        case a:Int => a > 0
        case _ => false
      }
    }
    val IPos:Rte = Singleton(SAnd(SAtomic(classOf[Int]),
                                  SSatisfies(positive, "IPos")))
    val M:Rte = Singleton(SEql("M"))
    val C:Rte = Singleton(SEql("C"))
    val XY:Rte = Singleton(SEql("XY"))

    val XYclause:Rte = XY ++ (Singleton(SAtomic(classOf[Int])) ++ DF).+
    val Mclause:Rte = M ++ DF.+
    val Cclause:Rte = C ++ IPos.+

    //assert(Mclause.contains(Seq("M", 0.5F, 0.8, 1.2F, 0.4F)))
    //assert(XYclause.contains(Seq("XY", 1, 0.5F, 2, 0.8F, 5, 1.2F, 7, 0.4F)))
    //assert(Cclause.contains(Seq("C", 2, 5, 3)))

    val pattern1:Rte = Or(XYclause, Mclause, Cclause).*

    val pattern2:Rte = ((XY ++ (IPos ++ DF).+) | (M ++ DF.+) | (C ++ IPos.+)).*

    assert(true == pattern1.contains(data1))
    assert(true == pattern2.contains(data1))
    val diff = Xor(pattern2 ,pattern1)
    val dfa = diff.toDfa(exitValue=true)
    val qs = dfa.spanningPath match {
      case Some((Satisfiable, qs)) => {
        assert(qs.length == 4)
        qs
      }
      case Some((Indeterminate, qs)) => {
        assert (qs.length == 4)
        qs
      }
      case _ => List()
    }
    dfa.spanningTrace match {
      case Some((Satisfiable,tds)) => assert(tds.length == 3)
      case Some((Indeterminate,tds)) => assert(tds.length == 3)
      case _ => ()
    }
    val m = dfa.findSpanningPathMap()
    val (x, qs2) = m(true)
    assert(qs == qs2)
  }

  test("complement 0"){
    import rte.{Rte,Not,Or, And, Atomic, Singleton}
    import rte.Rte.sigmaStar
    import xymbolyco.GraphViz.dfaView
    import genus.Types.oddType

    val rte1 = And(Not(Singleton(oddType())), Atomic(classOf[Int])) ++ Atomic(classOf[Number])
    val rte2 = Not(rte1)
    //val dfa1 = rte1.toDfa()
    val dfa2 = rte2.toDfa()

    //dfaView(dfa1, title = "dfa of rte", dotFileCB=(name:String)=>{println(s"dot file name=$name")})
    dfaView(dfa2, title = "dfa of negated rte", dotFileCB=(name:String)=>{println(s"dot file name=$name")})

  }

  test("complement 1"){
    import rte.{Rte,Not,Or, Atomic}
    import rte.Rte.sigmaStar
    import xymbolyco.GraphViz.dfaView
    import xymbolyco.Dfa.dfaXor
    // we should get equivalent Dfas if we build a Dfa from a negated rte
    // or if we build the Dfa, then negate it.

    val rte1 = Or(Atomic(classOf[Int]), Atomic(classOf[Number])) ++ sigmaStar
    val rte2 = Not(rte1)
    val dfa1 = rte1.toDfa().negate(true)
    val dfa2 = rte2.toDfa()
    val xor = dfaXor(dfa1,dfa2)
    val empty = xor.vacuous()


    dfaView(dfa1, title = "dfa1: negated dfa")
    dfaView(dfa2, title = "dfa2: dfa of negated rte")
    dfaView(xor, title = "xor of dfa1 and dfa2")
    if (empty == Some(false) || empty == None) {

      println(s"rte1=$rte1")
      dfaView(dfa1, title = "dfa1: negated dfa")
      dfaView(dfa2, title = "dfa2: dfa of negated rte")
      dfaView(xor, title = "xor of dfa1 and dfa2")
    }
    assert(empty == Some(true) || empty == None,
           s"rte1=$rte1")
  }

  test("complement 2"){
    import rte.{Rte,Not}
    import xymbolyco.GraphViz.dfaView
    import xymbolyco.Dfa.dfaXor
    // we should get equivalent Dfas if we build a Dfa from a negated rte
    // or if we build the Dfa, then negate it.
    for {depth <- 2 to 4
         n <- 0 to 50
         rte1 = Rte.randomRte(depth)
         rte2 = Not(rte1)
         dfa1 = rte1.toDfa().negate(true)
         dfa2 = rte2.toDfa()
         xor = dfaXor(dfa1,dfa2)
         empty = xor.vacuous()
         } {
      if (empty == Some(false) || empty == None) {

        println(s"depth=$depth n=$n rte1=$rte1")
        dfaView(dfa1, title = "dfa1: negated dfa")
        dfaView(dfa2, title = "dfa2: dfa of negated rte")
        dfaView(xor, title = "xor of dfa1 and dfa2")
      }
      assert(empty == Some(true) || empty == None,
             s"depth=$depth n=$n rte1=$rte1")
    }
  }
}
