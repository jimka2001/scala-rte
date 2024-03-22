// Copyright (©) 2021 EPITA Research and Development Laboratory
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

import rte._
import genus._
import Extract._
import GraphViz._
import adjuvant.MyFunSuite
import org.scalatest.concurrent.{Signaler, ThreadSignaler}
import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.time.{Millis, Span}
import org.scalatest.time.SpanSugar._

//noinspection RedundantDefaultArgument
class ExtractTestSuite  extends MyFunSuite {
  val Σ: Sigma.type = Sigma
  val ε: EmptyWord.type = EmptyWord
  val t2x: Singleton = Singleton(SAtomic(classOf[genus.RandomType.Trait2X]))
  val etrue: Singleton = Singleton(SEql(true))

  test("extraction 31"){
    check_extraction_cycle(Cat(Singleton(SEql(1)),Singleton(SEql(2))), 1, 1)
  }
  test("extraction 34") {
    SAtomic.withClosedWorldView {
      check_extraction_cycle(Star(Cat(Or(Cat(Σ, Σ, Star(Σ)), ε),
                                      t2x,
                                      Singleton(SAtomic(classOf[Integer])),
                                      etrue,
                                      Singleton(STop))),
                             1, 1)
    }
  }
  test("extraction 35") {
    SAtomic.withClosedWorldView {
      check_extraction_cycle(Star(Cat(Cat(Or(Cat(Σ, Σ, Star(Σ)), ε),
                                          Cat(t2x, Singleton(SAtomic(classOf[Integer])))),
                                      Cat(etrue, Singleton(STop)))),
                             1, 1)
    }
  }
  def check_extraction_cycle(rt: Rte, depth:Int, rep:Int): Unit = {
    println(s"depth=$depth   rep=$rep")
    val rt1 = rt.canonicalize
    val dfa1 = rt1.toDfa(exitValue=true)
    val extracted = dfaToRte[Boolean](dfa1,true)
    //println(s"extracted=$extracted")
    if (extracted.contains(true)) {
      val rt2 = extracted(true)
      //println(s"  rt2=$rt2")
      // compute xor, should be emptyset    if rt1 is equivalent to rt2
      val empty1 = Xor(rt1,rt2).canonicalize
      val empty_dfa = empty1.toDfa(true)
      val label_path = empty_dfa.vacuous() match {
        case None => empty_dfa.findTrace(requireSatisfiable=false)
        case Some(false) => empty_dfa.findTrace(requireSatisfiable=true)
        case Some(true) => None
      }
      if (empty_dfa.vacuous().contains(false)) {
        dfaView(rt1.toDfa(true),title="rt1")
        dfaView(empty_dfa,title="empty_dfa")
      }
      assert(!empty_dfa.vacuous().contains(false),
             s"\nrt1=$rt1\n" +
               s"rt2=$rt2\n" +
               s"empty=$empty1\n" +
               s"path=$label_path"
             )
    }
  }

  test("test_extract_rte") {
    // test should fail if cannot run within 60 seconds
    // perhaps need to adjust number of seconds
    val start = System.nanoTime()
    implicit val signaler: Signaler = ThreadSignaler
    failAfter(Span(60000, Millis)) {
      SAtomic.withClosedWorldView {
        for {depth <- 1 to 3
             rep <- 0 to 25
             rt = Rte.randomRte(depth)
             } locally{
          val now = System.nanoTime()
          print(s"elapsed = ${(now - start)/(1e9)} ")
          check_extraction_cycle(rt, depth, rep)}
      }
    }
  }
}
