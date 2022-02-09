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
import org.scalatest.funsuite.AnyFunSuite
import rte._
import genus._
import Extract._
import GraphViz._

//noinspection RedundantDefaultArgument
class ExtractTestSuite  extends AnyFunSuite {
  val Σ: Sigma.type = Sigma
  val ε: EmptyWord.type = EmptyWord
  val t2x: Singleton = Singleton(SAtomic(classOf[genus.RandomType.Trait2X]))
  val etrue: Singleton = Singleton(SEql(true))
  val num_random_tests: Int = 1000
  test("extraction 31"){
    check_extraction_cycle(Cat(Singleton(SEql(1)),Singleton(SEql(2))))
  }
  test("extraction 34") {
    SAtomic.withClosedWorldView {
      check_extraction_cycle(Star(Cat(Or(Cat(Σ, Σ, Star(Σ)), ε),
                                      t2x,
                                      Singleton(SAtomic(classOf[Integer])),
                                      etrue,
                                      Singleton(STop))))
    }
  }
  test("extraction 35") {
    SAtomic.withClosedWorldView {
      check_extraction_cycle(Star(Cat(Cat(Or(Cat(Σ, Σ, Star(Σ)), ε),
                                          Cat(t2x, Singleton(SAtomic(classOf[Integer])))),
                                      Cat(etrue, Singleton(STop)))))
    }
  }
  def check_extraction_cycle(rt: Rte): Unit = {
    val rt1 = rt.canonicalize
    val extracted = dfaToRte[Boolean](rt1.toDfa(exitValue=true),true)
    //println(s"extracted=$extracted")
    if (extracted.contains(true)) {
      val rt2 = extracted(true)
      //println(s"  rt2=$rt2")
      // compute xor, should be emptyset    if rt1 is equivalent to rt2
      val empty1 = Xor(rt1,rt2).canonicalize
      val empty_dfa = empty1.toDfa(true)
      val label_path = empty_dfa.vacuous() match {
        case None => empty_dfa.findTrace(None)
        case Some(false) => empty_dfa.findTrace(Some(true))
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
    SAtomic.withClosedWorldView {
      for {depth <- 1 to 3
           _ <- 0 to num_random_tests
           rt = Rte.randomRte(depth)
           } check_extraction_cycle(rt)
    }
  }
}