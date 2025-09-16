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
// NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package xymbolyco

import genus.RandomType._
import genus.Types.isOdd
import genus._
import org.scalatest.funsuite.AnyFunSuite
import rte._

// these 5 test cases contain RTEs which Minimised Brozozwski constructed DFAs present some undeterministic constructions
// the obtained DFAs will not always be of the same size without anything in the code changing. the results are seemingly
// random. However, the obtained DFAs should be of the same size of the DFAs obtained by minimising the ones built
// using the thompson algorithm. The tests should be rerun in their entirety should they be tested.

class KnownBugsSuite extends AnyFunSuite {
  /*
  test("known bug A") {
    val rte = Star(Or(Singleton(SSatisfies(evenp, "evenp")), Singleton(SAtomic(classOf[ADT_abstr])), Singleton(SAtomic(classOf[Class2X]))))
    val dfa = Minimize.minimize(Minimize.trim(rte.toDfa(42)))
    assert(dfa.Qids.size == 1)
  }
  test("known bug B") {
    import genus.Types.isOdd
    val rte = Or(And(Star(Singleton(SSatisfies(isOdd, "oddp"))), Cat(Singleton(SSatisfies(evenp, "evenp")),
                                                                    Singleton(SMember("a", "b", "c")))), Star(Or(Singleton(SAtomic(classOf[Class1X])),
                                                                                                                 Singleton(SAtomic(classOf[Trait2X])))), Not(Star(Singleton(SAtomic(classOf[Trait3X])))))
    val dfa = Minimize.minimize(Minimize.trim(rte.toDfa(42)))
    assert(dfa.Qids.size == 2)
  }
  test("known bug C") {
    val rte = Cat(Star(Singleton(SSatisfies(evenp, "evenp"))), Or(Singleton(SAtomic(classOf[Trait2X])),
                                                                  Singleton(SAtomic(classOf[ADT1])), Singleton(SEql(false))), Singleton(SAtomic(classOf[Integer])))
    val dfa = Minimize.minimize(Minimize.trim(rte.toDfa(42)))
    assert(dfa.Qids.size == 5)
  }
  test("known bug D") {
    val rte = Or(And(Not(Singleton(SAtomic(classOf[String]))),
                     And(Singleton(SAtomic(classOf[Class2X])), Singleton(SAtomic(classOf[ADT_abstr])))),
                 Star(Singleton(SSatisfies(isOdd, "oddp"))))
    val dfa = Minimize.minimize(Minimize.trim(rte.toDfa(42)))
    assert(dfa.Qids.size == 1)
  }
  test("known bug E") {
    val rte = Cat(Star(Singleton(SOr(SAtomic(classOf[ADT1])))),
                  Or(Singleton(SMember(false, true)), Singleton(SAtomic(classOf[Boolean])),
                     Singleton(SMember("a", "b", "c")), Singleton(SMember(4, 5, 6))),
                  Cat(Singleton(SAtomic(classOf[Trait3X])), Singleton(SEql(1))))
    val dfa = Minimize.minimize(Minimize.trim(rte.toDfa(42)))
    assert(dfa.Qids.size == 4)
  }*/
}

