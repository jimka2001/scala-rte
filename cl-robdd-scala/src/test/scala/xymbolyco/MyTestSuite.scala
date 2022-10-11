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
// WITH THE SOFTWARE OR THE USE OR OTHER D  EALINGS IN THE SOFTWARE.

package xymbolyco


import java.io.{File, PrintWriter}

import genus.RandomType.{Abstract1, Trait3X}
import genus._
import org.scalatest.funsuite.AnyFunSuite
import rte._
import xymbolyco.GraphViz.dfaView
import xymbolyco.Minimize.minimize
import xymbolyco.Thompson._
import xymbolyco.Profiling.check
import genus.RandomType._

class MyTestSuite extends AnyFunSuite {

  test("statistics tests") {
    Profiling.statisticsTests()
    val rte = SAnd(SAtomic(classOf[Trait3X]),SNot(SAtomic(classOf[ADT1])),
                  SNot(SAtomic(classOf[ADT2])),SNot(SAtomic(classOf[ADT3])),SSatisfies(evenp,"even"))
    val rte2 = SAnd(SAtomic(classOf[Trait3X]),SNot(SAtomic(classOf[ADT1])),
                   SNot(SAtomic(classOf[ADT2])),SNot(SAtomic(classOf[ADT3])))
    //println(rte2.disjoint(SSatisfies(evenp)))
    /*val pattern = Star(Cat(Singleton(SAtomic(classOf[Integer])),
                           Star(Singleton(SAtomic(classOf[String]))),
                           Singleton(SSatisfies(evenp,"evenp"))))

    val data = Profiling.check(pattern,1,1)

    dfaView(data("dfa_thompson"), "thompson", abbrev = true,
            label = Some(s"depth= " + pattern.toString))
    dfaView(data("dfa_brzozowski"), "brzozowski", abbrev = true,
            label = Some(s"depth= " + pattern.toString))
    dfaView(data("dfa_trim_thompson"), "trim-thompson", abbrev = true,
            label = Some(s"depth=r " + pattern.toString))
    dfaView(data("dfa_trim_brzozowski"), "trim-brzozowski", abbrev = true,
            label = Some(s"depth=r " + pattern.toString))
    dfaView(data("min_thompson"), "thompson-min", abbrev = true,
            label = Some(s"depth=r " + pattern.toString))
    dfaView(data("min_brzozowski"), "brzozowski-min", abbrev = true,
            label = Some(s"depth=r " + pattern.toString))
*/
  }
}
