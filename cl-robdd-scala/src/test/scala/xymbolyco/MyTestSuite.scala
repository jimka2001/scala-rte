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
import genus._
import org.scalatest.funsuite.AnyFunSuite
import rte._
import xymbolyco.GraphViz._
import RandomType.Trait2


class MyTestSuite extends AnyFunSuite {
  def foo(): Boolean = {
    val rte = Or(Not(Or(Singleton(SAtomic(classOf[Class2X])),
                        Singleton(SSatisfies(evenp, "evenp")), Singleton(SAtomic(classOf[Abstract2X])))),
                 Or(Or(Singleton(SAtomic(classOf[Integer])), Singleton(SAtomic(classOf[Abstract2X])),
                       Singleton(STop)), Star(Singleton(SAtomic(classOf[Abstract2]))),
                    And(Singleton(SMember("a", "b", "c")), Singleton(SAtomic(classOf[Trait1X])))),
                 Singleton(SAtomic(classOf[Boolean])), Star(Or(Singleton(SSatisfies(evenp, "evenp")),
                                                               Singleton(SMember(false, true)), Singleton(SEql(true)))))
    val data = Profiling.check(rte, 1, 1)
    val minbrz = data("min_brzozowski")
    val minThompson = data("min_thompson")
    if (minbrz.Q.size != minThompson.Q.size) {
      dfaView(minbrz, "mbrz", true, Some(rte.toString), false)
      dfaView(minThompson, "mthmp", true, Some(rte.toString), false)
    }
    minbrz.Q.size == minThompson.Q.size

  }

  test("d") {

    var i = 0
    while (foo()) {
      i += 1
      println(i)
    }

    /*
    val rte2 = Star(Or(Singleton(SAtomic(classOf[Trait2X])),Singleton(SAtomic(classOf[Trait1X])),Singleton(SSatisfies(oddp,"oddp"))
               ,Singleton(SOr(SAtomic(classOf[ADT1]),SAtomic(classOf[ADT2]),SAtomic(classOf[ADT3])))))
    val rte3 = Or(Not(Or(Singleton(SAtomic(classOf[Class2X])),
                         Singleton(SSatisfies(evenp,"evenp")),Singleton(SAtomic(classOf[Abstract2X])))),
                  Or(Or(Singleton(SAtomic(classOf[Integer])),Singleton(SAtomic(classOf[Abstract2X])),
                        Singleton(STop),Singleton(SEmpty)),Star(Singleton(SAtomic(classOf[Abstract2]))),
                     And(Singleton(SMember("a", "b", "c")),Singleton(SAtomic(classOf[Trait1X])))),
                     Not(Not(Singleton(SAtomic(classOf[Boolean])))),Star(Or(Singleton(SSatisfies(evenp,"evenp")),
                                      Singleton(SEql(-1)), Singleton(SMember(false,true)),Singleton(SEql(true)))))


    //unexplained yet
    val rte4= Star(Cat(Not(Singleton(SAtomic(classOf[Trait3X]))),Star(Singleton(STop)),And(Singleton(SSatisfies(oddp,"oddp")),
                                                                                          Singleton(SAtomic(classOf[ADT_abstr])))))

    val rte = Or(Not(Or(Singleton(SAtomic(classOf[Class2X])),
                        Singleton(SSatisfies(evenp,"evenp")),Singleton(SAtomic(classOf[Abstract2X])))),
                 Or(Or(Singleton(SAtomic(classOf[Integer])),Singleton(SAtomic(classOf[Abstract2X])),
                       Singleton(STop),Singleton(SEmpty)),Star(Singleton(SAtomic(classOf[Abstract2]))),
                    And(Singleton(SMember("a", "b", "c")),Singleton(SAtomic(classOf[Trait1X])))),
                 Not(Not(Singleton(SAtomic(classOf[Boolean])))))
    val data = Profiling.check(rte,1,1)
    val minbrz = data("min_brzozowski")
    val minThompson = data("min_thompson")
    if(minbrz.Q.size!=minThompson.Q.size) {
      dfaView(minbrz, "mbrz", true, Some(rte.toString), false)
      dfaView(minThompson, "mthmp", true, Some(rte.toString), false)
    }
    //val rte = rte4.canonicalize

    val rtea = Not(Or(Singleton(SAtomic(classOf[Trait2X])),Singleton(SSatisfies(oddp,"oddp")),
                      Singleton(SAtomic(classOf[Abstract1X]))))
    val dataa = Profiling.check(rtea,1,1)
    val minbrza = dataa("min_brzozowski")
    val minThompsona = dataa("min_thompson")
    if(minbrza.Q.size!=minThompsona.Q.size) {
     dfaView(minbrza,"mbrz", true, Some(rtea.toString), false)
      dfaView(minThompsona, "mthmp", true, Some(rtea.toString), false)
    }
    val rteb = rte2
    val datab = Profiling.check(rteb,1,1)
    val minbrzb = datab("min_brzozowski")
    val minThompsonb = datab("min_thompson")
    if(minbrzb.Q.size!=minThompsonb.Q.size) {
      dfaView(minbrzb, "mbrz", true, Some(rteb.toString), false)
      dfaView(minThompsonb, "mthmp", true, Some(rteb.toString), false)
    }
    val myarray = Array.fill(2)(Array.fill(3)(0))
    val depth = 7
    val num_random_tests = 10000
    for {depth <- 5 until depth
         r <- 0 until num_random_tests
         pattern = Rte.randomRte(depth)
         } {
      val data = Profiling.check(pattern, r, depth)
      val dfabrz = data("dfa_brzozowski")
      val minbrz = data("min_brzozowski")
      val dfathompson = data("dfa_thompson")
      val minThompson = data("min_thompson")
      if(r%100==0){println(r)}
      if(minbrz.Q.size>minThompson.Q.size)
      {
        println(pattern)
      }




      if (minThompson.protoDelta.isEmpty) {
        if (dfabrz.protoDelta.size * dfabrz.Q.size > dfathompson.protoDelta.size * dfathompson.Q.size) {
          myarray(0)(0) += 1
        }
        else if (dfabrz.protoDelta.size * dfabrz.Q.size < dfathompson.protoDelta.size * dfathompson.Q.size) {
          myarray(0)(2) += 1
        }
        else {
          myarray(0)(1) += 1
        }
        if (minbrz.protoDelta.size * minbrz.Q.size > minThompson.protoDelta.size * minThompson.Q.size) {
          myarray(1)(0) += 1
        }
        else if (minbrz.Q.size * minbrz.protoDelta.size < minThompson.protoDelta.size * minThompson.Q.size) {
          myarray(1)(2) += 1
        }
        else {
          myarray(1)(1) += 1
        }
        for (j <- Range(0, 2)) {
          for (k <- Range(0, 3)) {
            println(myarray(j)(k))
          }
        }
        }
      }*/
    // mystats.statisticSizePerRndDFASize(4,5,10,5,mystats.brz)
  }
}