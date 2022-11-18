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
import DFAIso._

class MyTestSuite extends AnyFunSuite {
  def foo(): Boolean = {
    val rte = Star(Or(Singleton(SSatisfies(evenp, "evenp")), Singleton(SAtomic(classOf[Abstract1X])), Singleton(SAtomic(classOf[Abstract2X]))))
    val data = Profiling.check(rte, 1, 1)
    val minbrz = data("min_brzozowski")
    val minThompson = data("min_thompson")
    if (minbrz.Q.size > minThompson.Q.size) {
      dfaView(minbrz, "mbrz", true, Some(rte.toString), false)
      dfaView(minThompson, "mthmp", true, Some(rte.toString), false)
    }
    minbrz.Q.size == minThompson.Q.size

  }

  /*
  test("e")
  {
    val rte = And(Star(Singleton(SEmpty)),Star(Sigma))
    val dfa = Thompson.constructThompsonDfa(rte, 42)
    val rte2 = Or(Star(Singleton(SEmpty)),Star(!EmptyWord),Sigma,Or(EmptyWord,Cat(Cat(Singleton(SAtomic:ADT1),Singleton(SEmpty)),!Singleton(SAtomic:Number),!Singleton(SAtomic:ADT1)),And(Singleton(SOr(SAtomic:ADT1,SAtomic:ADT2,SAtomic:ADT3)),Or(Cat(Sigma,Sigma,Star(Sigma)),EmptyWord)),!Cat(Singleton(SEql(-1 /* Integer*/ )),Singleton(SMember(1 /* Integer*/, 2 /* Integer*/, 3 /* Integer*/, 4 /* Integer*/)),Singleton(SEmpty))))
    val dfa2 = Thompson.constructThompsonDfa(rte2, 43)
    val union = Dfa.dfaUnion(dfa, dfa2)
    val dfa3 = rte2.toDfa(43)
    val dfa4 = rte.toDfa(42)
    val union2 = Dfa.dfaUnion(dfa4, dfa3)
    val data = isIsomorphic(union, union2)
    println(data._1)
  }
*/

  test("d") {


    val depth = 6
    val num_random_tests = 10000
    for {depth <- 0 until depth
         r <- 0 until num_random_tests
         pattern = Rte.randomRte(depth)
         } {
      val rte = Rte.randomRte(4)
      val dfa = Minimize.minimize(Minimize.trim(Thompson.constructThompsonDfa(rte, 42)))
      val rte2 = Rte.randomRte(4)
      val dfa2 = Minimize.minimize(Minimize.trim(Thompson.constructThompsonDfa(rte2, 43)))
      val union = Dfa.dfaUnion(dfa, dfa2)
      val dfa3 = rte.toDfa(42)
      val dfa4 = rte2.toDfa(43)
      val union2 = Dfa.dfaUnion(dfa3, dfa4)
      println("____________________________________________________________________________________________")
      println(rte)
      println(rte2)
      val data = isIsomorphic(union, union2)

      if (data._1.contains(false)) {
        dfaView(union2, "union2", true, Some(""), false)
        dfaView(union, "union", true, Some(""), false)
        dfaView(Dfa.dfaXor(union2, union), "xor", true, Some(""), false)
        dfaView(dfa, "dfa", true, Some(""), false)

        dfaView(dfa2, "dfa2", true, Some(""), false)

        dfaView(dfa3, "dfa3", true, Some(""), false)

        dfaView(dfa4, "dfa4", true, Some(""), false)
        println(data._2)
        assert(false)
      }
    }

    /*
  var i =0

  while (foo()) {
    i += 1
    println(i)
  }
*/
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
    }*/
    /*
        val myarray = Array.fill(2)(Array.fill(3)(0))
        val depth = 7
        val num_random_tests = 10000
        for {depth <- 0   until depth
             r <- 0 until num_random_tests
             pattern = Rte.randomRte(depth)
             } {
          val data = Profiling.check(pattern, r, depth)
          val dfabrz = data("dfa_brzozowski")
          val minbrz = data("min_brzozowski")
          val dfathompson = data("dfa_thompson")
          val minThompson = data("min_thompson")
          if (minbrz.Q.size > minThompson.Q.size) {
            println(pattern)
          }
        }

    */
    /*
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

// brzozowski divides boolean into (true, false) after the algorithms both translate the (true,false) from the rte, into boolean
// only difference is if there is another SEql/SMember in the rte inside of a Star(Or())

// Star(Or(Singleton(SSatisfies(evenp,"evenp")),Singleton(SAtomic(classOf[ADT_abstr])),Singleton(SAtomic(classOf[Class2X]))))


// second RTE that provokes the "undeterminism"


// Or(And(Star(Singleton(SSatisfies(oddp,"oddp"))), Cat(Singleton(SSatisfies(evenp,"evenp")),
//     Singleton(SMember("a", "b", "c")))),Star(Or(Singleton(SAtomic(classOf[Class1X])),
//       Singleton(SAtomic(classOf[Trait2X])))),Not(Star(Singleton(SAtomic(classOf[Trait3X])))))


//Cat(Star(Singleton(SSatisfies(evenp,"evenp"))),Or(Singleton(SAtomic(classOf[Trait2X])),
//                                                               Singleton(SAtomic(classOf[ADT1])),
//                                                               Singleton(SEql(false))),Singleton(SAtomic(classOf[Integer])))


// Or(And(Not(Singleton(SAtomic(classOf[String]))),
//                    And(Singleton(SAtomic(classOf[Class2X])),Singleton(SAtomic(classOf[ADT_abstr])))),
//                Star(Singleton(SSatisfies(oddp,"oddp"))))


//Cat(Star(Singleton(SOr(SAtomic(classOf[ADT1])))),
//                  Or(Singleton(SMember(false, true)),Singleton(SAtomic(classOf[Boolean])),
//                    Singleton(SMember("a", "b", "c")),Singleton(SMember(4, 5, 6))),
//                 Cat(Singleton(SAtomic(classOf[Trait3X])),Singleton(SEql(1))))