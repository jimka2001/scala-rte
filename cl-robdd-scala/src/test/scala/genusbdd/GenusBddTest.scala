// Copyright (c) 2020 EPITA Research and Development Laboratory
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

package genusbdd

import bdd._
import genus._
import genus.Types._


import scala.collection.{Seq, mutable}
import adjuvant._
import RandomType.randomType

class GenusBddTest extends MyFunSuite {
  trait Trait1

  trait Trait2

  trait Trait3 extends Trait2

  abstract class Abstract1

  abstract class Abstract2

  test("subclass") {
    SAtomic.withOpenWorldView {
      val tdToInt: mutable.Map[SimpleTypeD, Int] = mutable.Map[SimpleTypeD, Int]()

      Bdd.withNewBddHash {
        val bdd = GenusBdd(SAnd(SAtomic(classOf[Trait2]),
                                SAtomic(classOf[Trait3])), tdToInt)
        val dnf = bdd.dnf
        assert(dnf == SAtomic(classOf[Trait3]))
      }
      Bdd.withNewBddHash {
        val dnf = GenusBdd(SOr(classOf[Trait2], classOf[Trait3]), tdToInt).dnf
        assert(dnf == SAtomic(classOf[Trait2]))
      }
    }
  }
  test("disjoint") {
    val tdToInt: mutable.Map[SimpleTypeD, Int] = mutable.Map[SimpleTypeD, Int]()
    Bdd.withNewBddHash {
      val dnf = GenusBdd(SAnd(classOf[Abstract1], classOf[Abstract2]), tdToInt).dnf
      assert(dnf == SEmpty)
    }
  }
  test("disjoint and subtype") {
    val tdToInt: mutable.Map[SimpleTypeD, Int] = mutable.Map[SimpleTypeD, Int]()
    SAtomic.withOpenWorldView {
      Bdd.withNewBddHash {
        val dnf = GenusBdd(SAnd(classOf[Abstract1], SOr(SAnd(classOf[Abstract1], classOf[Trait2]),
                                                        SAnd(classOf[Abstract2], classOf[Trait2]))), tdToInt).dnf
        assert(dnf == SAnd(classOf[Trait2], classOf[Abstract1]))
      }
    }
  }

  def testDnf(td: SimpleTypeD, tdToInt: mutable.Map[SimpleTypeD, Int]): Unit = {
    val bdd = GenusBdd(td, tdToInt)
    val dnf = bdd.dnf
    if (dnf == td)
      ()
    else {
      assert((dnf - td) == SEmpty,
             s"dnf=$dnf  td=$td   dnf-td=${dnf - td} expecting SEmpty")
      assert((td - dnf) == SEmpty,
             s"dnf=$dnf  td=$td   dnf-td=${td - dnf} expecting SEmpty")
    }
  }

  test("randomizer") {

    for {depth <- 2 to 7} {
      val tdToInt: mutable.Map[SimpleTypeD, Int] = mutable.Map[SimpleTypeD, Int]()
      Bdd.withNewBddHash {
        for {_ <- 0 to 750 - 100 * depth
             td = randomType(depth = depth)
             }
          testDnf(td, tdToInt)
      }
    }
  }

  test("test 1") {
    Bdd.withNewBddHash {

      val tds = Seq(SEmpty,
                    STop,
                    SAtomic(classOf[Trait1]),
                    SNot(classOf[Trait1]),
                    SEql(42),
                    SMember(1, 2, 3),
                    SMember(1, 3, 2),
                    evenType, // SSatisfies(evenp)
                    SAnd(classOf[Trait1], classOf[Trait2]),
                    SAnd(classOf[Trait1], classOf[Trait2], classOf[Trait3]),
                    SOr(classOf[Trait1], classOf[Trait2]),
                    SOr(classOf[Trait1], classOf[Trait2], classOf[Trait3]))
      val tdToInt: mutable.Map[SimpleTypeD, Int] = mutable.Map[SimpleTypeD, Int]()
      tds.foreach { td => GenusBdd(td, tdToInt).bdd }
      tds.foreach { td =>
        GenusBdd(td, tdToInt).dnf.canonicalize()
      }
    }
  }

  test("typep") {
    import scala.collection.mutable
    import GraphViz._
    def pos(x: Any): Boolean = {
      x match {
        case x: Double => x > 0.0
        case _ => false
      }
    }

    val td1 = SOr(classOf[Integer],
                  classOf[String],
                  SAnd(classOf[java.lang.Double],
                       SNot(SSatisfies(pos, "pos"))))
    val goods = Seq(1, "hello", -1.0)
    val bads = Seq(1.0, true)
    val tdToInt: mutable.Map[SimpleTypeD, Int] = mutable.Map[SimpleTypeD, Int]()
    Bdd.withNewBddHash {
      val gb = GenusBdd(td1, tdToInt)
      //gb.bdd.bddView( drawFalseLeaf=true,title="td1",labelToString=gb.labelToString)
      for {good <- goods} {
        val t = good.getClass.getName
        assert(td1.typep(good), s",{141} $good (type=$t) not of type $td1")
        assert(gb.typep(good), s",{142} $good (type=$t) not of type $td1")
      }
      for {bad <- bads} {
        val t = bad.getClass.getName
        assert(!td1.typep(bad), s",{146} $bad (type=$t) in type $td1")
        assert(!gb.typep(bad), s",{147} $bad (type=$t) not of type $td1")
      }
    }
  }
}
