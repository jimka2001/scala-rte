// Copyright (c) 2021 EPITA Research and Development Laboratory
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


package rte

import genus._
import org.scalatest.funsuite.AnyFunSuite
import RandomType.randomType
import adjuvant.MyFunSuite
import genus.Types._
import xymbolyco.GraphViz.dfaView
import xymbolyco.Profiling

class DerivativeTestSuite extends MyFunSuite {
  test("nullable") {
    assert(!Sigma.nullable)
    assert(!EmptySet.nullable)
    assert(Star(Sigma).nullable)
    assert(Star(EmptySet).nullable)
    for {depth <- 1 to 5
         _ <- 0 to 100
         r = Rte.randomRte(depth)} {
      assert(Star(r).nullable)
      assert(!Not(Star(r)).nullable)
    }
    for {depth <- 1 to 5
         _ <- 1 to 1000
         r = Rte.randomRte(depth)}
      r.nullable
  }

  test("canonicalize nullable"){
    for {depth <- 0 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         } {
      assert(r1.nullable == r1.canonicalize.nullable,
             s"\nr1=$r1 is"
               + (if (r1.nullable) "" else " not")
               + " nullable while r1.canonicalize="
               + r1.canonicalize
               + " is"
               + (if (r1.canonicalize.nullable) "" else " not")
               + " nullable")
    }
  }

  test("firstTypes") {
    for {depth <- 1 to 5
         _ <- 1 to 1000
         r = Rte.randomRte(depth)}
      r.firstTypes
  }

  test("derivative 595") {
    val r1 = Cat(Singleton(SAtomic(classOf[java.lang.Number])), Singleton(SEql(0)))
    val r2 = Sigma
    val a = Not(And(r1, r2)).canonicalize
    val b = Or(Not(r1), Not(r2)).canonicalize

    val aNotB = And(a,Not(b))

    assert(aNotB.derivative1(Some(SAtomic(classOf[java.lang.Number]))) != Singleton(SEql(0)))

  }

  test("derivative special cases") {
    //import genus.STop
    assert(EmptySet.derivative1(Some(STop)) == EmptySet)
    assert(Sigma.derivative1(Some(STop)) == EmptyWord)
    assert(EmptyWord.derivative1(Some(STop)) == EmptySet)
    assert(Singleton(STop).derivative1(Some(STop)) == EmptyWord)
    assert(Singleton(SEmpty).derivative1(Some(STop)) == EmptySet)

    assert(EmptySet.derivative1(Some(SEmpty)) == EmptySet)
    assert(Sigma.derivative1(Some(SEmpty)) == EmptySet)
    assert(EmptyWord.derivative1(Some(SEmpty)) == EmptySet)
    assert(Singleton(STop).derivative1(Some(SEmpty)) == EmptySet)
    assert(Singleton(SEmpty).derivative1(Some(SEmpty)) == EmptySet)

    // deriv wrt EmptyWord
    assert(EmptySet.derivative1(None) == EmptySet)
    assert(Sigma.derivative1(None) == Sigma)
    assert(EmptyWord.derivative1(None) == EmptyWord)
    assert(Singleton(STop).derivative1(None) == Sigma)
    assert(Singleton(SEmpty).derivative1(None) == EmptySet)
  }

  test("random derivative") {
    for {depth <- 0 to 5
         _ <- 1 to 1000
         td = randomType(depth)
         //rt = Singleton(td)
         } {
      td.inhabited match {
        case Some(true) =>
          assert (Sigma.derivative1(Some(td) ) == EmptyWord)
        case Some(false) =>
          assert (Sigma.derivative1(Some(td) ) == EmptySet)
        case None => ()
      }
    }
  }
  test("random derivative 2") {
    for {depth <- 0 to 5
         _ <- 1 to 1000
         td = randomType(depth)
         } {
      assert(EmptySet.derivative1(Some(td)) == EmptySet)
      assert(EmptyWord.derivative1(Some(td)) == EmptySet)
    }
  }
  test("random derivative 3") {
    for {depth <- 0 to 5
         _ <- 1 to 1000
         td = randomType(depth)
         rt = Singleton(td)
         } {
      if (td.inhabited.contains(true))
        assert(rt.derivative1(Some(STop)) == EmptyWord,
               s"failed deriv of $rt wrt Some(STop)")
      else if (td.inhabited.contains(false))
        assert(rt.derivative1(Some(STop)) == EmptySet,
               s"failed inhabited=${td.inhabited} deriv of $rt wrt Some(STop)")
    }
  }
  test("random derivative 4") {
    for {depth <- 0 to 5
         _ <- 1 to 1000
         td = randomType(depth)
         rt = Singleton(td)
         } {
      assert(rt.derivative1(Some(SEmpty)) == EmptySet)
    }
  }
  test("random derivative 5") {
    for {depth <- 0 to 5
         _ <- 1 to 1000
         td = randomType(depth)
         rt = Singleton(td)
         } {
      val d = rt.derivative1(None)
      val c = rt.canonicalize
      val dc = d.canonicalize
      assert(dc ~= c,
             s": deriv of $rt wrt EmptyWord/None returned $d" +
               s"\n   expecting $rt which reduces to $c")
    }
  }

  test("derivative random") {
    for {depth <- 0 to 5
         _ <- 1 to 1000
         rt = Rte.randomRte(depth)
         can = rt.canonicalize
         m = mdtd(can.firstTypes)
         (td,(factors,disjoints)) <- m
         } {
      can.derivative(Some(td),factors.toList,disjoints.toList)
    }
  }

  test("derivative factors"){
    //val rt = Cat(Star(Singleton(SEql(1))),And(Singleton(SMember(1,2)),Singleton(SMember(2,3))))
    val z = Singleton(SEql(0))
    val f = Singleton(SEql(false))
    val i = Singleton(SAtomic(classOf[Integer]))
    val s = Singleton(SAtomic(classOf[String]))
    val rt = And(Or(Star(And(Sigma,Not(i))),
                    EmptyWord),
                 Star(Star(Or(Or(z,f),Star(s)))))
    val ft= rt.firstTypes
    //println(s"rt=$rt")
    //println(s"first types = $ft")
    mdtd(ft).map {
      case (td,(factors,disjoints)) =>
        //println(s"td=$td")
        //println(s"  factors   = $factors")
        //println(s"  disjoints = $disjoints")
        val deriv = rt.derivative(Some(td),factors.toList,disjoints.toList)
        //println("  rt.derivative(Some(td)) = " + deriv)
        //println("  --> " + deriv.canonicalize)
        deriv
    }

//    val (v1,v2) = rt.derivatives()
//    println("derivatives")
//    println(" v1")
//    for{ i <- v1.indices } println(s"    v1[$i] = " + v1(i))
//    println(" v2")
//    for{ i <- v2.indices } println(s"    v2[$i] = " + v2(i))
  }

  test("derivatives random") {

    for {depth <- 0 to 4
         _ <- 1 to 2000
         rt = Rte.randomRte(depth)
         (intToV,_) = rt.derivatives()
         } {
      assert(intToV.nonEmpty)
    }
  }

  test("rte deriv case 1"){
    // OK
    Star(Cat(And(Or(Not(Singleton(SMember(1,2,3,4))),
                    Star(Singleton(SEmpty)))
                 ),
             Singleton(SMember("a","b","c","d")))).toDfa()

    // OK
    Star(Cat(And(Singleton(SMember(1)),
                 Sigma),
             Singleton(SMember("a")))).toDfa()

    // OK
    Cat(And(Not(Singleton(SMember(1))),
            Sigma),
        Singleton(SMember("a"))).toDfa()

    // OK
    Star(Cat(And(Not(Singleton(SMember(1))),
                 Sigma),
             Singleton(SMember(1)))).toDfa()

    // was Error
    Star(Cat(Not(Singleton(SEql(1))),
             Singleton(SEql("a")))).toDfa()

    // was Error
    Star(Cat(And(Not(Singleton(SMember(1))),
                 Sigma),
             Singleton(SMember("a")))).toDfa()


    // was Error
    Star(Cat(And(Or(Not(Singleton(SMember(1)))
                    ),
                 Sigma),
             Singleton(SMember("a")))).toDfa()

    // was Error
    Star(Cat(And(Or(Not(Singleton(SMember(1))),
                    Star(Singleton(SEmpty))),
                 Sigma),
             Singleton(SMember("a")))).toDfa()


    //  when generating dfa from (Cat(And(Or(Not(<[Member 1,2,3,4]>),(<SEmpty>)*),Σ),<[Member a,b,c]>))*
    //    canonicalized to (Cat(Not(<[Member 1,2,3,4]>),<[Member a,b,c]>))*
    //       computing derivative of <[Member a,b,c]>
    //          wrt=[Not [Member 1,2,3,4]]
    Star(Cat(And(Or(Not(Singleton(SMember(1,2,3,4))),
                    Star(Singleton(SEmpty))),
                 Sigma),
             Singleton(SMember("a","b","c","d")))).toDfa()
  }

//  test("Number 4,5,6") {
//    val num = SAtomic(classOf[Number])
//    val rte = Star(Or(Singleton(num), Singleton(SMember(4, 5, 6))))
//    val firsts = rte.firstTypes
//    val mdtd = Types.mdtd(firsts)
//    println(mdtd)
//    println("subtype = " + SMember(4, 5, 6).subtypep(num))
//
//    for {(wrt, (factors, disjoints)) <- mdtd} {
//      println(s" wrt=$wrt --> " + rte.derivative(Some(wrt), factors.toList, disjoints.toList))
//      println(s"   canonicalized --> " + rte.derivative(Some(wrt), factors.toList, disjoints.toList).canonicalize)
//    }
//  }
//  test("Number*") {
//    val num = SAtomic(classOf[Number])
//    val rte = Star(Singleton(num))
//    val firsts = rte.firstTypes
//    val mdtd = Types.mdtd(firsts)
//
//    println(s"inhabited = " + num.inhabited)
//
//    println(mdtd)
//
//    for {(wrt, (factors, disjoints)) <- mdtd} {
//      println(s" wrt=$wrt --> " + rte.derivative(Some(wrt), factors.toList, disjoints.toList))
//      println(s"   canonicalized --> " + rte.derivative(Some(wrt), factors.toList, disjoints.toList).canonicalize)
//    }
//
//    Profiling.check(rte,1,1)
//    dfaView(rte.toDfa(42),title="Number-star",true,Some("xyzzy"))
//  }
}
