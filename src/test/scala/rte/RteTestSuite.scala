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

import RteImplicits._
import adjuvant.Accumulators.withOutputToString
import genus._
import adjuvant.AdjFunSuite
import RandomType.randomType
import genus.Types.mysteryType
import xymbolyco.Dfa
import xymbolyco.Extract.{dfaToRte, extractRte}
import xymbolyco.GraphViz.dfaView

class RteTestSuite extends AdjFunSuite {
  val SInt = SSatisfies(Types.intp, "Int")
  val SDouble = SSatisfies(Types.doublep, "Double")
  test("implicits test") {

    assert(Not(SAtomic(classOf[Integer])) == Not(classOf[Integer]))
    assert(Not(SAtomic(classOf[Long])) != Not(classOf[Integer]))

    assert(And(SAtomic(classOf[Integer])) == And(classOf[Integer]))
    assert(And(SAtomic(classOf[Long])) != And(classOf[Integer]))

    assert(Or(SAtomic(classOf[Integer])) == Or(classOf[Integer]))
    assert(Or(SAtomic(classOf[Long])) != Or(classOf[Integer]))

    assert(Cat(SAtomic(classOf[Integer])) == Cat(classOf[Integer]))
    assert(Cat(SAtomic(classOf[Long])) != Cat(classOf[Integer]))

    assert(Star(SAtomic(classOf[Integer])) == Star(classOf[Integer]))
    assert(Star(SAtomic(classOf[Long])) != Star(classOf[Integer]))
  }

  test("Number is not empty") {
    assert(And(Singleton(SAtomic(classOf[Number]))) != And(Singleton(SEmpty)))
    assert(And(classOf[Number]) != And(Singleton(SEmpty)))
  }

  test("LaTeX 1") {
    println(And(classOf[Number]))
    assert(And(classOf[Number]).toLaTeX()
             == "(Number)")
    assert(And(classOf[Number],
               Not(SAtomic(classOf[Int])),
               Not(SEql(44))).toLaTeX()
             == "(Number\\wedge \\overline{Integer}\\wedge \\overline{44})")
  }

  test("LaTeX") {
    Or(And(SAtomic(classOf[Integer]),
           Not(SAtomic(classOf[Long]))),
       Not(SEql(43))).toLaTeX()

    Or(And(classOf[Integer],
           Not(SAtomic(classOf[Long]))),
       Not(SEql(44))).toLaTeX()

    assert(And(classOf[Number],
               Not(SAtomic(classOf[Int])),
               Not(SEql(44))).toLaTeX()
             == "(Number\\wedge \\overline{Integer}\\wedge \\overline{44})")
    assert(Or(Singleton(SAtomic(classOf[String])),
              Singleton(SMember(1, 2, 3))).toLaTeX()
             == "(String\\vee \\{1 ,2 ,3\\})")
  }

  test("operators") {
    for {depth <- 1 to 5
         _ <- 1 to num_random_tests
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)} {
      assert((r1 | r2) == Or(r1, r2))
      assert((r1 & r2) == And(r1, r2))
      assert(r1 ++ r2 == Cat(r1, r2)) // check that reversing the arguments works correctly
      assert(!r1 == Not(r1))
      assert(r1.? == Or(r1, EmptyWord))
      assert(r1.* == Star(r1))
      assert(r1.+ == Cat(r1, Star(r1)))
      assert((r1 ^ 0) == EmptyWord)
      assert((r1 ^ 1) == r1)
      assert((r1 ^ 2).canonicalize ~= Cat(r1, r1).canonicalize,
             s"\nr1= $r1" +
               s"\n  (r1 ^ 2).canonicalize= " + (r1 ^ 2).canonicalize +
               s"\n  Cat(r1, r1).canonicalize= " + Cat(r1, r1).canonicalize)
      assert((r1 ^ 3).canonicalize ~= Cat(r1, r1, r1).canonicalize)
    }
  }

  test("canonicalize random") {
    for {depth <- 0 to 5
         _ <- 1 to num_random_tests
         r1 = Rte.randomRte(depth = depth)
         } {
      r1.canonicalize
    }
    //println(adjuvant.Adjuvant.simplifierUsed.toList.sortBy{
    //  case ((x,y),i) => (x,i)
    //}.map(println))
  }

  test("canonicalize") {

    assert(EmptySet.canonicalize == EmptySet)
    assert(Sigma.canonicalize == Sigma)
    assert(EmptyWord.canonicalize == EmptyWord)
  }
  test("discovered 102") {
    val t1 = SEql(0)
    val t2 = SEql(0L)
    assert(Singleton(SAnd(t1, t2)).canonicalize == And(Singleton(t1), Singleton(t2)).canonicalize)
    assert(Singleton(SOr(t1, t2)).canonicalize == Singleton(SMember(0, 0L)))
    println(Or(Singleton(t1), Singleton(t2)).canonicalize)
    assert(Singleton(SOr(t1, t2)).canonicalize == Or(Singleton(t1), Singleton(t2)).canonicalize)
    val s = Singleton(SNot(t1))
    if (s.flattenTypes)
      assert(s.canonicalize == And(Not(t1), Sigma).canonicalize)
  }
  test("canonicalize Singleton") {
    for {
      _ <- 1 to num_random_tests
      t1 = randomType(0)
      t2 = randomType(0)
    } {
      val s1 = Singleton(SAnd(t1, t2))
      if (s1.flattenTypes)
        assert(s1.canonicalize == And(Singleton(t1), Singleton(t2)).canonicalize)
      val s2 = Singleton(SOr(t1, t2))
      if (s2.flattenTypes)
        assert(s2.canonicalize == Or(Singleton(t1), Singleton(t2)).canonicalize)
      val s3 = Singleton(SNot(t1))
      if (s3.flattenTypes)
        assert(s3.canonicalize == And(Not(t1), Sigma).canonicalize)
    }
  }
  test("discovered case 108") {
    //r1 = Or(Or(Cat(Σ,Σ,(Σ)*),ε),Or(<Abstract1$1>,Cat(<Abstract2$1>,<{4,5,6}>)))
    //r2 = Or(Or(Cat(<[= 0]>,<{1,2,3,4}>),(<[= -1]>)*),Not(Or(<[= 0]>,<{a,b,c}>)))
    trait Trait1
    trait Trait2
    trait Trait3 extends Trait2
    abstract class Abstract1
    abstract class Abstract2 extends Trait3
    val r1 = Or(Or(Cat(Sigma, Sigma, Star(Sigma)), EmptyWord),
                Or(Atomic(classOf[Abstract1]),
                   Cat(Atomic(classOf[Abstract2]),
                       Member(4, 5, 6))))
    val r2 = Or(Or(Cat(Eql(0), Member(1, 2, 3, 4)), Star(Eql(-1))),
                Not(Or(Eql(0), Member("a", "b", "c"))))

    val rt1 = Not(And(r1, r2)).canonicalize
    val rt2 = Or(Not(r1), Not(r2))

    import xymbolyco.GraphViz.dfaToPng
    withOutputToString { printer =>
      xymbolyco.Serialize.serialize(rt1.toDfa(), printer)
    }
    dfaToPng(rt1.toDfa(), title = "debug", abbrev = true)
    dfaToPng(Not(rt1).toDfa(), title = "not rt1", abbrev = true)
    dfaToPng(xymbolyco.Minimize.minimize(rt1.toDfa()), title = "minimized", abbrev = true)
    val cano: Rte = Or(And(rt1, Not(rt2)),
                       And(rt2, Not(rt1))).canonicalize
    dfaToPng(cano.toDfa(), title = "cano", abbrev = true)
  }
  test("discovered case 109 infinite loop") {
    // r1 = Or(Or(Or(<java.lang.String>,<[= 0]>),Or(<Abstract2$1>,<Trait2$1>)),Or((<java.lang.Integer>)*,Or(<Trait3$1>,<java.lang.Number>)))
    // r2 = Cat(Or(Or(<[Member 1,2,3,4]>,<SEmpty>),Not(<java.lang.Number>)),Not(And(<[Member 4,5,6]>,<Abstract2$1>)))
    trait Trait1
    trait Trait2
    trait Trait3 extends Trait2

    abstract class Abstract1
    abstract class Abstract2 extends Trait3
    val string = Atomic(classOf[java.lang.String])
    val integer = Singleton(SAtomic(classOf[java.lang.Integer]))
    val number = Singleton(SAtomic(classOf[java.lang.Number]))
    val r1 = Or(string,
                Eql(0),
                //Rte.Atomic(classOf[Abstract2]),
                Atomic(classOf[Trait2]),
                Star(integer),
                Atomic(classOf[Trait3]),
                number
                )
    val r2 = Cat(Or(Eql(1),
                    Not(number)),
                 Rte.sigmaStar
                 )

    val rt1 = Not(And(r1, r2)).canonicalize
    val rt2 = Or(Not(r1), Not(r2))

    import xymbolyco.GraphViz.dfaToPng

    withOutputToString { printer =>
      xymbolyco.Serialize.serialize(rt1.toDfa(), printer)
    }
    dfaToPng(rt1.toDfa(), title = "debug", abbrev = true)
    dfaToPng(Not(rt1).toDfa(), title = "not rt1", abbrev = true)
    Or(And(rt1, Not(rt2)),
       And(rt2, Not(rt1))).toDfa()
    //Not(And(r1, r2)).canonicalize ~= Or(Not(r1), Not(r2)).canonicalize


  }
  test("canonicalize not de morgan") {
    assert(Not(Sigma).canonicalize == Or(Cat(Sigma, Sigma, Star(Sigma)),
                                         EmptyWord))
    assert(Not(Star(Sigma)).canonicalize == EmptySet)
    assert(Not(EmptyWord).canonicalize == Cat(Sigma, Star(Sigma)))
    assert(Not(EmptySet).canonicalize == Star(Sigma))
    for {depth <- 0 to 2
         _ <- 1 to num_random_tests / 2
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         } {

      assert(Not(Not(r1)).canonicalize ~= r1.canonicalize)
      if (!(Not(And(r1, r2)).canonicalize ~= Or(Not(r1), Not(r2)).canonicalize)) {
        val a = Not(And(r1, r2)).canonicalize
        val b = Or(Not(r1), Not(r2)).canonicalize
        xymbolyco.GraphViz.dfaToPng(Or(And(a, Not(b)),
                                       And(b, Not(a))).toDfa(), "de-morgan", abbrev = true)
        assert(Not(And(r1, r2)).canonicalize ~= Or(Not(r1), Not(r2)).canonicalize,
               s"\nr1=$r1  \nr2=$r2" +
                 s"\n  Not(And(r1, r2)) = Not(And($r1, $r2)) = " + Not(And(r1, r2)).canonicalize +
                 s"\n  Or(Not(r1), Not(r2)) = Or(Not($r1), Not($r2)) = " + Or(Not(r1), Not(r2)).canonicalize)
      }
      assert(Not(Or(r1, r2)).canonicalize ~= And(Not(r1), Not(r2)).canonicalize)
      assert(Not(r1).canonicalize ~= Not(r1.canonicalize).canonicalize)
    }
  }

  test("de morgan 245") {
    // Not.apply(And.apply(r1, r2)(scala.this.DummyImplicit.dummyImplicit)).canonicalize.
    //     ~=(Or.apply(Not.apply(r1), Not.apply(r2))(scala.this.DummyImplicit.dummyImplicit).canonicalize)
    //     was false
    // r1=Or(<[Member a,b,c]>,<Trait3$1>)
    // r2=<Trait1$1>
    // Not(And(r1, r2)) = Not(And(Or(<[Member a,b,c]>,<Trait3$1>), <Trait1$1>)) = Or(Not(<Trait3$1>),Not(<Trait1$1>))
    //   Or(Not(r1), Not(r2)) = Or(Not(Or(<[Member a,b,c]>,<Trait3$1>)), Not(<Trait1$1>)) = Or(And(Not(<[Member a,b,c]>),Not(<Trait3$1>)),Not(<Trait1$1>))
    trait Trait1
    trait Trait2
    val r1 = Or(Singleton(SMember(1, 2, 3)), Singleton(SAtomic(classOf[Trait1])))
    val r2 = Singleton(SAtomic(classOf[Trait2]))
    val a = Not(And(r1, r2)).canonicalize
    val b = Or(Not(r1), Not(r2)).canonicalize

    assert(a ~= b)
  }

  test("de morgan 272") {

    val r1 = Cat(Singleton(SAtomic(classOf[java.lang.Number])), Singleton(SEql(0)))
    val r2 = Sigma
    val a = Not(And(r1, r2)).canonicalize
    val b = Or(Not(r1), Not(r2)).canonicalize

    assert(And(a, Not(b)).canonicalize ~= EmptySet)

    assert(a ~= b)
  }

  test("rteCase") {
    assert(classOf[String].isInstance("hello"))
    assert(Types.intp(1))
    assert(Types.doublep(1.1))
    assert(SInt.typep(1))
    assert(!SInt.typep(1.0))
    assert(!SDouble.typep(1))
    assert(SDouble.typep(1.0))
    val arbitrate = Rte.rteCase(Seq(Star(classOf[String]) -> 1,
                                    Cat(Star(classOf[String]), Singleton(SEql(1))) -> 2,
                                    Cat(Star(classOf[String]), Singleton(SEql(-1))) -> 3,
                                    Cat(Star(classOf[String]), Singleton(SEql(0))) -> 4,
                                    Cat(Star(classOf[String]), Singleton(SInt)) -> 5
                                    ))
    assert(arbitrate(Seq("hello")).contains(1))
    assert(arbitrate(List()).contains(1))
    assert(arbitrate(List("hello", "world", 1)).contains(2))
    assert(arbitrate(List("hello", "world", -1)).contains(3))
    assert(arbitrate(List("hello", "world", 0)).contains(4))
    assert(arbitrate(List("hello", "world", 12)).contains(5))
    assert(arbitrate(List("hello", "world", 12.0)).isEmpty)
  }
  test("rteIfThenElse") {
    import rte.RteImplicits._
    import scala.language.implicitConversions
    import rte.Rte.rteIfThenElse
    val int = classOf[Int]
    val str = classOf[String]

    var c1 = 0
    var c2 = 0
    var c3 = 0
    var cdefault = 0

    val f: Seq[Any] => Int = rteIfThenElse(
      Seq(
        Star(int) -> (() => {
          c1 += 1
          1
        }),
        Star(str) -> (() => {
          c2 += 1
          2
        }),
        Cat(Star(int), Star(str)) -> (() => {
          c3 += 1
          3
        })),
      () => {
        cdefault += 1
        4
      })
    assert(f(List(1, 2, 3, 4, 5)) == 1)
    assert(c1 == 1)
    assert(f(List(1, 2, 3, 4, 5)) == 1)
    assert(c1 == 2)

    assert(f(List("one", "two", "three", "four")) == 2)
    assert(c2 == 1)
    assert(f(List(1, 2, 3, 4, 5)) == 1)
    assert(c1 == 3)
    assert(c2 == 1)

    assert(f(List("one", "two", 3, 4)) == 4)
    assert(cdefault == 1)
    assert(c1 == 3)
    assert(c2 == 1)

    assert(f(List(1, 2, "three", "four")) == 3)
    assert(cdefault == 1)
    assert(c1 == 3)
    assert(f(List("one", "two", "three", "four")) == 2)
    assert(c2 == 2)
    assert(c3 == 1)
  }

  test("search") {
    val rte: Rte = Star(And(Cat(Or(Not(Singleton(SEql(1))),
                                   Singleton(SEql(2)))),
                            Or(Singleton(SEql(3)), Singleton(SEql(3)))))
    rte.search(Rte.isSingleton) match {
      case Some(Singleton(SEql((_, x)))) => assert(x == 1)
      case _ => fail()
    }
  }

  test("isomorphic") {
    import scala.language.implicitConversions
    import rte.RteImplicits._

    val r1:Rte = Star(Cat(classOf[Int],
                          Star(classOf[String]),
                          mysteryType()))
    val r2:Rte = Not(mysteryType())
    val dfa:Dfa[Any, SimpleTypeD, Boolean] = Xor(r1,r2).toDfa(true)
    // println(dfa.spanningPath)
    // println(dfa.spanningTrace)
    // dfaView(dfa, title="xor r1 r2")
    assert(r1.isomorphic(r2) == Some(false)) // every spanning path contains mysteryType in its transition label
    assert((Star(Cat(classOf[Int], Star(classOf[String]), mysteryType())) ~= Not(mysteryType()))
             == false)
  }

  test("spanning paths") {
    import xymbolyco.Dfa.dfaUnion
    var lostValues: Set[Int] = Set()
    val depth=3
    def arbitrate(a: Int, b: Int): Int = {
      lostValues = lostValues + b
      a
    }
    for {n <- 0 to 10
         rte1 = Rte.randomRte(depth)
         rte2 = Rte.randomRte(depth)
         rte3 = Rte.randomRte(depth)
         dfa1 = Cat(rte1, Star(rte2), Not(rte2), rte3).toDfa(1)
         dfa2 = Cat(rte2, Star(rte3), Not(rte3), rte1).toDfa(2)
         dfau = dfaUnion(dfa1, dfa2, arbitrate = arbitrate)
         } {
      println("======================")
      println(f"depth=$depth n=$n")
      println(f"rte1=$rte1")
      println(f"rte2=$rte2")
      println(f"rte3=$rte3")
      //println(f"dfa1=" + dfaToRte(dfa1,1))
      //println(f"dfa2=" + dfaToRte(dfa2,2))
      //println(f"dfa4= states=${dfau.Qids.size}  transitions=${dfau.protoDelta.size}")

      dfau.findSpanningPathMap()
    }
    println(f"lostValues = $lostValues")
  }
}
