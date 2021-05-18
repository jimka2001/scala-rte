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

import org.scalatest.funsuite.AnyFunSuite
import RteImplicits._
import genus._

class RteTestSuite extends AnyFunSuite {

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

  test("LaTeX") {
    Or(And(SAtomic(classOf[Integer]),
           Not(SAtomic(classOf[Long]))),
       Not(SEql(43))).toLaTeX

    Or(And(classOf[Integer],
           Not(SAtomic(classOf[Long]))),
       Not(SEql(44))).toLaTeX
  }


  test("operators") {
    for {depth <- 1 to 5
         _ <- 1 to 1000
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
      assert((r1 ^ 2).canonicalize == Cat(r1, r1).canonicalize, s"r1=$r1")
      assert((r1 ^ 3).canonicalize == Cat(r1, r1, r1).canonicalize)
    }
  }

  test("canonicalize random") {
    for {depth <- 0 to 5
         _ <- 1 to 10000
         r1 = Rte.randomRte(depth = depth)
         } {
      r1.canonicalize
    }
  }

  test("canonicalize") {

    assert(EmptySet.canonicalize == EmptySet)
    assert(Sigma.canonicalize == Sigma)
    assert(EmptyWord.canonicalize == EmptyWord)
  }

  test("canonicalize Singleton") {
    import Types.randomType
    for {
      _ <- 1 to 1000
      t1 = randomType(0)
      t2 = randomType(0)
    } {
      assert(Singleton(SAnd(t1, t2)).canonicalize == And(Singleton(t1), Singleton(t2)).canonicalize)
      assert(Singleton(SOr(t1, t2)).canonicalize == Or(Singleton(t1), Singleton(t2)).canonicalize)
      assert(Singleton(SNot(t1)).canonicalize == And(Not(t1), Sigma).canonicalize)
    }
  }

  test("discovered case 109 infinite loop"){
    // r1 = Or(Or(Or(<java.lang.String>,<[= 0]>),Or(<Abstract2$1>,<Trait2$1>)),Or((<java.lang.Integer>)*,Or(<Trait3$1>,<java.lang.Number>)))
    // r2 = Cat(Or(Or(<[Member 1,2,3,4]>,<SEmpty>),Not(<java.lang.Number>)),Not(And(<[Member 4,5,6]>,<Abstract2$1>)))
    trait Trait1
    trait Trait2
    trait Trait3 extends Trait2
    abstract class Abstract1
    abstract class Abstract2 extends Trait3
    val string = Rte.Atomic(classOf[java.lang.String])
    val integer = Singleton(SAtomic(classOf[java.lang.Integer]))
    val number = Singleton(SAtomic(classOf[java.lang.Number]))
    val r1 = Or(string,
                Rte.Eql(0),
                Rte.Atomic(classOf[Abstract2]),
                Rte.Atomic(classOf[Trait2]),
                Star(integer),
                Rte.Atomic(classOf[Trait3]),
                number)
    val r2 = Cat(Or(Rte.Member(1,2,3,4),
                    Not(number)),
                 Not(EmptySet
                     )
                 )

      println(s"  r1 = $r1")
      println(s"  r2 = $r2")
      assert(Not(Not(r1)).canonicalize ~= r1.canonicalize)
      if (! (Not(And(r1, r2)).canonicalize ~= Or(Not(r1), Not(r2)).canonicalize)) {
        val a = Not(And(r1, r2)).canonicalize
        val b = Or(Not(r1), Not(r2)).canonicalize
        xymbolyco.GraphViz.dfaView(Or(And(a,Not(b)),
                                      And(b,Not(a))).toDfa(),"demorgan",true)
        assert(Not(And(r1, r2)).canonicalize ~= Or(Not(r1), Not(r2)).canonicalize,
               s"\nr1=$r1  \nr2=$r2" +
                 s"\n  Not(And(r1, r2)) = Not(And($r1, $r2)) = " + Not(And(r1, r2)).canonicalize +
                 s"\n  Or(Not(r1), Not(r2)) = Or(Not($r1), Not($r2)) = " + Or(Not(r1), Not(r2)).canonicalize)
      }
      assert(Not(Or(r1, r2)).canonicalize ~= And(Not(r1), Not(r2)).canonicalize)
      assert(Not(r1).canonicalize ~= Not(r1.canonicalize).canonicalize)

  }
  test("canonicalize not de morgan") {
    assert(Not(Sigma).canonicalize == Or(Cat(Sigma, Sigma, Star(Sigma)),
                                         EmptyWord))
    assert(Not(Star(Sigma)).canonicalize == EmptySet)
    assert(Not(EmptyWord).canonicalize == Cat(Sigma, Star(Sigma)))
    assert(Not(EmptySet).canonicalize == Star(Sigma))
    for {depth <- 0 to 5
         _ <- 1 to 1000
         r1 = Rte.randomRte(depth)
         r2 = Rte.randomRte(depth)
         } {
      assert(Not(Not(r1)).canonicalize ~= r1.canonicalize)
      if (! (Not(And(r1, r2)).canonicalize ~= Or(Not(r1), Not(r2)).canonicalize)) {
        val a = Not(And(r1, r2)).canonicalize
        val b = Or(Not(r1), Not(r2)).canonicalize
        xymbolyco.GraphViz.dfaView(Or(And(a,Not(b)),
                                      And(b,Not(a))).toDfa(),"demorgan",true)
        assert(Not(And(r1, r2)).canonicalize ~= Or(Not(r1), Not(r2)).canonicalize,
               s"\nr1=$r1  \nr2=$r2" +
                 s"\n  Not(And(r1, r2)) = Not(And($r1, $r2)) = " + Not(And(r1, r2)).canonicalize +
                 s"\n  Or(Not(r1), Not(r2)) = Or(Not($r1), Not($r2)) = " + Or(Not(r1), Not(r2)).canonicalize)
      }
      assert(Not(Or(r1, r2)).canonicalize ~= And(Not(r1), Not(r2)).canonicalize)
      assert(Not(r1).canonicalize == Not(r1.canonicalize).canonicalize)
    }
  }

  test("demorgan 245"){
    // Not.apply(And.apply(r1, r2)(scala.this.DummyImplicit.dummyImplicit)).canonicalize.
    //     ~=(Or.apply(Not.apply(r1), Not.apply(r2))(scala.this.DummyImplicit.dummyImplicit).canonicalize)
    //     was false
    // r1=Or(<[Member a,b,c]>,<Trait3$1>)
    // r2=<Trait1$1>
    // Not(And(r1, r2)) = Not(And(Or(<[Member a,b,c]>,<Trait3$1>), <Trait1$1>)) = Or(Not(<Trait3$1>),Not(<Trait1$1>))
    //   Or(Not(r1), Not(r2)) = Or(Not(Or(<[Member a,b,c]>,<Trait3$1>)), Not(<Trait1$1>)) = Or(And(Not(<[Member a,b,c]>),Not(<Trait3$1>)),Not(<Trait1$1>))
    trait Trait1
    trait Trait2
    val r1 = Or(Singleton(SMember(1,2,3)),Singleton(SAtomic(classOf[Trait1])))
    val r2 = Singleton(SAtomic(classOf[Trait2]))
    val a = Not(And(r1, r2)).canonicalize
    val b = Or(Not(r1), Not(r2)).canonicalize

    //xymbolyco.GraphViz.dfaView(Or(And(a,Not(b)),
    //                              And(b,Not(a))).toDfa(),"demorgan",true)

    assert(a ~= b)
  }

  test("demorgan 272"){

    val r1 = Cat(Singleton(SAtomic(classOf[java.lang.Number])), Singleton(SEql(0)))
    val r2 = Sigma
    val a = Not(And(r1, r2)).canonicalize
    val b = Or(Not(r1), Not(r2)).canonicalize

    println(s"r1=$r1")
    println(s"r2=$r2")
    println(s"And(r1,r2) = And($r1,$r2) = " + And(r1,r2).canonicalize)
    println(s"Not(r1) = Not($r1) = " + Not(r1).canonicalize)
    println(s"Not(r2) = Not($r2) = " + Not(r2).canonicalize)
    println(s"a=$a")
    println(s"b=$b")
    xymbolyco.GraphViz.dfaView(b.toDfa(),"b",true)
    xymbolyco.GraphViz.dfaView(a.toDfa(),"a",true)
    xymbolyco.GraphViz.dfaView(And(a,Not(b)).toDfa(),"a & !b",true)
    xymbolyco.GraphViz.dfaView(And(b,Not(a)).toDfa(),"b & !a",true)

    println("derivative = " + And(a,Not(b)).derivative(Some(SAtomic(classOf[java.lang.Number]))))

    assert(And(a,Not(b)).canonicalize == EmptySet)

    assert(a ~= b)
  }

}