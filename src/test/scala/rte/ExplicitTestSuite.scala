// copyright (c) 2024 epita research laboratory
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

import adjuvant.AdjFunSuite
import adjuvant.Adjuvant.{eql, fixedPoint}
import genus._
import rte.RteImplicits._
import genus.GenusImplicits._
import rte.Rte.rteView
import rte.Sigma.toDfa
import xymbolyco.GraphViz.dfaView
import xymbolyco.{Dfa, GraphViz}


class ExplicitTestSuite extends AdjFunSuite {
  val I:Rte = Singleton(classOf[Int])
  val S:Rte = Singleton(classOf[String])
  //val X = Singleton(SEql(-1))
  //val Y = Singleton(SEql(1))
  val ε = EmptyWord
  val ∅ = EmptySet
  val SInt = SSatisfies(Types.intp,"Int")
  val SDouble = SSatisfies(Types.doublep,"Double")
  test("match 1") {
    val rte1 = Cat(I, Star(Or(I, S)), S)
    val rte2 = Cat(classOf[Int], Star(Or(classOf[Int],classOf[String])), classOf[String])
    val rte3 = I ++ (I | S).* ++ S
    val rte4 = SInt ++ (SInt | S).* ++ S // SInt is an SSatisfies type

    // GraphViz.dfaView(Dfa.dfaXor(rte4.toDfa(),rte3.toDfa()), title="xor")
    for{r <- Seq(rte1, rte2, rte3, rte4)} {

      assert(None == r.simulate(42, List(1, 2, 3, 3.2, "hello", "world")))

      assert(Some(42) == r.simulate(42, List(1, 2, 3, "hello", "world")))
      assert(Some(42) == r.simulate(42, List(1, 2, 3,
                                                "a", 4, "b", "c", 5, 6, 7,
                                                "hello", "world")))
    }
  }

  test("match 2"){
    import Types.{evenType,oddType}
    val rte = (evenType() ++ oddType()).*
    assert(Some(42) == rte.simulate(42, List(2,3,4,5)))
    assert(None == rte.simulate(42, List(3,4,5,6)))
    assert(None == rte.simulate(42, List("hello",2,3,4,5,6,7)))
  }
  test("match 3"){
    import Types.{evenType,oddType}
    val S:Rte = Singleton(classOf[String])

    val rte = S.? ++ (evenType() ++ oddType()).*
    assert(None == rte.simulate(42, List("hello",3,4,5)))
    assert(None == rte.simulate(42, List(3,4,5,6)))
    assert(Some(42) == rte.simulate(42, List("hello",2,3,4,5,6,7)))
    assert(Some(42) == rte.simulate(42, List(2,3,4,5,6,7)))
  }
  test("match 4"){

    val rte1 = I ++ (I | S).* ++ S
    val rte2 = I ++ S ++ I.* ++ S.?

    val rte3 = rte1 & !rte2
    // rteView(rte3, "title rte3")
    assert(Some(42) == rte3.simulate(42, List(1,"a", "b", "c")))
    assert(None == rte2.simulate(42, List(1,"a", "b", 3, false)))

    val rte4 = rte1 & rte2
    // rteView(rte4, "title rte4")
    assert(Some(42) == rte4.simulate(42, List(1, "a")))
    assert(Some(42) == rte4.simulate(42, List(1, "a", 2, "b")))
    assert(None == rte4.simulate(42, List(1, "a", "b", "c")))

    val rte5 = !rte1 & rte2
    rteView(rte5, "title rte5")
    assert(Some(42) == rte5.simulate(42, List(1, "a", 2, 3, 4, 5)))
    assert(None == rte5.simulate(42, List(1, "a", 2, 3, 4, 5, false)))

  }
  test("match 5"){
    val data = Seq("integers", 100, 200, 300,
                   "floats", 10.0, 20.0.toFloat,
                   "floats",
                   "integers", 1, 2, 3,
                   "integers", -1, -3, -7, -8)
    val F = SAtomic(classOf[Double]) | SAtomic(classOf[Float])
    val I = SAtomic(classOf[Int])
    val keyI = SEql("integers") // will be implicitly converted to Rte
    val keyF = SEql("floats")   // will be implicitly converted to Rte

    val re:Rte = ((keyI ++ I.*) | (keyF ++ F.*)).*

    assert( (keyI ++ I.*).contains(Seq("integers", 100, 200, 300)))
    assert( (keyI ++ I.*).contains(Seq("integers")))

    assert( keyF.contains(Seq("floats")))

    dfaView(re.toDfa(true), title="floats", dotFileCB=println, showSink=false)
    assert( F.*.contains(Seq(1.1, 2.2, 3.3)))
    assert( (keyF ++ F.*).contains(Seq("floats", 1.1, 2.2, 3.3)))
    assert( (keyF ++ F.*).contains(Seq("floats")))

    assert(re.contains(data))

  }
}
