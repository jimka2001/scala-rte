// Copyright (c) 2019 EPITA Research and Development Laboratory
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

package bdd
import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite

class MapColoringTestSuite extends AnyFunSuite {
  import org.scalactic.source
  override def test(testName: String, testTags: Tag*)(testFun: =>Any)(implicit pos: source.Position):Unit = {
    super.test(testName, testTags :_*){
      println(s"[ starting $testName")
      testFun
      println(s"] finished $testName")
    }
  }
  import MapColoring._

  test("coloring") {
    Bdd.withNewBddHash {
      val nodes: List[String] = List("a", "b", "c", "d")
      val uniDirectionalGraph: Map[String, Set[String]] = Map("a" -> Set(),
                                                              "b" -> Set("a"),
                                                              "c" -> Set("a"),
                                                              "d" -> Set("c", "b"))
      val biDirectionalGraph: Map[String, Set[String]] = Map("a" -> Set("b","c"),
                                                             "b" -> Set("a","d"),
                                                             "c" -> Set("a","d"),
                                                             "d" -> Set("c", "b"))

      val colors = Array("red", "green", "blue", "yellow")
      val (colorization, bdd) = graphToBdd(nodes,
                                           uniDirectionalGraph,
                                           biDirectionalGraph,
                                           4,
                                           (_,_)=>(),
                                           List(),
                                           1,
                                           verbose=false)
      bdd.visitSatisfyingAssignments { (assignTrue,assignFalse) =>
        val colorMapping: Map[String, String] = assignColors(colorization, assignTrue,assignFalse, colors)
        colors.foreach { color =>
          val sameColorStates = colorMapping.view.filterKeys { state => colorMapping(state) == color }.keys
          for {state1 <- sameColorStates
               state2 <- sameColorStates
               if state1 != state2
               } locally {
            // assert that if two states on the map are colored the same, then they don't connect
            //   i.e., they don't share a border as per uniDirectionalGraph
            assert(!uniDirectionalGraph(state1).contains(state2))
            assert(!uniDirectionalGraph(state2).contains(state1))
          }
        }
      }
    }
  }

  test("europe"){
    europeMapColoringTest(12, verbose = false)
  }

  test("usa"){
    colorizeMap(20, "test-us", "AL",
                USAgraph.stateUniGraph, USAgraph.stateBiGraph, List("MS","AL","TN"),
                verbose = false)
  }
  def sanityCheck(numNodes:Int,verbose:Boolean):Unit = {
    import adjuvant.Accumulators._
    Bdd.withNewBddHash {
      //val (states, subGraph) = findSubGraph("AL", numNodes)
      val (colorization,bdd) = graphToBdd(List("CA"),
                                           USAgraph.stateUniGraph,
                                           USAgraph.stateBiGraph,
                                           numNodes,
                                          (n,size)=> if (verbose) println(s"plot $n ${size()}"),
                                           List("AZ","CO","NM","UT"),
                                           1,
                                           verbose=verbose)

      if (verbose) println(s"colors=$colorization")
      val countSolutions =
        withCounter { count =>
          bdd.visitSatisfyingAssignments { (assignTrue:Assignment,assignFalse:Assignment) =>
            if (verbose)
              println(s"   color assignment="+ assignColors(colorization,
                                                            assignTrue,assignFalse,
                                                            Array("red","green","blue","yellow")))
            count()
          }
        }
      if (verbose)
        println(s"How many possible colorizations of the graph of $numNodes nodes = $countSolutions")
    }
  }

  test("sanity") {
    sanityCheck(4, verbose = false)
    sanityCheck(5, verbose = false)
    sanityCheck(10, verbose = false)
  }
}