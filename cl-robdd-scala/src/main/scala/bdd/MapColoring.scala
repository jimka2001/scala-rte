// Copyright (c) 2019,20 EPITA Research and Development Laboratory
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

import scala.annotation.tailrec

object MapColoring {
  type FOLD_FUN = (Seq[String], Bdd, String=>Bdd,(Bdd,Bdd)=>Bdd)=>Bdd

  val folders:Array[(String,FOLD_FUN)] = {
    import treereduce.TreeReducible._ // This imports the TreeReducible instances.
    import treereduce.TreeReduce._ // This imports the obj.treeMapReduce() syntax.
    Array((
           "tree-fold",
           (states, top, getConstraints, op) =>
             states.treeMapReduce(top)(getConstraints, op)),
         (
           "fold-left",
           (states, top, getConstraints, op) =>
             states.map(getConstraints).foldLeft(top)(op)))
  }

  // assign every state two Boolean variables to represent 1 of 4 colors
  def makeStateToVarMap(differentColor:List[String], allStates: List[String]): Map[String, (Int, Int)]  = {
    def correlate(bias:Int, states:List[String]):Map[String, (Int, Int)]  = states.zipWithIndex.map {
      case (st, index) => st -> (bias*2 + 2 * index + 1, bias*2 + 2 * index + 2)
    }.toMap
    val list2 = allStates.filterNot(x => differentColor.contains(x))
    correlate(0, differentColor) ++ correlate(differentColor.length, list2.reverse)
  }

  def uniGraphToBiGraph(uniGraph:Map[String,Set[String]]):Map[String,Set[String]] = {
    uniGraph.foldLeft(Map[String,Set[String]]()){
      case (acc:Map[String,Set[String]],(st1:String,states:Set[String])) =>
        acc ++ states.flatMap{st2 => Map(st1 -> (uniGraph(st1) + st2),
                                         st2 -> (uniGraph(st2)+st1))}}
  }

  def breadthFirstOrder(states:Set[String], uniGraph:Map[String,Set[String]]):List[String] = {

    val biGraph = uniGraphToBiGraph(uniGraph)
    @scala.annotation.tailrec
    def recur(generation:Set[String], generations:List[Set[String]]):List[String] = {
      val successors:Set[String] = generation.flatMap{biGraph}
      val nextGeneration = successors.filter{s => ! generations.exists(g => g.contains(s))} diff generation
      if (nextGeneration.isEmpty)
        generations.reverse.flatten
      else {
        recur(nextGeneration, generation::generations)
      }
    }
    recur(Set(states.head), List()) // just start with any one, doesn't matter for now
  }

  def graphToBdd(seed: List[String],
                 uniGraph:Map[String,Set[String]],
                 biGraph: Map[String, Set[String]],
                 numNodes:Int,
                 consume:(Double,()=>Double)=>Unit,
                 differentColor:List[String],
                 fold:Int): (Map[String,(Int,Int)],Bdd) = {

    require(differentColor.length <= 4) // states whose colors are different to reduce the size of the BDD
    import GenericGraph.orderStates

    val states = differentColor ++ orderStates(seed, biGraph).filter{st => !differentColor.contains(st)}.take(numNodes - differentColor.length)
    println(s"$numNodes (${states.length}) states: $states")

    val stateToVar: Map[String, (Int, Int)] = makeStateToVarMap(differentColor, states)
    // TODO we need to find C3 or C4 subgraph and fix those colors rather than taking differentColor as input parameter.
    val top: Bdd = differentColor.zipWithIndex.foldLeft(BddTrue: Bdd) { case (bdd, (st, index)) =>
      val (a, b) = stateToVar(st)
      val bit1 = (index & 1) == 1
      val bit2 = (index & 2) == 2
      //println(s"fixing color st=$st index=$index a=$a b=$b bit1=$bit1 bit2=$bit2")
      And(bdd,
          if (bit1) Bdd(a) else Bdd(-a),
          if (bit2) Bdd(b) else Bdd(-b))
    }

    def computeBorderConstraints(ab: String): Bdd = {
      // convert the connection (neighbor) information from a state (ab)
      //   to a Bdd representing the color constraints because neighboring
      //   states cannot have the same color.   a and b are the color bits
      //   of state ab.  c and d are the color bits of the neighbor.
      //   The constraint (per neighbor) is that either a and c are different
      //   or b and d are different.
      //   The computeBorderConstraints function AND's all these constraints for the
      //   neighbors of a given state.
      val (a, b) = stateToVar(ab)
      val neighbors = uniGraph.getOrElse(ab, Set())
      //val top: Bdd = BddTrue
      // println(s"    neighbors of $ab --> $neighbors")
      neighbors.foldLeft(top) { (acc2: Bdd, cd: String) =>
        if (states.contains(cd)) {
          val (c, d) = stateToVar(cd)
          // println(s"$ab -> $cd")
          And(acc2, Or(Xor(b, d), Xor(a, c)))
        }
        else
          top //BddTrue
      }
    }

    var n = 0
    (stateToVar,
      folders(fold)._2(states,
                       top,
                       computeBorderConstraints,
                       { (acc: Bdd, bdd: Bdd) =>
                         n = n + 1
                         val answer = And(acc, bdd)
                         val size = () => answer.size().toDouble
                         //GraphViz.bddView(answer,drawFalseLeaf=false,s"intermediate-$n-of-$numNodes")
                         consume(n.toDouble, size)
                         answer
                       }))
  }

  // calculate a mapping from graph node to color given that the hard work
  // of solving the Boolean equation has already been done.
  def assignColors[T](colorization:Map[String,(Int,Int)],assignTrue:Assignment,assignFalse:Assignment,colors:Array[T]):Map[String,T] = {
    // colorization maps the graph node to a pair of integers which represent the bitmask of the color
    //      which the node has been assigned.  such a Map[String,(Int,Int)] can be obtained from graphToBdd(...)
    // assign is an object which specifies which variables in the Bdd are set to true. such a value can
    //      be obtained from bdd.visitSatisfyingAssignments{ (assignTrue,assignFalse) => ...}
    // colors is an Array of length 4, each array entry is a user color,
    //      e.g. Array("red","green","blue","yellow")
    require(colors.length == 4)
    colorization.map{case (node,(v1,v2)) =>
      // v1 and v2 are labels (or variables within the Bdd), their Boolean value
      //    represents two bits of a color.  If the variable is not in the assignTrue
      //    object nor assignFalse it is a don't care (having been reduced from the Bdd)
      //    so we implicitly assume it is false.  that's good enough.
      val c1 = assignTrue.value(v1)
      val c2 = assignTrue.value(v2)
      val color = 2* (if (c1) 1 else 0 ) +  (if (c2) 1 else 0)
      node -> colors(color)
    }
  }

  def removeState(stateToRemove:String, biGraph:Map[String,Set[String]]):Map[String,Set[String]] = {
    (biGraph - stateToRemove ).map{case (st,states) =>
      st -> (states - stateToRemove)
    }
  }

  @tailrec
  def removeStates(statesToRemove:List[String], biGraph:Map[String,Set[String]]):Map[String,Set[String]] = {
    statesToRemove match {
      case Nil => biGraph
      case head::tail => removeStates(tail,removeState(head,biGraph))
    }
  }

  def colorizeMap(numNodes:Int,
                  baseName:String,
                  start:String,
                  uniGraph:Map[String,Set[String]],
                  biGraph:Map[String,Set[String]],
                  differentColor:List[String]):Map[String,String] = {
    //println(s"colorizeMap differentColor = $differentColor")
    import System.nanoTime
    val colors: Array[String] = Array("red", "green", "orange", "yellow")

    def colorize(fold             : Int,
                 newSize          : (Double, Double) => Unit,
                 newGcCount       : (Double, Double) => Unit,
                 newGcTime        : (Double, Double) => Unit,
                 newHashSize      : (Double, Double) => Unit,
                 newNumAllocations: (Double, Double) => Unit,
                 newTime          : (Double, Double) => Unit): Map[String, String] = {
      Bdd.withNewBddHash {
        // we first convert the graph to a Bdd without counting the size, because counting
        // the size is exponential in complexity given that the size of the Bdd grows
        // exponentially.
        val time0 = nanoTime.toDouble
        val bdd = graphToBdd(List(start), uniGraph, biGraph, numNodes,
                   (n                          : Double, _: () => Double) => {
                     newTime(n, nanoTime() - time0)
                   },
                   differentColor,
                   fold = fold)
        //GraphViz.bddView(bdd._2,drawFalseLeaf=false,s"$baseName-$numNodes")
        bdd
      }
      Bdd.withNewBddHash {
        import java.lang.management._

        // Thanks Jasper M for the following recipe.
        // https://users.scala-lang.org/u/jasper-m
        // https://users.scala-lang.org/t/how-to-call-a-function-from-a-java-library/5722/2
        val beans: Array[GarbageCollectorMXBean] = ManagementFactory
          .getGarbageCollectorMXBeans
          .toArray(Array.empty[GarbageCollectorMXBean])

        def gcCount(): Long = beans.foldLeft(0L) { (acc, b) => b.getCollectionCount + acc }

        def gcTime(): Long = beans.foldLeft(0L) { (acc, b) => b.getCollectionTime + acc }

        val gcCount0 = gcCount().toDouble
        val gcTime0 = gcTime().toDouble
        val (colorization, bdd) = graphToBdd(List(start), uniGraph, biGraph, numNodes,
                                             (n: Double, size: () => Double) => {
                                               newGcCount(n, gcCount() - gcCount0)
                                               newGcTime(n, gcTime() - gcTime0)
                                               newSize(n, size())
                                               val (hashSize, numAllocations) = Bdd.getBddSizeCount()
                                               newHashSize(n, hashSize.toDouble)
                                               newNumAllocations(n, numAllocations.toDouble)
                                             },
                                             differentColor,
                                             fold = fold)
        bdd.findSatisfyingAssignment() match {
          case None => Map[String, String]()
          case Some((assignTrue, assignFalse)) =>
            val ret = assignColors(colorization, assignTrue, assignFalse, colors)
            println(s"  $numNodes color assignment=" + ret)
            ret
        }
      }
    }

    val sizes: Array[List[(Double, Double)]] = Array(List(), List())
    val gcTimes: Array[List[(Double, Double)]] = Array(List(), List())
    val gcCounts: Array[List[(Double, Double)]] = Array(List(), List())
    val times: Array[List[(Double, Double)]] = Array(List(), List())
    val hashSizes: Array[List[(Double, Double)]] = Array(List(), List())
    val numAllocations: Array[List[(Double, Double)]] = Array(List(), List())


    var retain: Map[String, String] = null

    for {((folder, _),k) <- folders.zipWithIndex
         } {
      println(s" calculating $folder for numNodes=$numNodes")
      retain = colorize(fold = k,
                        (n: Double, m: Double) => sizes(k) = (n -> m) :: sizes(k),
                        (n: Double, m: Double) => gcCounts(k) = (n -> m) :: gcCounts(k),
                        (n: Double, m: Double) => gcTimes(k) = (n -> m) :: gcTimes(k),
                        (n: Double, m: Double) => hashSizes(k) = (n -> m) :: hashSizes(k),
                        (n: Double, m: Double) => numAllocations(k) = (n -> m) :: numAllocations(k),
                        (n: Double, m: Double) => times(k) = (n -> m) :: times(k))
    }
    //    val reclaimed: Array[List[(Double, Double)]] = for {(numAllocation:List[(Double,Double)],hashSize:List[(Double,Double)]) <- numAllocations.zip(hashSizes)
    //                                                        ((n1:Double,numObj1:Double),(n2:Double,numObj2:Double)) <- numAllocation.zip(hashSize)
    //                                                        } yield {
    //      assert(n1 == n2)
    //      (n1,numObj1 - numObj2)
    //    }

    val reclaimed: Array[List[(Double, Double)]] = numAllocations.zip(hashSizes)
      .map { case (numAllocation: List[(Double, Double)], hashSizes: List[(Double, Double)]) =>
        def tmp(l1: List[(Double, Double)], l2: List[(Double, Double)]): List[(Double, Double)] = {
          l1.lazyZip(l2).flatMap { case ((n1: Double, numObj1: Double), (n2: Double, numObj2: Double)) =>
            assert(n1 == n2)
            if (0.0 == numObj1 - numObj2)
              List()
            else List((n1, numObj1 - numObj2))
          }
        }

        tmp(numAllocation, hashSizes)

      }

    for {(measurements, scale, yLog, title, yAxisLabel, outputFileBaseName)
           <- List((sizes, 1 / 1000.0, true,
                     "Allocation for Map 4-coloring",
                     "Bdd size computed per step (K objects)",
                     "allocation"),
                   (gcCounts, 1.0, false,
                     "GC count for Map 4-coloring",
                     "GC count",
                     "gc-count "),
                   (gcTimes, 1.0, true,
                     "GC time for map 4-coloring",
                     "GC time (ms)",
                     "gc-time"),
                   (hashSizes, 1 / 1000.0, true,
                     "Hash Size for Map 4-coloring",
                     "Hash size at step (K objects)",
                     "hash-size"),
                   (numAllocations, 1 / 1000.0, true,
                     "Num Allocations for Map 4-coloring",
                     "Num Allocations accumulated through step (K objects)",
                     "num-allocations"),
                   (reclaimed, 1/1000.0, true,
                     "Num Objects reclaimed for Map 4-coloring",
                     "Num Objects reclaimed through step (K objects)",
                     "reclaimed"),
                   (times, 1e-9, true,
                     "Time elapsed for Map 4-coloring",
                     "Total time elapsed (sec)",
                     "time")
                   )} {
      import gnuplot.GnuPlot._

      gnuPlot(folders.zipWithIndex.map{case((folder,_),k) =>
        (s"Using $folder", measurements(k).map(_._1), measurements(k).map(_._2).map(_ * scale))}.toList)(
        title = title,
        xAxisLabel = "Step",
        yAxisLabel = yAxisLabel,
        yLog = yLog,
        grid = true,
        outputFileBaseName = s"$baseName-$numNodes-4-color-$outputFileBaseName"
        )
    }
    retain
  }

  def usMapColoringTest(numRegions:Int): Int = {
    import USAgraph._
    biGraphToDot(stateBiGraph, statePositions, s"us-political-$numRegions")(symbols = symbols)
    val colors = colorizeMap(numRegions, "US", "ME",
                             stateUniGraph, // removeStates(List("Russia"), stateUniGraph),
                             stateBiGraph, //removeStates(List("Russia"), stateBiGraph),
                             List("MA", "VT", "NH"))
    biGraphToDot(stateBiGraph, statePositions, s"us-political-$numRegions-colors"
                 )(symbols = symbols,
                   colors = { st => colors.getOrElse(st, "no-color") })
  }

  def europeMapColoringTest(numRegions:Int): Int = {
    import EuropeGraph._
    biGraphToDot(stateBiGraph, statePositions, s"europe-political-$numRegions")(symbols = symbols)
    val colors = colorizeMap(numRegions, "europe", "Russia",
                             stateUniGraph, //removeStates(List("Russia"), stateUniGraph),
                             stateBiGraph, // removeStates(List("Russia"), stateBiGraph),
                             List("Croatia", "Bosnia", "Serbia", "Montenegro"))
    biGraphToDot(stateBiGraph, statePositions, s"europe-political-$numRegions-colors"
                 )(symbols = symbols,
                   colors = { st => colors.getOrElse(st, "no-color") })
  }

  def main(argv: Array[String]): Unit = {
    for {numNodes <- 35 to 49} {
      import java.lang.System.nanoTime

      val time0 = nanoTime
      println(s" === coloring $numNodes regions")
      //europeMapColoringTest(numNodes)
      usMapColoringTest(numNodes)
      val now = nanoTime
      val elapsed = (now - time0 ) * 1e9
      println(s"Time to colorize $numNodes regions was $elapsed sec")
    }
  }
}
