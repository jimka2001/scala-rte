// Copyright (c) 2019,20,22 EPITA Research and Development Laboratory
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

package graphcolor

import bdd.{And, Assignment, Bdd, BddFalse, BddTrue, Or, Xor}
import gnuplot.GnuPlot.gnuPlot

import java.lang.System.nanoTime
import scala.annotation.tailrec

object MapColoring {
  type FOLD_FUN[V] = (Seq[V], Bdd, V => Bdd, (Bdd, Bdd) => Bdd) => Bdd

  def folders[V](): Array[(String, FOLD_FUN[V])] = {
    import treereduce.TreeReduce._
    import treereduce.TreeReducible._ // This imports the obj.treeMapReduce() syntax.
    Array((
            "tree-fold",
            (states, top, getConstraints, op) =>
              states.treeMapReduce(top)(getConstraints, op)),
          (
            "fold-left",
            (states, top, getConstraints, op) =>
              states.map(getConstraints).reduceOption(op) match {
                case None => top
                case Some(bdd) => bdd
              }))
  }

  // assign every state two Boolean variables to represent 1 of 4 colors
  def makeStateToVarMap[V](differentColor: List[V], allStates: List[V]): Map[V, (Int, Int)] = {
    def correlate(bias: Int, states: List[V]): Map[V, (Int, Int)] = states.zipWithIndex.map {
      case (st, index) => st -> (bias * 2 + 2 * index + 1, bias * 2 + 2 * index + 2)
    }.toMap

    val list2 = allStates.filterNot(x => differentColor.contains(x))
    correlate(0, differentColor) ++ correlate(differentColor.length, list2.reverse)
  }

  def uniGraphToBiGraph[V](uniGraph: Map[V, Set[V]]): Map[V, Set[V]] = {
    uniGraph.foldLeft(Map[V, Set[V]]()) {
      case (acc: Map[V, Set[V]], (st1, states: Set[V])) =>
        acc ++ states.flatMap { st2 =>
          Map(st1 -> (uniGraph(st1) + st2),
              st2 -> (uniGraph(st2) + st1))
        }
    }
  }

  def biGraphToUniGraph[V](biGraph:Map[V,Set[V]]): Map[V,Set[V]] = {
    val vertexToIndex = biGraph.keys.toVector.zipWithIndex.toMap
    biGraph.map{case (v,neighbors:Set[V]) =>
      v -> neighbors.filter{n => vertexToIndex(n) <= vertexToIndex(v)}
    }.filter {
      case (_, neighbors: Set[V]) => neighbors.nonEmpty
    }
  }

  def breadthFirstOrder[V](states: Set[V], uniGraph: Map[V, Set[V]]): List[V] = {

    val biGraph = uniGraphToBiGraph(uniGraph)

    @scala.annotation.tailrec
    def recur(generation: Set[V], generations: List[Set[V]]): List[V] = {
      val successors: Set[V] = generation.flatMap {
        biGraph
      }
      val nextGeneration = successors.filter { s => !generations.exists(g => g.contains(s)) } diff generation
      if (nextGeneration.isEmpty)
        generations.reverse.flatten
      else {
        recur(nextGeneration, generation :: generations)
      }
    }

    recur(Set(states.head), List()) // just start with any one, doesn't matter for now
  }

  def politicalGraphToBdd[V](seed: List[V],
                             uniGraph: Map[V, Set[V]],
                             biGraph: Map[V, Set[V]],
                             numNodes: Int,
                             consume: (Double, () => Double) => Unit,
                             differentColor: List[V],
                             fold: Int,
                             verbose: Boolean): (Map[V, (Int, Int)], Bdd) = {

    require(differentColor.length <= 4) // states whose colors are different to reduce the size of the BDD
    import graphcolor.GenericGraph.orderStates

    val states = differentColor ++ orderStates(seed, biGraph)
      .filter { st => !differentColor.contains(st) }
      .take(numNodes - differentColor.length)
    if (verbose)
      println(s"$numNodes (${states.length}) states: $states")

    val stateToVar: Map[V, (Int, Int)] = makeStateToVarMap(differentColor, states)
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

    def computeBorderConstraints(ab: V): Bdd = {
      // convert the connection (neighbor) information from a state (ab)
      //   to a Bdd representing the color constraints because neighboring
      //   states cannot have the same color.   a and b are the color bits
      //   of state ab.  c and d are the color bits of the neighbor.
      //   The constraint (per neighbor) is that either a and c are different
      //   or b and d are different.
      //   The computeBorderConstraints function intersects all these constraints for the
      //   neighbors of a given state.
      val (a, b) = stateToVar(ab)
      val neighbors = uniGraph.getOrElse(ab, Set())
      //val top: Bdd = BddTrue
      // println(s"    neighbors of $ab --> $neighbors")
      neighbors.foldLeft(top) { (acc2: Bdd, cd: V) =>
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
      folders[V]()(fold)._2(states,
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
  def assignColors[V, T](colorization: Map[V, (Int, Int)],
                         assignTrue: Assignment,
                         assignFalse: Assignment,
                         colors: Array[T]): Map[V, T] = {
    // colorization maps the graph node to a pair of integers which represent the bitmask of the color
    //      which the node has been assigned.  such a Map[String,(Int,Int)] can be obtained from graphToBdd(...)
    // assign is an object which specifies which variables in the Bdd are set to true. such a value can
    //      be obtained from bdd.visitSatisfyingAssignments{ (assignTrue,assignFalse) => ...}
    // colors is an Array of length 4, each array entry is a user color,
    //      e.g. Array("red","green","blue","yellow")
    require(colors.length == 4)
    colorization.map { case (node, (v1, v2)) =>
      // v1 and v2 are labels (or variables within the Bdd), their Boolean value
      //    represents two bits of a color.  If the variable is not in the assignTrue
      //    object nor assignFalse it is a don't care (having been reduced from the Bdd)
      //    so we implicitly assume it is false.  that's good enough.
      val c1 = assignTrue.value(v1)
      val c2 = assignTrue.value(v2)
      val color = 2 * (if (c1) 1 else 0) + (if (c2) 1 else 0)
      node -> colors(color)
    }
  }

  def removeState[V](stateToRemove: V, gr: Map[V, Set[V]]): Map[V, Set[V]] = {
    (gr - stateToRemove).map { case (st, states) =>
      st -> (states - stateToRemove)
    }
  }

  @tailrec
  def removeStates[V](statesToRemove: List[V], gr: Map[V, Set[V]]): Map[V, Set[V]] = {
    statesToRemove match {
      case Nil => gr
      case head :: tail => removeStates(tail, removeState(head, gr))
    }
  }

  def addEdge[V](v1:V,v2:V,biGraph:Map[V,Set[V]]):Map[V,Set[V]] = {
    biGraph
      .updated(v1,biGraph.getOrElse(v1,Set[V]()) + v2)
      .updated(v2,biGraph.getOrElse(v2,Set[V]()) + v1)
  }

  @tailrec
  def addEdges[V](biGraph:Map[V,Set[V]],edges:List[(V,V)]):Map[V,Set[V]] = {
    edges match {
      case Nil => biGraph
      case (v1,v2)::edges => addEdges(addEdge(v1,v2,biGraph),edges)
    }
  }
  @tailrec
  def unwindSimpleVertices[V](numCrayons: Int,
                              biGraph: Map[V, Set[V]],
                              unwind: List[(V, Set[V])]): (Map[V, Set[V]],
    Map[V, Set[V]], List[(V, Set[V])]) = {

    biGraph.iterator.find {
      case (_, neighbors: Set[V]) => neighbors.size < numCrayons
    } match {
      case None => (biGraphToUniGraph(biGraph), biGraph,unwind)
      case Some((v, neighbors: Set[V])) =>
        unwindSimpleVertices(numCrayons, removeState(v, biGraph), (v, neighbors) :: unwind)
    }
  }

  @tailrec
  def rewindSimpleVertices[V](colors:Array[String] = Array("red", "green", "orange", "yellow"),
                              biGraph:Map[V,Set[V]],
                              colorized:Map[V,String],
                              unwind:List[(V,Set[V])]):Map[V,String] = {
    import scala.util.Random
    unwind match {
      case Nil => colorized
      case (v,neighbors)::vns =>
        Random.shuffle(colors.toSeq).find{c =>
          ! neighbors.exists(n => colorized.getOrElse(n,-1) == c)
        } match {
          case None => throw new Exception(s"failed to colorize using simple algorithm")
          case Some(c) => rewindSimpleVertices(colors,
                                               addEdges(biGraph,neighbors.map{n => v -> n}.toList),
                                               colorized + (v -> c),
                                               vns)
        }
    }
  }

  def colorizeMap[V](givenBiGraph: Map[V, Set[V]],
                     palette: Array[String] = Array("red", "green", "orange", "yellow"),
                    ): Map[V, String] = {

    def colorize(uniGraph:Map[V, Set[V]],biGraph:Map[V, Set[V]]):Map[V, String] = {
      if (uniGraph.isEmpty)
        Map()
      else
        Bdd.withNewBddHash {
          val (colorization, bdd) = politicalGraphToBdd(List(biGraph.head._1), uniGraph, biGraph, biGraph.size,
                                               (_, _) => (),
                                                        Nil,
                                                        fold = 0,
                                                        verbose = false)
          bdd.findSatisfyingAssignment(determinist=false) match {
            case None => Map[V, String]()
            case Some((assignTrue, assignFalse)) =>
              assignColors(colorization, assignTrue, assignFalse, palette)
          }
        }
    }
    val (uniGraph,biGraph,unwind) = unwindSimpleVertices(4, givenBiGraph, Nil)

    rewindSimpleVertices(palette,biGraph,colorize(uniGraph,biGraph),unwind)
  }

  def keepMins(triples:Seq[(String,Int,Double)]):List[(String,Int,Double)] = {
    def recur(in:List[(String,Int,Double)],out:List[(String,Int,Double)]):List[(String,Int,Double)] = {
      in match {
        case (trip1@(_,n1,time1))::(trip2@(_,n2,time2)) :: trips
      if n1 == n2 => if (time1 < time2)
        // omit time2
        recur(trip1::trips, out)
        else
        // omit time1
        recur(trip2::trips, out)
        case h::t => recur(t,h::out)
        case Nil => out.reverse
      }
    }
    recur(triples.toList,List())
  }

  def timeColorizeGraphs[V](maxNumNodes:Int,
                            gnuFileCB:String=>Unit = (_)=>()):Unit = {
    // create plots showing time of different fold algorithms
    //   x-axis is number of states to be colors.
    val start = "Germany"
    val uniGraph = EuropeGraph.stateUniGraph
    val biGraph = EuropeGraph.stateBiGraph
    val numSamples = 4
    val data = for {
      k <- 1 to numSamples
      numNodes <- 10 to maxNumNodes
      ((text, _), fold) <- folders[V]().zipWithIndex.reverse
    } yield
      Bdd.withNewBddHash {
        println(s"k=$k numNodes=$numNodes  $text")
        // we first convert the graph to a Bdd without counting the size, because counting
        // the size is exponential in complexity given that the size of the Bdd grows
        // exponentially.
        val time0 = nanoTime.toDouble
        politicalGraphToBdd(List(start), uniGraph, biGraph, numNodes,
                            (_: Double, _: () => Double) => (),
                            List(),
                            fold = fold,
                            verbose = true)
        (text, numNodes, (nanoTime.toDouble - time0)/1e6)
      }
    val grouped = data.groupBy(_._1).toSeq
    gnuPlot(for {(text, triples) <- grouped
                 // each triple has the form (text, numNodes, time)
                 ordered = triples.sortBy(_._2)
                 } yield (text, ordered.map(_._2).map(_.toDouble), ordered.map(_._3)))(
      title = "Time per number of states (with HotSpot)",
      xAxisLabel = "Number of states",
      yAxisLabel = "Time (ms)",
      yLog = true,
      grid = true,
      key = "inside left",
      outputFileBaseName = "time-range-per-num-states-",
      view = true,
      verbose = true
      )
    // each element of grouped is of the form
    //    (text List[text,numNodes,time])
    //    we want to filter that List[...] so that numNodes appears
    //    only once,   if the starting list has multiple entries for
    //    a given numNodes, then keep only the one with minimum time.
    val justMin = for{ (text,triples) <- grouped
                       } yield (text,keepMins(triples.sortBy(_._2)))
    gnuPlot(for {(text, triples) <- justMin
                 // each triple has the form (text, numNodes, time)
                 // (time, triples) <- triples.groupBy(_._1)
                 ordered = triples.sortBy(_._2)
                 } yield (text, ordered.map(_._2).map(_.toDouble), ordered.map(_._3)))(
      title = s"Time per number of states (best of $numSamples)",
      xAxisLabel = "Number of states",
      yAxisLabel = "Time (ms)",
      yLog = true,
      grid = true,
      outputFileBaseName = "time-per-num-states-",
      gnuFileCB = gnuFileCB,
      key = "inside left",
      view = true,
      verbose = true
      )
    def ratioData(triples1:List[(String,Int,Double)],
                  triples2:List[(String,Int,Double)],
                  ds1:List[Double],
                  ds2:List[Double]):(List[Double],List[Double]) = {
      (triples1,triples2) match {
        case (Nil,_) |
             (_, Nil) =>  (ds1.reverse,ds2.reverse)
        case ((_,n1,_)::ts1,(_,n2,_)::_) if n1 < n2 => ratioData(ts1,triples2,ds1,ds2)
        case ((_,n1,_)::_,(_,n2,_)::ts2) if n1 > n2 => ratioData(triples1,ts2,ds1,ds2)
        case ((_,n1,t1)::ts1,(_,_,t2)::ts2) => ratioData(ts1,ts2,
                                                       n1.toDouble::ds1,
                                                         t1/t2::ds2)
      }
    }
    val justMinMap = justMin.toMap
    val (numNodess,ratios) = ratioData(justMinMap("fold-left").sortBy(_._2),
                                       justMinMap("tree-fold").sortBy(_._2),
                                       List(),List())
    gnuPlot(List(("fold-left/tree-fold", numNodess, ratios)))(
      title = s"Time ratios (best of $numSamples)",
      xAxisLabel = "Number of states",
      yAxisLabel = "Time ratio",
      yLog = false,
      grid = true,
      outputFileBaseName = "time-ratio-num-states-",
      gnuFileCB = gnuFileCB,
      key = "inside left",
      view = false,
      verbose = true
    )
  }


  def timedColorizeMap[V](numNodes: Int,
                          baseName: String,
                          start: V,
                          uniGraph: Map[V, Set[V]],
                          biGraph: Map[V, Set[V]],
                          differentColor: List[V],
                          colors: Array[String] = Array("red", "green", "orange", "yellow"),
                          gnuFileCB: String=>Unit = (_)=>(),
                          view:Boolean = false,
                          verbose: Boolean): Map[V, String] = {
    if (verbose)
      println(s"colorizeMap differentColor = $differentColor")
    import System.nanoTime

    def colorize(fold: Int,
                 newSizeCB: (Double, Double) => Unit,
                 newGcCountCB: (Double, Double) => Unit,
                 newGcTimeCB: (Double, Double) => Unit,
                 newHashSizeCB: (Double, Double) => Unit,
                 newNumAllocationsCB: (Double, Double) => Unit,
                 newTimeCB: (Double, Double) => Unit): Map[V, String] = {
      Bdd.withNewBddHash {
        // we first convert the graph to a Bdd without counting the size, because counting
        // the size is exponential in complexity given that the size of the Bdd grows
        // exponentially.
        val time0 = nanoTime.toDouble
        val bdd = politicalGraphToBdd(List(start), uniGraph, biGraph, numNodes,
                             (n: Double, _: () => Double) => {
                               newTimeCB(n, nanoTime() - time0)
                             },
                                      differentColor,
                                      fold = fold,
                                      verbose = verbose)
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
        val (colorization, bdd) = politicalGraphToBdd(List(start), uniGraph, biGraph, numNodes,
                                             (n: Double, size: () => Double) => {
                                               newGcCountCB(n, gcCount() - gcCount0)
                                               newGcTimeCB(n, gcTime() - gcTime0)
                                               newSizeCB(n, size())
                                               val (hashSize, numAllocations) = Bdd.getBddSizeCount
                                               newHashSizeCB(n, hashSize.toDouble)
                                               newNumAllocationsCB(n, numAllocations.toDouble)
                                             },
                                                      differentColor,
                                                      fold = fold,
                                                      verbose = verbose)
        bdd.findSatisfyingAssignment(determinist=false) match {
          case None => Map[V, String]()
          case Some((assignTrue, assignFalse)) =>
            val ret: Map[V, String] = assignColors(colorization, assignTrue, assignFalse, colors)
            if (verbose)
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

    var retain: Map[V, String] = null

    for {((folder, _), k) <- folders[V]().zipWithIndex
         } {
      if (verbose)
        println(s" calculating $folder for numNodes=$numNodes")
      retain = colorize(fold = k,
                        (n: Double, m: Double) => sizes(k) = (n -> m) :: sizes(k),
                        (n: Double, m: Double) => gcCounts(k) = (n -> m) :: gcCounts(k),
                        (n: Double, m: Double) => gcTimes(k) = (n -> m) :: gcTimes(k),
                        (n: Double, m: Double) => hashSizes(k) = (n -> m) :: hashSizes(k),
                        (n: Double, m: Double) => numAllocations(k) = (n -> m) :: numAllocations(k),
                        (n: Double, m: Double) => times(k) = (n -> m) :: times(k))
    }

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
                     "BDD node count x1000 per step n",
                     "allocation"),
                   (gcCounts, 1.0, false,
                     "GC count for Map 4-coloring",
                     "GC count",
                     "gc-count"),
                   (gcTimes, 1.0, true,
                     "GC time for map 4-coloring",
                     "GC time (ms)",
                     "gc-time"),
                   (hashSizes, 1 / 1000.0, true,
                     "Hash Size for Map 4-coloring",
                     "Hash size at step (K objects)",
                     "hash-size"),
                   (numAllocations, 1 / 1000.0, true,
                     "Num BDD Allocations for Map 4-coloring",
                     "BDD node count x1000 through step n",
                     "num-allocations"),
                   (reclaimed, 1 / 1000.0, true,
                     "Num Objects reclaimed for Map 4-coloring",
                     "Num Objects reclaimed through step (K objects)",
                     "reclaimed"),
                   (times, 1e-9, true,
                     "Incremental time for Map 4-coloring",
                     "Time elapsed (sec) for first n steps",
                     "time")
                   )} {
      import gnuplot.GnuPlot._

      gnuPlot(folders[V]().zipWithIndex.map { case ((folder, _), k) =>
        (folder, measurements(k).map(_._1), measurements(k).map(_._2).map(_ * scale))
      }.toList)(
        title = title,
        xAxisLabel = "Step",
        yAxisLabel = yAxisLabel,
        yLog = yLog,
        grid = true,
        key = "inside left",
        outputFileBaseName = s"$baseName-$numNodes-4-color-$outputFileBaseName",
        gnuFileCB=gnuFileCB,
        view = view,
        verbose = verbose
        )
    }
    retain
  }
}

object sampleColoring {
  import MapColoring._
  import graphcolor.GenericGraph._
  def usTimedMapColoringTest(numRegions:Int, view:Boolean, verbose:Boolean): Int = {
    import USAgraph._
    biGraphToDot(stateBiGraph, statePositions, s"us-political-$numRegions")(symbols = symbols,
                                                                            view=view,
                                                                            verbose=verbose)
    val colors = timedColorizeMap(numRegions, "US", "ME",
                                  stateUniGraph,
                                  stateBiGraph,
                                  List("MA", "VT", "NH"),
                                  view=view,
                                  verbose = false)
    biGraphToDot(stateBiGraph, statePositions, s"us-political-$numRegions-colors"
                 )(symbols = symbols,
                   colors = { st => colors.getOrElse(st, "no-color") },
                   view=view,
                   verbose=verbose)
  }

  def europeTimedMapColoringTest(numRegions:Int,
                                 gnuFileCB:String=>Unit=(_)=>(),
                                 view:Boolean,
                                 verbose:Boolean): Int = {
    import EuropeGraph._
    val pallet = Array("red", "green", "orange", "yellow")

    biGraphToDot[String](stateBiGraph, statePositions, s"europe-political-$numRegions"
                         )(symbols = symbols,
                           view=view,
                           verbose=verbose)

    val colors = timedColorizeMap(numRegions, "europe", "Germany",
                                  removeStates(List(), stateUniGraph),
                                  removeStates(List(), stateBiGraph),
                                  List("Croatia", "Bosnia", "Serbia", "Montenegro"),
                                  colors = pallet,
                                  gnuFileCB = gnuFileCB,
                                  view = view,
                                  verbose=verbose)

    biGraphToDot(stateBiGraph, statePositions, s"europe-political-$numRegions-colors"
                 )(symbols = symbols,
                   colors = { st => colors.getOrElse(st, "no-color") },
                   view = view,
                   verbose = verbose)
  }

  def europeMapColoringTest(): Int = {
    import EuropeGraph._
    val palette = Array("red", "green", "orange", "yellow")
    val numRegions = stateUniGraph.size
    biGraphToDot[String](stateBiGraph, statePositions, s"europe-political-$numRegions"
                         )(symbols = symbols,view=false,verbose=false)
    val colorized = colorizeMap( stateBiGraph, palette)

    biGraphToDot(stateBiGraph, statePositions, s"europe-political-$numRegions-colors"
                 )(symbols = symbols,
                   colors = { st => colorized.getOrElse(st, "no-color") },
                   view=false,
                   verbose = false)
  }

  def main(argv: Array[String]): Unit = {
    europeMapColoringTest()
    timeColorizeGraphs(41)

  }
}
