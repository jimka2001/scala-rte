package bdd

object MapColoring {

  // assign every state two Boolean variables to represent 1 of 4 colors
  def makeStateToVarMap(allStates: List[String]): Map[String, (Int, Int)] = {
    allStates.zipWithIndex.map {
      case (st, index) => (st -> (2 * index + 1, 2 * index + 2))
    }.toMap
  }
  def uniGraphToBiGraph(uniGraph:Map[String,Set[String]]):Map[String,Set[String]] = {
    uniGraph.foldLeft(Map[String,Set[String]]()){
      case (acc:Map[String,Set[String]],(st1:String,states:Set[String])) =>
        acc ++ states.flatMap{st2 => Map((st1 -> (uniGraph(st1)+st2)),
                                         (st2 -> (uniGraph(st2)+st1)))}}
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
                 numNodes:Int,consume:(Double,Double)=>Unit,
                 differentColor:List[String],
                 fold:Int): (Map[String,(Int,Int)],Bdd) = {

    require(differentColor.length <= 4) // states whose colors are different to reduce the size of the BDD
    import GenericGraph.orderStates

    val states = orderStates(seed, biGraph).take(numNodes)
    println(s"$numNodes (${states.length}) states: $states")

    val stateToVar: Map[String, (Int, Int)] = makeStateToVarMap(states)
    // TODO we need to find C3 or C4 subgraph and fix those colors rather than taking differentColor as input parameter.
    val top: Bdd = differentColor.intersect(states).zipWithIndex.foldLeft(BddTrue:Bdd){case (bdd,(st,index)) =>
      val (a,b) = stateToVar(st)
      val bit1 = (index & 1) == 1
      val bit2 = (index & 2) == 2
      //println(s"fixing color st=$st index=$index a=$a b=$b bit1=$bit1 bit2=$bit2")
      And(bdd,
          (if(bit1) Bdd(a) else Bdd(-a)),
          (if(bit2) Bdd(b) else Bdd(-b)))
    }
    val topx:Bdd = BddTrue // or fixColors

    def getConstraints(ab: String): Bdd = {
      // convert the connection (neighbor) information from a state (ab)
      //   to a Bdd representing the color constraints because neighboring
      //   states cannot have the same color.   a and b are the color bits
      //   of state ab.  c and d are the color bits of the neighbor.
      //   The constraint (per neighbor) is that either a and c are different
      //   or b and d are different.
      //   The getConstraints function AND's all these constraints for the
      //   neighbors of a given state.
      val (a, b) = stateToVar(ab)
      val neighbors = uniGraph.getOrElse(ab, Set())
      val top: Bdd = BddTrue
      // println(s"    neighbors of $ab --> $neighbors")
      neighbors.foldLeft(top) { (acc2: Bdd, cd: String) =>
        if (states.contains(cd)) {
          val (c, d) = stateToVar(cd)
          // println(s"$ab -> $cd")
          And(acc2, Or(Xor(b, d), Xor(a, c)))
        }
        else
          BddTrue
      }
    }

    if (fold == 1)
      locally {
        // This imports the TreeReducible instances.
        import treereduce.TreeReducible._
        // This imports the obj.treeMapReduce() syntax.
        import treereduce.TreeReduce._
        var n = 0
        (stateToVar,
          states.treeMapReduce(top)(getConstraints, { (acc: Bdd, bdd: Bdd) =>
            n = n + 1
            val answer = And(acc, bdd)
            val size = answer.size()
            consume(n, size)
            answer
          })
        )
      }
    else
      locally {
        var n = 0
        (stateToVar,
          states.map(getConstraints).foldLeft(top) { (acc: Bdd, bdd: Bdd) =>
            n = n + 1
            val answer = And(acc, bdd)
            val size = answer.size()
            consume(n, size)
            answer
          }
        )
      }
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

    def colorize(fold: Int, newSize: (Double, Double) => Unit, newTime: (Double, Double) => Unit): Map[String, String] = {
      Bdd.withNewBddHash {
        val time0 = nanoTime.toDouble
        val (colorization, bdd) = graphToBdd(List(start), uniGraph, biGraph, numNodes,
                                             (n: Double, size: Double) => {
                                               newSize(n, size)
                                               newTime(n, nanoTime() - time0)
                                             },
                                             // TODO remove this differentColor argument and auto-generate a C3 or C4
                                             //    within graphToBdd
                                             differentColor,
                                             fold = fold)

        bdd.findSatisfyingAssignment() match {
          case None => Map()
          case Some((assignTrue, assignFalse)) =>
            val ret = assignColors(colorization, assignTrue, assignFalse, colors)
            println(s"  $numNodes color assignment=" + ret)
            ret
        }
      }
    }

    var sizes1: List[(Double, Double)] = List()
    var times1: List[(Double, Double)] = List()
    colorize(1,
    { (n: Double, size: Double) => sizes1 = (n -> size) :: sizes1 },
    { (n: Double, time: Double) => times1 = (n -> time) :: times1 })
    var sizes2: List[(Double, Double)] = List()
    var times2: List[(Double, Double)] = List()
    cl.CLcompat.prog1(colorize(2,
    { (n: Double, size: Double) => sizes2 = (n -> size) :: sizes2 },
    { (n: Double, time: Double) => times2 = (n -> time) :: times2 }),

                      locally {
                        import gnuplot.GnuPlot._
                        gnuPlot(List(("Using treeMapReduce", sizes1.map(_._1), sizes1.map(_._2).map(_ / 1000.0)),
                                     ("Using foldLeft", sizes2.map(_._1), sizes2.map(_._2).map(_ / 1000.0))))(
                          title = "Allocation for Map 4-coloring",
                          xAxisLabel = "Step",
                          yAxisLabel = "Bdd size computed per step (K objects)",
                          yLog = true,
                          outputFileBaseName = "4-color-allocation")
                        gnuPlot(List(("Using treeMapReduce", times1.map(_._1), times1.map(_._2).map(_ / 1e9)),
                                     ("Using foldLeft", times2.map(_._1), times2.map(_._2).map(_ / 1e9))))(
                          title = "Time elapsed for Map 4-coloring",
                          xAxisLabel = "Step",
                          yAxisLabel = "Total time elapsed (sec)",
                          yLog = false,
                          outputFileBaseName = "4-color-time")
                      })
  }

  def mapColoringTest(numRegions:Int) = {
    import EuropeGraph._
    biGraphToDot(stateBiGraph, statePositions, s"europe-political-$numRegions")(symbols = symbols)
    val colors = colorizeMap(numRegions, "europe", "Greece",
                             removeStates(List("Russia"), stateUniGraph),
                             removeStates(List("Russia"), stateBiGraph),
                             List("Croatia", "Bosnia", "Serbia", "Montenegro"))
    biGraphToDot(stateBiGraph, statePositions, s"europe-political-$numRegions-colors"
                 )(symbols = symbols,
                   colors = { st => colors.getOrElse(st, "no-color") })
  }

  def main(argv: Array[String]): Unit = {
    for {numNodes <- (30 to 40)} {
      import java.lang.System.nanoTime

      val time0 = nanoTime
      println(s" === coloring $numNodes regions")
      mapColoringTest(numNodes)
      val now = nanoTime
      val elapsed = (now - time0 ) * 1e9
      println(s"Time to colorize $numNodes regions was $elapsed sec")
    }
  }
}
