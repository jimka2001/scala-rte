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


object DimacsBdd {
  import dimacs.QmVec.ClauseAsList

  // This imports the TreeReducible instances.
  import treereduce.TreeReducible._
  // This imports the obj.treeMapReduce() syntax.
  import treereduce.TreeReduce._

  def clauseToBdd(clause: ClauseAsList, ident: BddTerm, op: BinaryOperation): Bdd = {
    clause.treeMapReduce(ident: Bdd)(Bdd(_), (acc: Bdd, b: Bdd) => op(acc, b))
  }

  def cnfClauseToBdd(cnfClause: ClauseAsList): Bdd = clauseToBdd(cnfClause, BddFalse, Or)

  def dnfClauseToBdd(dnfClause: ClauseAsList): Bdd = clauseToBdd(dnfClause, BddTrue, And)

  // Interpret a list of Clauses as an And of Ors where each clause is a disjunction
  def cnfToBdd(cnf: IterableOnce[ClauseAsList]): Bdd = {
    cnf.treeMapReduce(BddTrue:Bdd)(cnfClauseToBdd, { (acc: Bdd, b: Bdd) => And(acc, b) })
  }

  def dnfToBdd(dnf: IterableOnce[ClauseAsList]): Bdd = {
    dnfToBdd2(dnf, ()=>())
  }
  // Interpret a list of Clauses as an Or of Ands where each clause is a conjunction
  def dnfToBdd2(dnf: IterableOnce[ClauseAsList], thunk: ()=>Unit): Bdd = {
    dnf.treeMapReduce(BddFalse:Bdd)(dnfClauseToBdd, { (acc: Bdd, b: Bdd) =>
      thunk()
      Or(acc, b)
    })
  }
}

object DimacsParallelReduce {
  import dimacs.QmVec.ClauseAsList

  // This imports the obj.treeMapReduce() syntax.
  import treereduce.TreeParallelReduce._

  def clauseToBdd(clause: ClauseAsList, ident: BddTerm, op: BinaryOperation): Bdd = {
    pairMapReduce(clause)(ident: Bdd,Bdd(_), (acc: Bdd, b: Bdd) => op(acc, b))
  }
  def dnfClauseToBdd(dnfClause: ClauseAsList): Bdd = clauseToBdd(dnfClause, BddTrue, And)

  // Interpret a list of Clauses as an Or of Ands where each clause is a conjunction
  def dnfToBdd(dnf: List[ClauseAsList]): Bdd = {
    pairMapReduce(dnf)(BddFalse:Bdd,dnfClauseToBdd, { (acc: Bdd, b: Bdd) => Or(acc, b) })
  }
}


object DimacsMixedReduce {
  import dimacs.QmVec.ClauseAsList

  // This imports the TreeReducible instances.
  import treereduce.TreeReducible._
  // This imports the obj.treeMapReduce() syntax.
  import treereduce.TreeReduce._

  def clauseToBdd(clause: ClauseAsList, ident: BddTerm, op: BinaryOperation): Bdd = {
    clause.treeMapReduce(ident: Bdd)(Bdd(_), (acc: Bdd, b: Bdd) => op(acc, b))
  }
  def dnfClauseToBdd(dnfClause: ClauseAsList): Bdd = clauseToBdd(dnfClause, BddTrue, And)

  // Interpret a list of Clauses as an Or of Ands where each clause is a conjunction
  def dnfToBdd(dnf: IterableOnce[ClauseAsList]): Bdd = {
    def x(acc:Bdd,next:ClauseAsList):Bdd = Or(acc,dnfClauseToBdd(next))
    dnf.iterator.foldLeft(BddFalse:Bdd)(x)
  }
}

object DimacsFold {
  import dimacs.QmVec.ClauseAsList

  def clauseToBdd(clause: ClauseAsList, ident: BddTerm, op: BinaryOperation): Bdd = {
    def x(b:Bdd,i:Int):Bdd = op(b,i)
    clause.foldLeft(ident: Bdd)( x)
  }
  def dnfClauseToBdd(dnfClause: ClauseAsList): Bdd = clauseToBdd(dnfClause, BddTrue, And)

  def dnfToBdd(dnf: IterableOnce[ClauseAsList]): Bdd = {
    dnfToBdd2(dnf,()=>())
  }
  // Interpret a list of Clauses as an Or of Ands where each clause is a conjunction
  def dnfToBdd2(dnf: IterableOnce[ClauseAsList],thunk:()=>Unit): Bdd = {
    def x(acc:Bdd,next:ClauseAsList):Bdd = {
      thunk()
      Or(acc,dnfClauseToBdd(next))
    }
    dnf.iterator.foldLeft(BddFalse:Bdd)(x)
  }
}

object ReducePerf {

  import gnuplot.GnuPlot.gnuPlot

  // returns the average amount of time of evaluating block
  // after evaluating block repetitions number of times.
  def time[R](repetitions: Int, name: String, block: => R): Double = {
    val t0 = System.nanoTime()
    (1 to repetitions).foreach { _ => block }
    val t1 = System.nanoTime()
    val elapsed = {
      (t1 - t0) / (repetitions * 1.0e6)
    }
    println(s"$name: Elapsed time: $elapsed ms")
    elapsed
  }

  val foldPairs = List(("         fold", bdd.DimacsFold.dnfToBdd _)
                       , ("pairMapReduce", bdd.DimacsParallelReduce.dnfToBdd _)
                       , ("treeMapReduce", bdd.DimacsBdd.dnfToBdd _)
                       //,("  mixedReduce", bdd.DimacsMixedReduce.dnfToBdd _)
                       )

  def testLimitedBddConstruction():Unit = {
    testLimitedBddConstruction(maxNumBits = 4,
                               numVars = 30,
                               maxNumTerms = 200,
                               numTermsStep = 3,
                               timeOutMinutes = 45)
  }
  def testLimitedBddConstruction(maxNumBits:Int,numVars:Int,maxNumTerms:Int,numTermsStep:Int,timeOutMinutes:Int): Unit = {
    require (maxNumTerms > 10)
    require (numTermsStep > 1)
    require (timeOutMinutes > 1)
    import bdd.BitFiddle.genDnfFromBitMask
    import bdd.Bdd.withNewBddHash

    var size = 0L
    var count = 0L
    val t0 = System.nanoTime() // nano-seconds
    val timeOutSeconds =  timeOutMinutes * 60
    def timeOutNotReached(timeOut:Int):Boolean = {
      val now = System.nanoTime()
      val elapsed = (now - t0) / 1.0e9 // time in seconds
      if (elapsed < timeOut)
        true
      else {
        println(s"Timeout reached: elapsed=$elapsed   timeOut=$timeOut")
        false
      }
    }

    type PLOT_DATA = (String, Double, Double, Double, Double)
    val rawDataForPlot: Iterator[PLOT_DATA] = for {
      numTerms <- (10 to maxNumTerms by numTermsStep).iterator.takeWhile(_ => timeOutNotReached(timeOutSeconds))
      (text, f) <- foldPairs
      dnf = genDnfFromBitMask(numVars, maxNumBits, numTerms, _ => true).iterator.toList
      ms: Double = time(3, s"maxNumBits=$maxNumBits num-terms=$numTerms algo=$text", {
        withNewBddHash {
          f(dnf)
          // (size,count) = bdd.Bdd.getBddSizeCount()
          size = bdd.Bdd.getBddSizeCount()._1
          count = bdd.Bdd.getBddSizeCount()._2
        }
      })
    } yield (text // _1
      ,numTerms.toDouble // _2
      ,ms              // _3
      ,size.toDouble  // _4
      ,count.toDouble // _5
    )

    val rawDataForPlotAsList = rawDataForPlot.toList

    for { (extract_y, label_y,baseName) <- List(((_:PLOT_DATA)._3,"Time (ms)","limited-bdd"),
                                                ((_:PLOT_DATA)._4,"Size","limited-bdd-size"),
                                                ((_:PLOT_DATA)._5,"Count","limited-bdd-count"),
                                                )}  {
      val dataForPlot = for {
        (text, tuples) <- rawDataForPlotAsList.groupBy(_._1).toList
      } yield (text, tuples.map(_._2), tuples.map(extract_y))

      gnuPlot(dataForPlot)(
              title=s"Fold Strategy $label_y Performance of n=$numVars bits=$maxNumBits",
              comment=s"Fold Strategy $label_y Performance of n=$numVars bits=$maxNumBits",
              xAxisLabel="Number of terms (density)",
              yAxisLabel=label_y,
              outputFileBaseName=baseName)
    }
  }

  def testNumBitsConstruction():Unit = {
    testNumBitsConstruction(numVars = 30,
                            maxNumTerms = 200,
                            numDnfs = 10,
                            maxNumLiteralsPerTerm = 7
                            )
  }

  def testGenSizePlotPerFold(numVars:Int,numTerms:Int,maxNumLiteralsPerTerm:Int): Unit = {
    import bdd.BitFiddle.genDnfFromBitMask
    import bdd.Bdd.withNewBddHash
    val foldPairs = List(("         fold", bdd.DimacsFold.dnfToBdd2 _)
                         , ("treeMapReduce", bdd.DimacsBdd.dnfToBdd2 _)
                         )
    type PLOT_DATA = (String, Int, Int, Long, Long, Long, Long)
    val rawDataForPlot:Seq[PLOT_DATA] = for {
      numLiteralsPerTerm <- 2 to maxNumLiteralsPerTerm
      dnf = genDnfFromBitMask(numVars, numLiteralsPerTerm, numTerms, _ => true).iterator.toList
      (text, f) <- foldPairs
      dataPoint <- locally{
        import adjuvant.Accumulators.withCollector
        import Bdd.getBddSizeCount
        withCollector[PLOT_DATA] { collect =>
          var iteration = 0
          var oldSize: Long = 0L
          var oldCount: Long = 0L
          withNewBddHash {
            f(dnf, () => {
              // generate a per-fold plot of size/count
              // we must generate (text, List[x], List[y])
              // at each step in the iteration get the size, delta-size, count, and delta-count
              //println(s"iteration=$iteration numLiteralsPerTerm=$numLiteralsPerTerm  text=$text")
              val (size, count) = getBddSizeCount()
              collect((text, numLiteralsPerTerm, iteration, size, oldSize - size, count, oldCount - count))
              iteration = iteration + 1
              oldCount = count
              oldSize = size

            })
          }
        }}
    } yield dataPoint

    for {(label,extract_y) <- List(("size",       (d:PLOT_DATA)=>d._4.toDouble),
                                   ("delta-size", (d:PLOT_DATA)=>d._5.toDouble),
                                   ("count",      (d:PLOT_DATA)=>d._6.toDouble),
                                   ("delta-count",(d:PLOT_DATA)=>d._7.toDouble))} {
      val dataForPlot = for {
        (numLiteralsPerTerm, tuples1) <- rawDataForPlot.groupBy(_._2).toList.sortBy(_._1)
        xs = tuples1.filter(_._1 == "treeMapReduce").map(_._3.toDouble).toList // iteration
        tree = tuples1.filter(_._1 == "treeMapReduce").map(extract_y)
        fold = tuples1.filter(_._1 == "         fold").map(extract_y)
        ratios = tree.zip(fold).map { case (tr, fo) => tr / fo }.toList
      } yield (s"$numLiteralsPerTerm", xs, ratios)

      gnuPlot(dataForPlot)(
        title = s"$label Ratio tree-fold / default-fold",
        comment = s"$label Ratio tree-fold / default-fold",
        grid = true,
        plotWith = "points",
        xAxisLabel = "Iteration",
        yAxisLabel = "Ratio for given termSize", yLog = true,
        outputFileBaseName = s"bdd-ratio-fold-iterations-$label")
    }
    for {(llabel,slabel,extract_y) <- List(("Memory Size", "size", (d:PLOT_DATA)=>d._4.toDouble),
                                    ("Allocation Count", "count",      (d:PLOT_DATA)=>d._6.toDouble),
                                    )} {
      val dataForPlot = for {
        (numLiteralsPerTerm, tuples1) <- rawDataForPlot.groupBy(_._2).toList.sortBy(_._1)
        xs = tuples1.filter(_._1 == "treeMapReduce").map(_._3.toDouble).toList // iteration
        tree = tuples1.filter(_._1 == "treeMapReduce").map(extract_y)
        fold = tuples1.filter(_._1 == "         fold").map(extract_y)
        data <- List((s"$numLiteralsPerTerm - tree-fold", xs, tree.toList),
                     (s"$numLiteralsPerTerm - fold-right", xs, fold.toList))
      } yield data

      gnuPlot(dataForPlot)(
        title = s"$llabel Ratio tree-fold / default-fold",
        comment = s"$llabel Ratio tree-fold / default-fold",
        xAxisLabel = "Iteration",
        yAxisLabel = "Ratio for given termSize", yLog = true,
        grid = true,
        plotWith = "points",
        outputFileBaseName = s"bdd-fold-iterations-$slabel")
    }
    for{ numLiteralsPerTerm <- 2 to maxNumLiteralsPerTerm} {
      for {(llabel, slabel, extract_y) <- List(("Cumulative Hash size", "size", (d: PLOT_DATA) => d._4.toDouble),
                                               ("Cumulative Object Count", "count", (d: PLOT_DATA) => d._6.toDouble),
                                      )} {
        val dataForPlot = for {
          (num, tuples1) <- rawDataForPlot.groupBy(_._2).toList.sortBy(_._1)
          if num == numLiteralsPerTerm
          xs = tuples1.filter(_._1 == "treeMapReduce").map(_._3.toDouble).toList // iteration
          tree = tuples1.filter(_._1 == "treeMapReduce").map(extract_y)
          fold = tuples1.filter(_._1 == "         fold").map(extract_y)
          data <- List((s"tree-fold", xs, tree.toList),
                       (s"fold-right", xs, fold.toList))
        } yield data

        gnuPlot(dataForPlot)(
          title = s"$numLiteralsPerTerm-Density $llabel",
          comment = s"$llabel Ratio tree-fold / default-fold",
          xAxisLabel = "Iteration",
          grid = true,
          plotWith = "points",
          yAxisLabel = "Number of objects allocated", yLog = true,
          outputFileBaseName = s"bdd-fold-iterations-$numLiteralsPerTerm-$slabel")
      }
    }
  }


  def testNumBitsConstruction(numVars:Int,maxNumTerms:Int,numDnfs:Int,maxNumLiteralsPerTerm:Int): Unit = {
    import bdd.BitFiddle.genDnfFromBitMask
    import bdd.Bdd.withNewBddHash
    val foldPairs = List(("         fold", bdd.DimacsFold.dnfToBdd _)
                         , ("treeMapReduce", bdd.DimacsBdd.dnfToBdd _)
                         )
    val runsPerDnf = 1
    val step =  4
    val rawDataForPlot = for {
      numLiteralsPerTerm <- 2 to maxNumLiteralsPerTerm
      dnfs = (1 to numDnfs).map(_ => genDnfFromBitMask(numVars, numLiteralsPerTerm, maxNumTerms, _ => true).iterator.toList)
      numTerms <- 10 to maxNumTerms by step
      (text, f) <- foldPairs
      truncatedDnfs = dnfs.map { dnf => dnf.take(numTerms) }
      ms: Double = time(runsPerDnf, s"numLiteralsPerTerm=$numLiteralsPerTerm numTerms=$numTerms $text", {
        truncatedDnfs.foreach{dnf=>
          withNewBddHash {
            f(dnf)
          }
        }}) / numDnfs

    } yield (text, numLiteralsPerTerm, numTerms.toDouble, ms)

    locally {
      val dataForPlot = for {
        (numLiteralsPerTerm, tuples1) <- rawDataForPlot.groupBy(_._2).toList.sortBy(_._1)
        xs = tuples1.filter(_._1 == "treeMapReduce").map(_._3).toList
        tree = tuples1.filter(_._1 == "treeMapReduce").map(_._4)
        fold = tuples1.filter(_._1 == "         fold").map(_._4)
        ratios = tree.zip(fold).map{case (tr,fo) => tr/fo}.toList
      } yield (s"$numLiteralsPerTerm", xs, ratios)

      gnuPlot(dataForPlot)(
        title=s"Ratio tree-fold / default-fold for n=$numVars",
        comment=s"Ratio tree-fold / default-fold for n=$numVars",
        xAxisLabel="Number of terms (density)",
        yAxisLabel="Ratio for given termSize", yLog=true,
        outputFileBaseName="bdd-ratio")
    }
  }

  def testRandomBddConstruction():Unit = {
    testRandomBddConstruction(maxNumVars=23)
  }

  def testRandomBddConstruction(maxNumVars:Int): Unit = {
    import bdd.BitFiddle.genRandomDnf
    import bdd.Bdd.withNewBddHash
    require(maxNumVars >= 2)
    val rawDataForPlot = for {
      n <- (3 to maxNumVars).iterator // 24 is too large
      dnfIt = genRandomDnf(n)
      (text, f) <- foldPairs
      _ = println(s"constructing BDD with n=$n $text")
      dnf = dnfIt.iterator.toList
      elapsed = time(2, s"n=$n   $text ", {
        withNewBddHash {
          f(dnf)
        }
      })
    } yield (text, n, elapsed)
    val dataForPlot: List[(String, List[Double], List[Double])] = for {
      _ <- List(1) // just to fool{}yield for into returning a list
      (text, data) <- rawDataForPlot.toList.groupBy(_._1)
      sorted = data.sortBy(_._2)
      xs = sorted.map {
        _._2.toDouble
      }
      ys = sorted.map {
        _._3
      }
    } yield (text, xs, ys)
    gnuPlot(dataForPlot)(
      title = "Fold Strategy Performance for Bdd Construction",
      comment = "Fold Strategy Performance for Bdd Construction",
      xAxisLabel = "Number of Boolean variables",
      yAxisLabel = "Time (ms)", yLog = true,
      outputFileBaseName = "bdd-construction"
      )
  }

  def main(argv: Array[String]): Unit = {
    //import treereduce.RationalFoldTest.rationalFoldTest

    //println("rationalFoldTest")
    //rationalFoldTest()
    println("testGenSizePlotPerFold")
    testGenSizePlotPerFold(30,200,7)
    println("testLimitedBddConstruction")
    testLimitedBddConstruction()
    println("testNumBitsConstruction")
    testNumBitsConstruction()
    println("testRandomBddConstruction")
    testRandomBddConstruction()
  }
}
