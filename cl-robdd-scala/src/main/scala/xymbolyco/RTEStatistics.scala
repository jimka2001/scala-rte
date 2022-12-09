// Copyright (©) 2021 EPITA Research and Development Laboratory
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
// NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package xymbolyco

import genus.{SAtomic, SimpleTypeD}
import javax.management.openmbean.SimpleType
import rte._
import xymbolyco.Profiling.check
import adjuvant.GnuPlot._
import xymbolyco.Extract.dfaToRte

case class RTEStatistics(rte: Rte) {
//object countains a map of the transitions
  lazy val numberOfTransitions: Map[(Boolean, String, Boolean, String), Int] = createMap(rte)
//function that creates the map to be returned
  // in the for loop each of the DFAs are being built in turn and the transitions are being counted in a way that the DFAs do not need to be built twice.
  def createMap[Σ, L, E](rte_raw: Rte): Map[(Boolean, String, Boolean, String), Int] = {
    val list = for {(can, rte) <- Seq((false, rte_raw), (true, rte_raw.canonicalize))
                    (constr, dfa_tmp) <- Seq(("Thompson", Thompson.constructThompsonDfa(rte, true))
                                             , ("Brz", rte.toDfa()))
                    m <- Seq(false, true)
                    dfa: Dfa[Any, SimpleTypeD, Boolean] = if (m) Minimize.minimize(Minimize.trim(dfa_tmp)) else dfa_tmp
                    labels: List[SimpleTypeD] = dfa.protoDelta.toList.map(_._2)
                    grouped: Map[Option[Boolean], List[SimpleTypeD]] = labels.groupBy(lab => lab.inhabited)
                    (w, inh) <- Seq(("indeterminate", None),
                                    ("satisfiable", Some(true)),
                                    ("unsatisfiable", Some(false)))
                    } yield {
      if (grouped.contains(inh)) {
        ((can, constr, m, w), grouped(inh).size)
      } else ((can, constr, m, w), 0)
    }
    list.toMap
  }
}


object mystats {
  //creates a GNUPLOT for a given type of DFA ( brzmin,brz,thmp,thmp min) can also work with any function of the form
  // rte=> Int, the function has to be given into parameters, number of RTEs per depth and amount of depths
  def statisticSizePerDepth(number: Int = 30000, depth: Int = 5, f: Rte => Double = brz): Unit = {
    //sequence that will contain all curves
    var myseq: Seq[(String, Seq[(Double, Double)])] = Seq()
    for (j <- Range(0, depth)) {
      //using a map to count DFAs (creating couples of size->count, incrementing at each DFA
      val mymap: Map[Double, Double] = Map().withDefaultValue(0)
      val data = Range(0, number).foldLeft(mymap) { (acc, _) =>
        val fr = f(Rte.randomRte(depth + 1))
        acc + (fr -> (acc(fr) + 1))
      }
      //converting map to sequence of doubles, then adding the sequence of doubles to curve sequence
      val mytempseq: Seq[(Double, Double)] = data.toSeq.map(a => (a._1, (a._2 * 100) / number)).sorted
      myseq :+= ("Depth :".concat((j+1).toString), mytempseq)
    }
    //build graph with each curve
    gnuPlot(myseq)(Set("png"), "", "", "Number of States per Sigma-DFA",
                   false, "Proportion of sigma DFAs", false,
                   false, "DFAsizeperdepth", "linespoints",
                   "horizontal bmargin", _ => (), false, false)
  }

  def statisticSizePerDFA(num: Int = 30000, depth: Int = 5): Unit = {
    //sequence that will contain all the curves
    var myseq: Seq[(String, Seq[(Double, Double)])] = Seq()
    //seq of strings to name the curves
    val fnames = Seq("thompson", "thompson_min", "brzozowski", "brzozowski min")
    //seq of functions to create each curve
    val myfuncseq: Seq[Rte => Double] = Seq(thmp, thmpmin, brz, brzmin)
    for (i <- Range(0, 4)) {
      val f = myfuncseq(i)
      //builds map of double->double, rte, counts all values
      val mymap: Map[Double, Double] = Map().withDefaultValue(0)
      val data = Range(0, num * depth).foldLeft(mymap) { (acc, x) =>
        val fr = f(Rte.randomRte(x / num + 1))
        acc + (fr -> (acc(fr) + 1))
      }
      //adds curve to sequence of curves
      myseq :+= (fnames(i), data.toSeq.map(a => (a._1, (a._2 * 100) / num * depth)).sorted)
    }
    //creates gnuplot
    gnuPlot(myseq)(Set("png"), title = "", comment = "", xAxisLabel = "Number of States per Sigma-DFA",
                   xLog = true, yAxisLabel = "Proportion of sigma DFAs", yLog = true,
                   grid = false, outputFileBaseName = "DFAsizeperdepth", plotWith = "linespoints",
                   key = "horizontal bmargin", _ => (), verbose = false, view = false)
  }

  //functions to use as arguments for statisticsizeperdepth
  def brz(rte: Rte): Double = {
    rte.toDfa(42).Q.size
  }

  def brzmin(rte: Rte): Double = {
    Minimize.minimize(Minimize.trim(rte.toDfa(42))).Q.size
  }

  def thmp(rte: Rte): Double = {
    Thompson.constructThompsonDfa(rte, 42).Q.size
  }

  def thmpmin(rte: Rte): Double = {
    Minimize.minimize(Minimize.trim(Thompson.constructThompsonDfa(rte, 42))).Q.size
  }
  def statisticSizePerRndDFASize(number: Int = 30000,  minsize :Int = 5, maxsize:Int = 20, sizevar : Int = 5,typeDepth: Int = 1, f: Rte => Double = brz): Unit = {
    //sequence that will contain all curves
    val r = scala.util.Random
    val myseq: Seq[(String, Seq[(Double, Double)])] =
    for (i <- minsize/sizevar to (maxsize/sizevar)) yield {
      //using a map to count DFAs (creating couples of size->count, incrementing at each DFA
      val mymap: Map[Double, Double] = Map().withDefaultValue(0)
      val data = (0 until number).foldLeft(mymap) { (acc, x) =>
        val fins = r.nextInt(3)
        val dfa = RandomDFAGautier.RandomDFA(i * sizevar, fins, 2, 1, 42, typeDepth, None)
        val rte = dfaToRte(dfa, 42)(42)
        val fr = f(rte)
        acc + (fr -> (acc(fr) + 1))
      }
      //converting map to sequence of doubles, then adding the sequence of doubles to curve sequence
      (s"Random DFA Size : {(i * sizevar)}", data.toSeq.map(a => (a._1, (a._2 * 100) / number)).sorted)
    }
    //build graph with each curve
    gnuPlot(myseq)(Set("png"), "", "", "Number of States per Sigma-DFA from a random DFA",
                   false, "Proportion of sigma DFAs", false,
                   false, "DFAsizeperdepth", "linespoints",
                   "horizontal bmargin", _ => (), false, false)
  }
  def statisticSizePerDFAfromRandomDFA(num: Int = 30000, minsize :Int = 5, maxsize:Int = 20, sizevar : Int = 5 ): Unit = {
    //sequence that will contain all the curves
    val now = System.nanoTime()
    val r = scala.util.Random
    var myseq: Seq[(String, Seq[(Double, Double)])] = Seq()
    //seq of strings to name the curves
    val fnames = Seq("thompson", "thompson_min", "brzozowski", "brzozowski min")
    //seq of functions to create each curve
    val myfuncseq: Seq[Rte => Double] = Seq(thmp, thmpmin, brz, brzmin)
    for (i <- Range(0, 4)) {
      val f = myfuncseq(i)
      //builds map of double->double, rte, counts all values
      val mymap: Map[Double, Double] = Map().withDefaultValue(0)
      val data = Range(0, (num * (maxsize - minsize)) / sizevar).foldLeft(mymap) { (acc, x) =>
        val fins = r.nextInt(3)
        val dfa = RandomDFAGautier.RandomDFA(((x / num) * sizevar) + minsize, fins, 2, 1, 42, 1, None)
        val rte = dfaToRte(dfa,42)(42)
        val fr = f(rte)
        acc + (fr -> (acc(fr) + 1))
      }
      //adds curve to sequence of curves
      myseq :+= (fnames(i), data.toSeq.map(a => (a._1, (a._2 * 100) / num * ((maxsize - minsize) / sizevar))).sorted)
    }
    //creates gnuplot
    gnuPlot(myseq)(Set("png"), title = "", comment = "", xAxisLabel = "Number of States per Sigma-DFA from a Random DFA",
                   xLog = true, yAxisLabel = "Proportion of sigma DFAs", yLog = true,
                   grid = false, outputFileBaseName = "DFAsizeperdepth", plotWith = "linespoints",
                   key = "horizontal bmargin", _ => (), verbose = false, view = false)
  }

  def statisticSizePerF(number: Int = 30000, minsize: Int = 5, maxsize: Int = 20, sizevar: Int = 5, frte: Int => Rte, f: Rte => Double = brz): Unit = {
    //sequence that will contain all curves
    val r = scala.util.Random
    val myseq: Seq[(String, Seq[(Double, Double)])] =
      for (i <- minsize / sizevar to (maxsize / sizevar)) yield {
        //using a map to count DFAs (creating couples of size->count, incrementing at each DFA
        val mymap: Map[Double, Double] = Map().withDefaultValue(0)
        val data = (0 until number).foldLeft(mymap) { (acc, _) =>
          val fins = r.nextInt(3)
          val fr = f(frte(i))
          acc + (fr -> (acc(fr) + 1))
        }
        //converting map to sequence of doubles, then adding the sequence of doubles to curve sequence
        (s"Random DFA Size : {(i * sizevar)}", data.toSeq.map(a => (a._1, (a._2 * 100) / number)).sorted)
      }
    //build graph with each curve
    gnuPlot(myseq)(Set("png"), "", "", "Number of States per Sigma-DFA from a random DFA",
                   false, "Proportion of sigma DFAs", false,
                   false, "DFAsizeperdepth", "linespoints",
                   "horizontal bmargin", _ => (), false, false)
  }

  def statisticSizePerDFAfromF(num: Int = 30000, minsize: Int = 5, maxsize: Int = 20, sizevar: Int = 5, frte: Int => Rte): Unit = {
    //sequence that will contain all the curves
    val now = System.nanoTime()
    val r = scala.util.Random
    var myseq: Seq[(String, Seq[(Double, Double)])] = Seq()
    //seq of strings to name the curves
    val fnames = Seq("thompson", "thompson_min", "brzozowski", "brzozowski min")
    //seq of functions to create each curve
    val myfuncseq: Seq[Rte => Double] = Seq(thmp, thmpmin, brz, brzmin)
    for (i <- Range(0, 4)) {
      val f = myfuncseq(i)
      //builds map of double->double, rte, counts all values
      val mymap: Map[Double, Double] = Map().withDefaultValue(0)
      val data = Range(0, (num * (maxsize - minsize)) / sizevar).foldLeft(mymap) { (acc, x) =>
        val fins = r.nextInt(3)
        val rte = frte(((x / num) * sizevar) + minsize)
        val fr = f(rte)
        acc + (fr -> (acc(fr) + 1))
      }
      //adds curve to sequence of curves
      myseq :+= (fnames(i), data.toSeq.map(a => (a._1, (a._2 * 100) / num * ((maxsize - minsize) / sizevar))).sorted)
    }
    //creates gnuplot
    gnuPlot(myseq)(Set("png"), title = "", comment = "", xAxisLabel = "Number of States per Sigma-DFA from a Random DFA",
                   xLog = true, yAxisLabel = "Proportion of sigma DFAs", yLog = true,
                   grid = false, outputFileBaseName = "DFAsizeperdepth", plotWith = "linespoints",
                   key = "horizontal bmargin", _ => (), verbose = false, view = false)
  }




  def main(argv: Array[String]):Unit=
  {
    mystats.statisticSizePerRndDFASize(4,5,10,5,1,mystats.brz)
  }
}