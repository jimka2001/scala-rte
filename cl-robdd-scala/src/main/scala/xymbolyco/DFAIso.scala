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
import genus.SimpleTypeD
import xymbolyco._
import adjuvant.BellmanFord._
import scala.Double.PositiveInfinity

object  DFAIso {
  // calculate the cost of each edge depending on if the label is inhabited or not
  def edgecost(size: Int, label: SimpleTypeD): Double = {
    if (label.inhabited.isEmpty) {
      size * 2
    }
    else if (label.inhabited.contains(false)) {
      PositiveInfinity
    }
    else {
      1
    }
  }

  //create set of exit values
  // create XOR of the dfas
  // check if there is a path to one of the final states for each exit value
  // return a map of exit value -> option(boolean)
  def isIsomorphic(dfa: Dfa[Any, SimpleTypeD, Int], dfa2: Dfa[Any, SimpleTypeD, Int]): (Option[Boolean], Map[Any, Option[Boolean]]) =
      {
        var resmap: Map[Any, Option[Boolean]] = Map()
        var res: Option[Boolean] = Some(true)
        val fvals: Set[Int] = dfa.fMap.toList.map(a => a._2).toSet
        val fvals2: Set[Int] = dfa2.fMap.toList.map(a => a._2).toSet
        val fint = fvals union fvals2
        for (i <- fvals) {
          resmap += (i -> Some(true))
        }
        val xor = Dfa.dfaXor(dfa, dfa2)
        val edges: Set[((Int, Int), Double)] = for {edge <- xor.protoDelta
                                                    newedge = ((edge._1, edge._3), edgecost(xor.Q.size, edge._2))
                                                    } yield newedge
        val xormap = Dijkstra(xor.Qids.toSeq, xor.q0id, edges.toSeq)
        val x: Map[Int, Double] = xormap._1
        for (i <- xor.fMap.keys) {
          if (x(i) < xor.Q.size.toDouble) {
            resmap += (xor.fMap(i) -> Some(false))
            res = Some(false)
          }
          else if (xormap._1(i) < PositiveInfinity) {
            if (resmap(xor.fMap(i)).contains(true)) {
              resmap += (i -> None)
              if (res.contains(true)) {
                res = None
              }
            }
          }
        }
        (res, resmap)
      }
}