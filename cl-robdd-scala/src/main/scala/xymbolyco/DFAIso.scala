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
import rte.Rte

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
  def isIsomorphic[E](dfa: Dfa[Any, SimpleTypeD, E], dfa2: Dfa[Any, SimpleTypeD, E]): (Option[Boolean], Map[E, Option[Boolean]]) =
      {
        //creating a set of all exit values ( from both dfas)
        val fvals: Set[E] = dfa.fMap.toList.map(a => a._2).toSet union dfa2.fMap.toList.map(a => a._2).toSet
        // creating the xor of the two dfas
        val xor = Dfa.dfaXor(dfa, dfa2)
        //calculating the value of each edge for Bellman Ford or Dijkstra
        val edges: Set[((Int, Int), Double)] = for {edge <- xor.protoDelta
                                                    newedge = ((edge._1, edge._3), edgecost(xor.Q.size, edge._2))
                                                    } yield newedge
        //creating a Map of vertices to their "cost" and a Map of vertices to their parent with the lowest cost
        // the function can be replaced by any other function that does the same job, Dijkstra/ Bellman-Ford
        val xormap = Dijkstra(xor.Qids.toSeq, xor.q0id, edges.toSeq)
        //creating a map to determine wether each state is reachable or not
        // ( by transitions being satisfiable, undeterministic or unsatisfiable)
        val reachableMap: Map[Int, Option[Boolean]] = xor.fMap.map(x => if (xormap._1(x._1) < xor.Q.size.toDouble) x._1 -> Some(false)
        else if (xormap._1(x._1) < PositiveInfinity) x._1 -> None
        else x._1 -> Some(true))
        // creates a map that associates exit values to the reachability of all their final states in the XOR DFA
        val SatisfiableExitValues: Map[E, Set[Option[Boolean]]] = reachableMap.groupBy(x => xor.fMap(x._1)).
          map(a => (a._1, a._2.map(b => b._2).toSet)).withDefaultValue(Set(Some(true)))
        //creates the final map, for each exit value, if one of their final states is reachable, or undeterministic
        // the values in the map will change accordingly ( Some(false) means that an exit state is reachable and that the
        // exit value is not isomorphic for both DFAs, None means that it is undeterministic, due to an undeterministic transition
        // true means that either the final states are unreachable, or that there is no final state.
        val resmap: Map[E, Option[Boolean]] = fvals.map(a => if (SatisfiableExitValues(a).contains(Some(false))) a -> Some(false)
        else if (SatisfiableExitValues(a).contains(None)) a -> None else a -> Some(true)).toMap
        //determining if the two dfas are isomorphic for all exit values
        val resSet = resmap.map(a => a._2).toSet
        var res: Option[Boolean] = Some(true)
        if (resSet.contains(Some(false))) {
          res = Some(false)
        }
        else if (resSet.contains(None)) {
          res = None
        }
        //return both
        (res, resmap)
      }
}