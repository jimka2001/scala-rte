// Copyright (c) 2019,20,21 EPITA Research and Development Laboratory
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

package xymbolyco

import scala.annotation.tailrec

object Minimize {

  import adjuvant.Adjuvant._

  def trim[Σ, L, E](dfa: Dfa[Σ, L, E]): Dfa[Σ, L, E] = {
    removeNonAccessible(removeNonCoAccessible(dfa))
  }

  def findReachable(succ:Map[Int,Set[Int]], done:Set[Int],todo:Set[Int]):Set[Int] = {
    if (todo.isEmpty)
      done
    else {
      val q = todo.head
      findReachable(succ,done + q, todo ++ (succ(q) diff done) - q)
    }
  }

  def removeNonAccessible[Σ, L, E](dfa: Dfa[Σ, L, E]): Dfa[Σ, L, E] = {
    val succ:Map[Int,Set[Int]] = dfa.successors()
    val accessible:Set[Int] = findReachable(succ,Set(),Set(dfa.q0.id))
    new Dfa(accessible,
            dfa.q0id,
            dfa.Fids.intersect(accessible),
            dfa.protoDelta.collect{case sld@(src,_,dst) if accessible.contains(src) && accessible.contains(dst) =>sld},
            dfa.labeler,
            dfa.fMap
            )
  }

  def removeNonCoAccessible[Σ, L, E](dfa: Dfa[Σ, L, E]): Dfa[Σ, L, E] = {
    val pred:Map[Int,Set[Int]] = dfa.successors()
    val coaccessible:Set[Int] = findReachable(pred,Set[Int](),dfa.Fids)
    if(coaccessible.contains(dfa.q0id)){
      new Dfa(coaccessible,
              dfa.q0id,
              dfa.Fids,
              dfa.protoDelta.collect{case sld@(src,_,dst) if coaccessible.contains(src) && coaccessible.contains(dst) =>sld},
              dfa.labeler,
              dfa.fMap)
    }
    else
    {
      val q0id = dfa.q0id
      val allLabel = dfa.protoDelta.collect{case (src,lab,_) if src == q0id => lab}
      val protoDelta:Set[(Int,L,Int)] = if (allLabel.isEmpty)
        Set()
      else
        Set((q0id,allLabel.reduce((acc:L,lab:L)=> dfa.labeler.combineLabels(acc,lab)),q0id))
      // make trivial dfa with only a self-loop labeled Sigma on q0
      new Dfa(Set(q0id),
              q0id,
              Set(),
              protoDelta,
              dfa.labeler,
              dfa.fMap
              )
    }
  }

  def findHopcroftPartition[Σ, L, E](dfa: Dfa[Σ, L, E]): Set[Set[State[Σ, L, E]]] = {
    type STATE = State[Σ, L, E]
    type EQVCLASS = Set[STATE]
    type PARTITION = Set[EQVCLASS]

    val Pi0 = partitionBy(dfa.F, dfa.exitValue) + dfa.Q.diff(dfa.F)

    def partitionEqual(pi1: PARTITION, pi2: PARTITION): Boolean =
      pi1 == pi2

    def refine(partition: PARTITION): PARTITION = {
      def phi(source: STATE, label: L): EQVCLASS = {
        // find the element of partition, itself an equivalence class, which
        //   which contains the destination state of the transition
        //   whose source and label are given
        partition.find(_.contains(source.delta(label))).get
      }

      def PhiPrime(s: STATE): Set[(L, EQVCLASS)] = {
        for {Transition(_, label, _) <- s.transitions}
          yield (label, phi(s, label))
      }

      def Phi(s: STATE): Set[(L, EQVCLASS)] = {
        val m = for {(k, pairs) <- PhiPrime(s).groupBy(_._2)
                     labels = pairs.map(_._1)
                     label = labels.reduce(dfa.labeler.combineLabels)}
        yield (label, k)
        m.toSet
      }

      def repartition(eqvClass: EQVCLASS): PARTITION = {
        partitionBy(eqvClass, Phi)
      }

      partition.flatMap(repartition)
    }

    fixedPoint(Pi0, refine, partitionEqual)
  }

  def minimize[Σ, L, E](dfa: Dfa[Σ, L, E]): Dfa[Σ, L, E] = {
    val PiMinimized = findHopcroftPartition(dfa)

    // associate each element of PiMinimized with a new id
    // attempt to correlate the old state.id with the new
    def minState(eqvClass: Set[State[Σ, L, E]]): Int = {
      eqvClass.map(_.id).reduce((a: Int, b: Int) => a.min(b))
    }

    val ids: Map[Set[State[Σ, L, E]], Int] = PiMinimized.map { eqvClass => eqvClass -> minState(eqvClass) }.toMap

    def eqvClassOf(s: State[Σ, L, E]): Set[State[Σ, L, E]] = {
      PiMinimized.find(eqvClass => eqvClass.contains(s)).get
    }

    def newId(s: State[Σ, L, E]): Int = {
      ids(eqvClassOf(s))
    }

    //val ids: Array[Set[State[Σ,L,E]]] = PiMinimized.toArray
    val newIds = ids.values.toSet
    val newQ0: Int = newId(dfa.q0)
    val newFids = PiMinimized.filter(eqv => dfa.F.exists(q => eqv.contains(q))).map(ids)

    val newProtoDelta = for {
      q <- dfa.Q
      tr <- q.transitions
    } yield (newId(tr.source), tr.label, newId(tr.destination))

    val newFmap = for {
      (eqv, id) <- ids
      if newFids.contains(id)
      q = eqv.head
    } yield id -> dfa.exitValue(q)

    // return a newly constructed Dfa extracted from the Hopcroft partition minimization
    new Dfa[Σ, L, E](newIds, newQ0, newFids, newProtoDelta, dfa.labeler, newFmap)
  }
}
