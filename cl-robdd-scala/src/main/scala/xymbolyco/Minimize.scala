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

  //trust accessible will be true for brzozowski constructions as it does not have unsatisfiable or unaccesible states
  def trim[Σ, L, E](dfa: Dfa[Σ, L, E], trustAccessible: Boolean = false): Dfa[Σ, L, E] = {
    if (!trustAccessible) {
      removeNonAccessible(removeNonCoAccessible(dfa))
    }
    else {
      removeNonCoAccessible(dfa)
    }
  }

  @tailrec
  def findReachable(succ: Map[Int, Set[Int]], done: Set[Int], todo: Set[Int]): Set[Int] = {
    if (todo.isEmpty)
      done
    else {
      val q = todo.head
      findReachable(succ, done + q, todo ++ (succ.getOrElse(q, Set()) diff done) - q)
    }
  }

  def removeNonAccessible[Σ, L, E](dfa: Dfa[Σ, L, E]): Dfa[Σ, L, E] = {
    val succ: Map[Int, Set[Int]] = dfa.successors()
    val accessible: Set[Int] = findReachable(succ, Set(), Set(dfa.q0.id))
    new Dfa(accessible,
      dfa.q0id,
      dfa.Fids.intersect(accessible),
      dfa.protoDelta.collect { case sld@(src, _, dst) if accessible.contains(src) && accessible.contains(dst) => sld },
      dfa.labeler,
      dfa.fMap
    )
  }

  def removeNonCoAccessible[Σ, L, E](dfa: Dfa[Σ, L, E]): Dfa[Σ, L, E] = {
    val pred: Map[Int, Set[Int]] = dfa.predecessors()
    val coaccessible: Set[Int] = findReachable(pred, Set[Int](), dfa.Fids)
    if (coaccessible.contains(dfa.q0id)) {
      new Dfa(coaccessible,
        dfa.q0id,
        dfa.Fids,
        dfa.protoDelta.collect { case sld@(src, _, dst) if coaccessible.contains(src) && coaccessible.contains(dst) => sld },
        dfa.labeler,
        dfa.fMap)
    }
    else {
      val q0id = dfa.q0id
      val allLabel = dfa.protoDelta.collect { case (src, lab, _) if src == q0id => lab }
      val protoDelta: Set[(Int, L, Int)] = if (allLabel.isEmpty)
        Set()
      else
        Set((q0id, allLabel.reduce((acc: L, lab: L) => dfa.labeler.combineLabels(acc, lab)), q0id))
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

  def complete[Σ, L, E](dfa: Dfa[Σ, L, E]): Dfa[Σ, L, E] = {
    val sinkId = dfa.Qids.maxOption match {
      case Some(m) => m + 1
      case _ => 0
    }
    val labeler = dfa.labeler
    val toSink = for {(src, triples) <- dfa.protoDelta.groupBy(_._1)
                      tds = triples.map(_._2).toSeq
                      newTd = labeler.subtractLabels(labeler.universe, tds)
                      if !labeler.inhabited(newTd).contains(false)
                      } yield (src, newTd, sinkId)

    val protoDelta = dfa.protoDelta ++
      toSink ++
      (if (toSink.nonEmpty) Set((sinkId, labeler.universe, sinkId)) else Set())

    val dfaComplete = new Dfa[Σ, L, E](Qids = dfa.Qids + sinkId,
      q0id = dfa.q0id,
      Fids = dfa.Fids,
      protoDelta = protoDelta,
      labeler = dfa.labeler,
      fMap = dfa.fMap)
    dfaComplete
  }

  def sxp[Σ, L, E](dfa1: Dfa[Σ, L, E],
                   dfa2: Dfa[Σ, L, E],
                   combineLabels: (L, L) => Option[L],
                   arbitrateFinal: (Boolean, Boolean) => Boolean,
                   combineFmap: (Option[E], Option[E]) => Option[E]): Dfa[Σ, L, E] = {

    val grouped1 = complete(dfa1).protoDelta.groupBy(_._1)
    val grouped2 = complete(dfa2).protoDelta.groupBy(_._1)

    def getEdges(pair: (Int, Int)): Seq[(L, (Int, Int))] = {
      val (q1id, q2id) = pair
      val x1 = grouped1.getOrElse(q1id, Set.empty)
      val x2 = grouped2.getOrElse(q2id, Set.empty)
      val edges = for {(_, lab1, dst1) <- x1
                       (_, lab2, dst2) <- x2
                       lab <- combineLabels(lab1, lab2)
                       } yield (lab, (dst1, dst2), (lab1, lab2))

      val edgeSeq = edges.toSeq
      assert(edgeSeq.map(_._1).distinct.size == edgeSeq.size,
        s"duplicate transitions found in " + edgeSeq.map(_.toString).mkString(": ", "\n: ", ""))
      edgeSeq.map { tr => (tr._1, tr._2) }
    }

    val (vertices, edges) = traceGraph[(Int, Int), L]((dfa1.q0id, dfa2.q0id),
      getEdges)
    assert(vertices(0) == (dfa1.q0id, dfa2.q0id))
    // assert( vertices(0) == (0,0))
    val fIds = vertices.indices.filter { q =>
      val (q1, q2) = vertices(q)
      arbitrateFinal(dfa1.Fids.contains(q1),
        dfa2.Fids.contains(q2))
    }

    val fMap = for {q <- fIds
                    (q1, q2) = vertices(q)
                    e <- combineFmap(dfa1.fMap.get(q1), dfa2.fMap.get(q2))
                    } yield q -> e
    val protoDelta = for {(seq, src) <- edges.zipWithIndex
                          (lab, dst) <- seq
                          } yield (src, lab, dst)
    val dfa = new Dfa[Σ, L, E](vertices.indices.toSet,
      0, // q0id:Int,
      fIds.toSet, // Fids:Set[Int],
      protoDelta.toSet, //    protoDelta:Set[(Int,L,Int)],
      dfa1.labeler, // labeler:Labeler[Σ,L],
      fMap.toMap // val fMap:Map[Int,E]
    )

    trim(dfa)
  }
}
