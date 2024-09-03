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

// Seq[Σ] is the type of the input sequence which the DFA is expected to match
// L is the type of label on transitions
// E is the type of exit values
class Dfa[Σ,L,E](val Qids:Set[Int],
                 val q0id:Int,
                 val Fids:Set[Int],
                 val protoDelta:Set[(Int,L,Int)],
                 val labeler:Labeler[Σ,L],
                 val fMap:Map[Int,E]) {
  // q0id is in Qids
  require(Qids.contains(q0id))
  // each element of Fids is in Qids
  require(Fids.subsetOf(Qids))
  // each triple in protoDelta is of the form (x,_,y) where x and y are elements of Qids
  require(protoDelta.forall  { case (from: Int, _, to: Int) => Qids.contains(from) && Qids.contains(to) })

  // returns a map of src -> Set(dst)
  // tests if the transitions are valid, if the transitions carry a type that is empty
  // the following state will not be added to the queue
  def successors():Map[Int,Set[Int]] = {
    for{ (src,triples) <- protoDelta.groupBy{case (src,_,_) => src}
      triples2= triples.filter{case (_,tr,_) => !labeler.inhabited(tr).contains(false)}
         } yield src -> triples2.map(_._3)
  }

  // returns a map of dst -> Set(src)
  def predecessors():Map[Int,Set[Int]] = {
    for{ (dst,triples) <- protoDelta.groupBy{case (_,_,dst) => dst}
         } yield dst -> triples.map(_._1)
  }

  def exitValue(q: State[Σ, L, E]): E = fMap(q.id)

  def idToState(id: Int): State[Σ, L, E] = {
    Q.find(s => s.id == id).get
  }

  def delta(s: State[Σ, L, E], label: L): State[Σ, L, E] = {
    s.transitions.find(tr => labeler.equivLabels(tr.label, label)).get.destination
  }

  val Q: Set[State[Σ, L, E]] = Qids.map((id: Int) => new State[Σ, L, E](dfa = this, id))
  for {
    (from, fromTriples) <- protoDelta.groupBy(_._1)
    fromState = idToState(from)
    (to, toFromTriples) <- fromTriples.groupBy(_._3)
    toState = idToState(to)
    label = toFromTriples.map(_._2).reduce(labeler.combineLabels)
  } fromState.transitions += Transition(fromState, label, toState)

  val q0: State[Σ, L, E] = idToState(q0id)
  val F: Set[State[Σ, L, E]] = Fids.map(idToState)

  override def toString: String = Serialize.serializeToString(dfa = this)

  def findReachableFinal(seq: IterableOnce[Σ], verbose:Boolean=false): Option[State[Σ, L, E]] = {
    val init: Option[State[Σ, L, E]] = Some(q0)
    seq.iterator.foldLeft(init) { (mq, s) =>
      if(verbose)
        println(s"mq=$mq s=$s [${s.getClass()}] --> ${mq.flatMap(_.successor(s))}")
      mq.flatMap(_.successor(s))
    }
  }

  // Take a path which is a list of states (assumed to be states
  //   that follow some computation path from q0 to some final
  //   state), and return a list of labels corresponding to the
  //   transitions from one state to the next along the path.
  //   The list of labels is length 1 fewer than the list of states.
  // If there are multiple transitions possible between two consecutive
  //   states, then their labels are combined with labeler.combineLabels.
  // If pathToLabels is called with a list of states which are not all consecutive,
  //   an exception will be thrown.
  def pathToLabels(path:Path):List[L] = {
      path.tails.flatMap {
        case q1 :: q2 :: _ =>
          // filter all transitions from q1 to q2, accumulate their labels
          val labels = for { Transition(_, label, dst) <- q1.transitions
                             if dst == q2
                             } yield label
          assert(labels.nonEmpty, "expecting at least one transition between $q1 and $q2")
          List(labels.reduce(labeler.combineLabels))
        case _ => List()
      }.toList
  }

  // find some sequence of labels of objects leading
  // from q0 to a final state.
  //   if satisfiable = Seq(Some(true)), then we only traverse transitions which are
  //      definitely satisfiable, i.e., the labeler.inhabited(label) function returns Some(true)
  //          as opposed to None for dont-know whether it is satisfiable
  //   if satisfiable = Seq(Some(true),None), then we also traverse transitions for which
  //          we get None from label.inhabited(label) meaning that we don't know for sure
  //          that the label is satisfiable.
  def findTrace(requireSatisfiable:Boolean=true):Option[List[L]] = {
    val maybePath:Option[Path] = spanningPath match {
      case None => None
      case Some(Right(path)) => Some(path)
      case Some(Left(path)) if !requireSatisfiable => Some(path)
      case _ => None
    }

    maybePath.map(path => pathToLabels(path))
  }

  type Path = List[State[Σ, L, E]]
  type MaybePath = Option[Either[Path, Path]]
  lazy val spanningPath: MaybePath = findSpanningPath()

  // spanningTrace is None if there is no spanning path
  //      Some(Right(List[L])) if there is a satisfiable path, List[L] are the labels
  //      Some(Left(List[L])) if there is a path, but some label is indeterminate.
  lazy val spanningTrace:Option[Either[List[L],List[L]]] = spanningPath match {
    case None => None
    case Some(Right(_)) => Some(Right(findTrace(true).getOrElse(List[L]())))
    case Some(Left(_)) => Some(Left(findTrace(false).getOrElse(List[L]())))
  }

  // Compute a Map from exit-value to (Path,Option[Boolean])) which denotes the shortest path
  //   from q0 to a final state with that exit value.
  //   if the Option[Boolean] is Some(true) then the path passes through only satisfiable
  //       transitions, i.e., transitions for which the label is satisfiable
  //   If the Option[Boolean] is None, then the path passes through at least one
  //       indeterminate transition, i.e., the label.inhabited returned None, dont-know.
  def findSpanningPathMap():Map[E,(Option[Boolean],Path)] = {
    import adjuvant.BellmanFord.{shortestPath,reconstructPath}
    import scala.Double.PositiveInfinity
    val numStates:Double = 2 * Q.size.toDouble
    val states = Q.toSeq
    // We use the Bellman Ford or Dijkstra shortest path algorithm to find the shortest
    //    path from q0 to each final state.  We do this by associating weights
    //    to each edge.  A satisfiable transition, has weight=1.  A uninhabited
    //    transition has weight = infinity.
    //    An indeterminate state has weight = 2*number-of-states.  Why?  Because
    //    if there is a path going only though satisfiable transitions, then it
    //    at most uses every state, thus has a length of num-states - 1.
    //    This means the graph search will always prefer satisfiable paths to
    //    indeterminate paths.
    val edge_weights: Seq[((State[Σ, L, E], State[Σ, L, E]), Double)] =
      for{q<-states
          Transition(src,lab,dst) <- q.transitions
          edge <- labeler.inhabited(lab) match {
            case None => Some(((src,dst),numStates)) // indeterminate
            case Some(true) => Some(((src,dst),1.0)) // satisfiable
            case Some(false) => None // non-satisfiable
          }} yield edge
    val edge_weights_map = edge_weights.toMap

    def path_weight(path:Path):Double = {
      path match {
        case List() => 0.0
        case _::Nil => 0.0
        case st1::st2::states => edge_weights_map.getOrElse((st1,st2), PositiveInfinity) + path_weight(st2::states)
      }
    }
    val (d,p) = shortestPath(states, q0, edge_weights) // Bellman Ford or Dikjstra
    // returns a pair (which will contribute to a Map entry)
    //   which maps a state id to a pair of (Option[Boolean], Path)
    //   where the path is a list of consecutive states from q0 to the state in question.
    //   if no such path exists, then the Option[Boolean] is Some(false) and the path is List()
    //   if Option[Boolean] is Some(true) then the path passes through no indeterminate states
    //   if Option[Boolean] is None then the path passes through at least one indeterminate state
    def maybePath(q:State[Σ, L, E]):(Int,(Option[Boolean],Path)) = {
      (q.id,
        if (d(q) < numStates) // satisfiable path
          (Some(true), reconstructPath[State[Σ, L, E]](p, q))
        else if (d(q) == PositiveInfinity) // not-satisfiable
          (Some(false),List())
        else
          (None, reconstructPath(p, q)) // non determinate
      )
    }

    // m is the map from final state to shortest path from q0 to that state
    val m:Map[Int,(Option[Boolean],Path)] = F.map(maybePath).toMap

    def bestPath(pairs:Seq[(Option[Boolean],Path)]):(Option[Boolean],Path) = {
      pairs.fold((Some(false),List())) { (acc, path) =>
        (acc, path) match {
          case ((ob, path1), (_, path2)) if path_weight(path1) < path_weight(path2) => (ob, path1)
          case (_, (ob, path2)) => (ob, path2)
        }
      }
    }

    // m maps final state to shortest path, we need to map exit-value
    // to shortest path.  In the case that 2 (or more) final states
    // have the same exit value, we need to select the best path of
    // the paths for those final states.
    val x: Map[E, (Option[Boolean], Path)] = for {(e,fs) <- fMap.groupMap(_._2)(_._1)
                                                  best = bestPath(fs.map(m).toSeq)
                                                  } yield e -> best
    x
  }

  // Try to identify a sequence of States which lead from q0 to a
  // final state.  There are several possible things that can happen.
  //   1) there is no path from q0 to a final state,
  //        return None
  //   2) there is such path for which all transitions are satisfiable
  //        return Right(path)
  //   3) there is such a path, but it passes through at least one
  //        semi-satisfiable transition
  //        return Left(path)
  //   4) there is a path, but it passes through a transition which
  //        is not satisfiable
  //        return None
  // This method is final because it is expensive to call.
  //   it is already called lazily in the lazy var spanningPath,
  private def findSpanningPath():MaybePath = {
    val m: Map[E, (Option[Boolean], Path)] = findSpanningPathMap()

    lazy val foundTrue = m.collectFirst{case (_,(Some(true),path)) => Right(path)}
    lazy val foundNone = m.collectFirst{case (_,(None,path)) => Left(path)}

    foundTrue.orElse(foundNone)
  }

  def simulate(seq: IterableOnce[Σ], verbose:Boolean=false): Option[E] = {
    for {
      d <- findReachableFinal(seq, verbose=verbose)
      if F.contains(d)
    } yield exitValue(d)
  }

  def vacuous():Option[Boolean] = {
    if (Q.isEmpty)
      Some(true)
    else if (F.isEmpty)
      Some(true)
    else spanningPath match {
      case None => Some(true)
      case Some(Right(_)) => Some(false)
      case Some(Left (_)) => None
    }
  }

  def findSinkStateIds():Set[Int] = {
    for{ q <- Q
         if q.isSinkState
         } yield q.id
  }
}

object Dfa {

  def mergeParallel[Σ, L](labeler:Labeler[Σ, L],
                          transitions: Seq[(Int, L, Int)]): Set[(Int, L, Int)] = {
    val merged = for {(src, triples1) <- transitions.groupBy(_._1)
                      (dst, triples2) <- triples1.groupBy(_._3)
                      label = triples2.map(_._2).reduce(labeler.combineLabels)
                      label2 = if (labeler.universal(label)) labeler.universe else label
                      } yield (src, label2, dst)

    merged.toSet
  }

  def apply[Σ, L, E](Qids: Set[Int],
                     q0id: Int,
                     Fids: Set[Int],
                     protoDelta: Set[(Int, L, Int)],
                     labeler: Labeler[Σ, L],
                     fMap: Map[Int, E]): Dfa[Σ, L, E] = {
    val merged = mergeParallel[Σ, L](labeler,protoDelta.toSeq)
    new Dfa[Σ, L, E](Qids, q0id, Fids, merged, labeler, fMap)
  }

  def combineFmap[E](e1: Option[E],
                     e2: Option[E],
                     arbitrate:(E,E)=>E = defaultArbitrate[E] _): Option[E] = {
    (e1, e2) match {
      case (None, None) => None
      case (Some(b), Some(c)) if c == b => Some(b)
      case (Some(b), Some(c)) => Some(arbitrate(b,c))
      case (Some(b), None) => Some(b)
      case (None, Some(b)) => Some(b)
    }
  }

  def defaultArbitrate[E](b:E, c:E):E = {
    println(s"combineFmap: warning loosing value $c, using $b")
    b // f-value of dfa1 has precedence over dfa2
  }

  def dfaXor[Σ,L,E](dfa1: xymbolyco.Dfa[Σ, L, E],
                    dfa2: xymbolyco.Dfa[Σ, L, E],
                    arbitrate: (E,E)=>E = defaultArbitrate[E] _): xymbolyco.Dfa[Σ, L, E] = {
    Minimize.sxp[Σ, L, E](dfa1, dfa2,
                          (a: Boolean, b: Boolean) => (a && !b) || (!a && b), // arbitrateFinal:(Boolean,Boolean)=>Boolean,
                          combineFmap(_,_,arbitrate)
                          )
  }

  def dfaUnion[Σ,L,E](dfa1: xymbolyco.Dfa[Σ, L, E],
                      dfa2: xymbolyco.Dfa[Σ, L, E],
                      arbitrate: (E,E)=>E = defaultArbitrate[E] _): xymbolyco.Dfa[Σ, L, E] = {
    Minimize.sxp[Σ, L, E](dfa1, dfa2,
                          (a: Boolean, b: Boolean) => a || b, // arbitrateFinal:(Boolean,Boolean)=>Boolean,
                          combineFmap(_,_,arbitrate)
                          )
  }

  // returns Some(true), Some(false), or None
  // Some(true) => the Dfas are provably equivalent, i.e., they both accept the
  //   same language
  // Some(false) => The Dfas are provably not equivalent.
  // None => It cannot be proven whether the Dfas are equivalent.  For example
  //   because it contains a transition which is not known to be inhabited.
  def dfaEquivalent[Σ,L,E](dfa1: xymbolyco.Dfa[Σ, L, E],
                           dfa2: xymbolyco.Dfa[Σ, L, E]): Option[Boolean] = {
    dfaXor(dfa1, dfa2).vacuous()
  }
}

object TestMe {
  import rte.{Or, Cat, Singleton, Not, And, Sigma, Star, EmptySet}
  import genus.SEql
  class TestAc

  //noinspection RedundantDefaultArgument
  def main(argv:Array[String]):Unit = {
    // val Test2 = classOf[TestAc]
    val Σ = Sigma
    // val ε = EmptyWord
    val ∅ = EmptySet
    val e = SEql(1)
    val rt1 = Cat(Star(Σ), Star(Singleton(e)))
    val rt2 = Cat(Not(∅), Star(Singleton(e)))
    val empty= Or(And(rt1,Not(rt2)),And(Not(rt1),rt2))

    GraphViz.dfaView(rt1.toDfa(),abbrev=true,title="rt1")

    GraphViz.dfaView(rt2.toDfa(),abbrev=false,title="rt2")
    GraphViz.dfaView(empty.toDfa(),abbrev=true,title="empty")
  }
}
