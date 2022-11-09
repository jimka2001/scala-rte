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
  require(protoDelta.forall { case (from: Int, _, to: Int) => Qids.contains(from) && Qids.contains(to) })

  // returns a Map of src -> Set(dst)
  // The Map collects the destinations per origin, omitting those
  //   only reachable by un-satisfiable transitions.
  def successors():Map[Int,Set[Int]] = {
    for{ (src,triples) <- protoDelta.groupBy{case (src,_,_) => src}
         satisfiable = triples.filter { case (_, lab, _) => !labeler.inhabited(lab).contains(false) }
         } yield src -> satisfiable.map(_._3)
  }

  // returns a map of dst -> Set(src)
  // The Map collects the origins per destination, omitting those
  //   only reachable by un-satisfiable transitions.
  def predecessors():Map[Int,Set[Int]] = {
    for{ (dst,triples) <- protoDelta.groupBy{case (_,_,dst) => dst}
         satisfiable = triples.filter { case (_, lab, _) => !labeler.inhabited(lab).contains(false) }
         } yield dst -> satisfiable.map(_._1)
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

  def findReachableFinal(seq: Seq[Σ]): Option[State[Σ, L, E]] = {
    val init: Option[State[Σ, L, E]] = Some(q0)
    seq.foldLeft(init) { (mq, s) =>
      mq.flatMap(_.successor(s))
    }
  }

  // take a path which is a list of states (assumed to be states
  //   that follow some computation path from q0 to some final
  //   state), and return a list of labels corresponding to the
  //   transitions from one state to the next along the path.
  //   The list of labels is length 1 fewer than the list of states.
  def pathToLabels(path:Path):List[L] = {
      path.tails.flatMap {
        case q1 :: q2 :: _ =>
          val Some(Transition(_, label, _)) = q1.transitions.find {
            case Transition(_, _, dst)
              if dst == q2  => true
            case _ => false
          }
          List(label)
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

  // Compute a Map from exit-value to (Path,Option[Boolean])) which denotes the shortest path
  //   from q0 to a final state with that exit value.
  //   if the Option[Boolean] is Some(true) then the path passes through only satisfiable
  //       transitions, i.e., transitions for which the label is satisfiable
  //   If the Option[Boolean] is None, then the path passes through at least one
  //       indeterminate transitions, i.e., the label.inhabited returned None, dont-know.
  def findSpanningPathMap():Map[E,(Option[Boolean],Path)] = {
    import adjuvant.BellmanFord.{shortestPath,reconstructPath}
    import scala.Double.PositiveInfinity
    val numStates:Double = 2 * Q.size.toDouble
    val states = Q.toSeq
    // We use the Bellman Ford shortest path algorithm to find the shortest
    //    path from q0 to each final state.  We do this by associating weights
    //    to each edge.  A satisfiable transition, has weight=1.  A uninhabited
    //    transition has weight = infinity.
    //    A indeterminate state has weight = 2*number-of-states.  Why?  Because
    //    if there is a path going only though satisfiable transitions, then it
    //    at most uses every state, thus has a length of num-states - 1.
    //    This means Bellman Ford will always prefer satisfiable paths to
    //    indeterminate paths.
    val (d,p) = shortestPath(states,
                             q0,
                             for{q<-states
                                 Transition(src,lab,dst) <- q.transitions
                                 edge <- labeler.inhabited(lab) match {
                                   case None => Some(((src,dst),numStates)) // indeterminate
                                   case Some(true) => Some(((src,dst),1.0)) // satisfiable
                                   case Some(false) => None // non-satisfiable
                                 }} yield edge)

    def maybePath(q:State[Σ, L, E]):(Int,(Option[Boolean],Path)) = {
      if (d(q) < numStates) // satisfiable path
        q.id -> (Some(true),reconstructPath[State[Σ, L, E]](p, q))
      else if (d(q) == PositiveInfinity) // not-satisfiable
        q.id -> (Some(false),List())
      else
        q.id -> (None,reconstructPath(p, q)) // non determinate
    }

    // m is the map from final state to shortest path for that state
    val m:Map[Int,(Option[Boolean],Path)] = F.map(maybePath).toMap

    def bestPath(pairs:Seq[(Option[Boolean],Path)]):(Option[Boolean],Path) = {
      pairs.fold((Some(false),List())) { (acc, path) =>
        (acc, path) match {
          case ((Some(true),_), _) => acc
          case (_,(Some(true),_)) => path
          case ((None,_),_) => acc
          case (_,(None,_)) => path
          case _ => acc
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
  private def findSpanningPath():MaybePath = {
    val m: Map[E, (Option[Boolean], Path)] = findSpanningPathMap()
    m.collectFirst{case item@(_,(Some(true),_)) => item} match {
      case Some((_,(_,path))) => Some(Right(path))
      case None => m.collectFirst{case item@(_,(None,_)) => item} match {
        case Some((_,(_,path))) => Some(Left(path))
        case _ => None
      }
    }
  }

  def simulate(seq: Seq[Σ]): Option[E] = {
    for {
      d <- findReachableFinal(seq)
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
           protoDelta: Set[(Int, L,Int)],
           labeler: Labeler[Σ, L],
           fMap: Map[Int, E]):Dfa[Σ, L, E] = {
    val merged = mergeParallel[Σ, L](labeler,protoDelta.toSeq)
    new Dfa[Σ, L, E](Qids, q0id, Fids, merged, labeler, fMap)
  }

  def combineFmap[E](e1: Option[E], e2: Option[E]): Option[E] = {
    (e1, e2) match {
      case (None, None) => None
      case (Some(b), Some(c)) if c == b => Some(b)
      case (Some(b), Some(c)) =>
        println(s"combineFmap: warning loosing value $c, using $b")
        Some(b) // f-value of dfa1 has precedence over dfa2
      case (Some(b), None) => Some(b)
      case (None, Some(b)) => Some(b)
    }
  }

  def dfaXor[Σ,L,E](dfa1: xymbolyco.Dfa[Σ, L, E],
                    dfa2: xymbolyco.Dfa[Σ, L, E]): xymbolyco.Dfa[Σ, L, E] = {
    Minimize.sxp[Σ, L, E](dfa1, dfa2,
                          (a: Boolean, b: Boolean) => (a && !b) || (!a && b), // arbitrateFinal:(Boolean,Boolean)=>Boolean,
                          combineFmap //:(E,E)=>E
                          )
  }

  def dfaUnion[Σ,L,E](dfa1: xymbolyco.Dfa[Σ, L, E],
                  dfa2: xymbolyco.Dfa[Σ, L, E]): xymbolyco.Dfa[Σ, L, E] = {
    Minimize.sxp[Σ, L, E](dfa1, dfa2,
                          (a: Boolean, b: Boolean) => a || b, // arbitrateFinal:(Boolean,Boolean)=>Boolean,
                          combineFmap //:(E,E)=>E
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
