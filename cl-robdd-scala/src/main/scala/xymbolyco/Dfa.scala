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

  // returns a map of src -> Set(dst)
  def successors():Map[Int,Set[Int]] = {
    for{ (src,triples) <- protoDelta.groupBy{case (src,_,dst) => src}
         } yield src -> triples.map(_._3)
  }

  // returns a map of dst -> Set(src)
  def predecessors():Map[Int,Set[Int]] = {
    for{ (dst,triples) <- protoDelta.groupBy{case (src,_,dst) => dst}
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

  def findReachableFinal(seq: Seq[Σ]): Option[State[Σ, L, E]] = {
    val init: Option[State[Σ, L, E]] = Some(q0)
    seq.foldLeft(init) { (mq, s) =>
      mq.flatMap(_.successor(s))
    }
  }

  // find some sequence of objects of labels leading
  // from q0 to a final state.
  def findTrace():Option[Seq[L]] = {
    findSpanningPath().map{states =>
      states.toList.tails.flatMap{
        case q1::q2::_ =>
          val Some(Transition(_,label,_)) = q1.transitions.find{
            case Transition(_,_,dst) if dst == q2 => true
            case _ => false
          }
          List(label)
        case _ => Nil
      }.toSeq
    }
  }

  // if possible, returns a sequence of States which lead from q0 to a
  // final state.
  def findSpanningPath():Option[Seq[State[Σ,L,E]]] = {
    def augment(paths: Seq[List[State[Σ, L, E]]]): Seq[List[State[Σ, L, E]]] = {
      paths.flatMap {
        case s :: ss => for {Transition(src, label, dst) <- s.transitions
                             if s != dst && !ss.contains(dst)
                             } yield dst :: s :: ss
        case _ => Nil // unused but silences compiler warning
      }
    }
    @tailrec
    def recur(paths:Seq[List[State[Σ, L, E]]]): Option[Seq[State[Σ, L, E]]] = {
      lazy val found = paths.collectFirst{
        case p@s::_ if F.contains(s) => p
      }
      if (paths.isEmpty)
        None
      else if (found.nonEmpty)
        found.map(_.reverse)
      else
        recur(augment(paths))
    }
    recur(Seq(List(q0)))
  }

  def simulate(seq: Seq[Σ]): Option[E] = {
    for {
      d <- findReachableFinal(seq)
      if F.contains(d)
    } yield exitValue(d)
  }
}
