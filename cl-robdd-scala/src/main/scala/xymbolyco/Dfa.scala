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
  def findTrace(satisfiable:Seq[Option[Boolean]]=Seq(Some(true))):Option[Seq[L]] = {
    findSpanningPath().map{states =>
      states.toList.tails.flatMap{
        case q1::q2::_ =>
          val Some(Transition(_,label,_)) = q1.transitions.find{
            case Transition(_,label,dst)
              if dst == q2 && satisfiable.contains(labeler.inhabited(label)) => true
            case _ => false
          }
          List(label)
        case _ => Nil
      }.toSeq
    }
  }

  type Path = List[State[Σ,L,E]]

  def findSpanningPathX():Option[Either[Path,Path]] = {
    def splitTransitions(transitions:List[Transition[Σ,L,E]]):(List[Transition[Σ,L,E]],List[Transition[Σ,L,E]]) = {
      val grouped = transitions.groupBy{case Transition(_, label, _) => labeler.inhabited(label)}
      Tuple2(grouped.getOrElse(Some(true),List()),
             grouped.getOrElse(None,List()))
    }
    def nonLooping(tr:Transition[Σ,L,E],path:Path):Boolean = {
      val Transition(_,_,dst) = tr
      !path.contains(dst)
    }
    def sortTransitions(transitions:List[Transition[Σ,L,E]]):List[Transition[Σ,L,E]] = {
      val (fs,nonfs) = transitions.partition{case Transition(_,_,dst) => F.contains(dst)}
      fs ++ nonfs
    }
    def recur(goodPaths:List[Path],
              badPaths:List[Path]):Option[Either[Path,Path]] = {
      (goodPaths,badPaths) match {
        case (Nil,Nil) => None
        case ((p@(s::_))::_, _) if F.contains(s)=> Some(Right(p))
        case ((p@(s::ss))::ps, _) =>
          val (goods, bads) = splitTransitions(s.transitions
                                                 .filter { tr => nonLooping(tr, p) }
                                                 .toList
                                               )
          val newGoodPaths = for{Transition(_,_,dst) <- sortTransitions(goods)} yield  dst::p
          val newBadPaths = for{Transition(_, _, dst) <- sortTransitions(bads)} yield  dst::p
          recur(newGoodPaths ++ ps,
                newBadPaths ++ badPaths)
        case (Nil,(p@(s::_))::_) if F.contains(s)=> Some(Left(p))
        case (Nil, (p@(s::ss))::ps) =>
          val bads = s.transitions
            .filter { tr => nonLooping(tr, p) }
            .toList
          val newBadPaths = for{Transition(_, _, dst) <- sortTransitions(bads)} yield  dst::p
          recur(Nil, newBadPaths ++ badPaths)
      }
    }
    recur(List(List(q0)),List())
  }

  // if possible, returns a sequence of States which lead from q0 to a
  // final state.
  //   if satisfiable = Some(true), then we only traverse transitions which are
  //      definitely satisfiable, i.e., the labeler.inhabited(label) function returns Some(true)
  //          as opposed to None for dont-know whether it is satisfiable
  //   if satisfiable = None, then we also traverse transitions for which
  //          we get None from label.inhabited(label) meaning that we don't know for sure
  //          that the label is satisfiable.
  def findSpanningPath(satisfiable:Seq[Option[Boolean]]=Seq(Some(true))):Option[Seq[State[Σ,L,E]]] = {
    def augment(paths: Seq[List[State[Σ, L, E]]]): Seq[List[State[Σ, L, E]]] = {
      paths.flatMap {
        case s :: ss => for {Transition(_, label, dst) <- s.transitions
                             if s != dst && !ss.contains(dst)
                             if satisfiable.contains(labeler.inhabited(label))
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

  def vacuous():Option[Boolean] = {
    if (Q.isEmpty)
      Some(true)
    else if (F.isEmpty)
      Some(true)
    else if (findSpanningPath(Seq(Some(true))).isEmpty)
      Some(true)
    else if (findSpanningPath(Seq(Some(true),None)).isEmpty)
      None
    else
      Some(false)
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
