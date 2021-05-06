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

package dfa

// Seq[Σ] is the type of the input sequence which the DFA is expected to match
// L is the type of label on transitions
// E is the type of exit values
class Dfa[Σ,L,E](Qids:Set[Int],
                 q0id:Int,
                 Fids:Set[Int],
                 protoDelta:Set[(Int,L,Int)],
                 val labeler:Labeler[Σ,L],
                 fMap:Map[Int,E]) {
  // q0id is in Qids
  require(Qids.contains(q0id))
  // each element of Fids is in Qids
  require(Fids.subsetOf(Qids))
  // each triple in protoDelta is of the form (x,_,y) where x and y are elements of Qids
  require(protoDelta.forall { case (from: Int, _, to: Int) => Qids.contains(from) && Qids.contains(to) })
  // fMap maps each element of Fids to some object of type E
  require(fMap.map{case (q,_) => q}.toSet == Fids)

  def exitValue(q:State[Σ,L,E]):E =  fMap(q.id)
  def findState(id: Int): State[Σ,L,E] = {
    Q.find(s => s.id == id).get
  }
  // to find the destination state of a state, we
  //   match the label exactly using ==
  //   TODO, this should really be done using either equivalent
  //    labels or subset relation etc.
  def delta(s:State[Σ,L,E], label:L):State[Σ,L,E] = {
    s.transitions.find(tr => labeler.equivLabels(tr.label, label)).get.destination
  }

  val Q: Set[State[Σ,L,E]] = Qids.map((id:Int) => new State[Σ,L,E](dfa=this,id))
  for{
    (from , fromTriples) <- protoDelta.groupBy(_._1)
    fromState = findState(from)
    (to, toFromTriples) <- fromTriples.groupBy(_._3)
    toState = findState(to)
    label = toFromTriples.map(_._2).reduce(labeler.combineLabels)
  } fromState.transitions += Transition(fromState,label,toState)

  val q0: State[Σ,L,E] = findState(q0id)
  val F: Set[State[Σ,L,E]] = Fids.map(findState)

  override def toString:String = Serialize.serializeToString(dfa=this)

  def findReachableFinal(seq: Seq[Σ]): Option[State[Σ,L, E]] = {
    val init:Option[State[Σ,L,E]] = Some(q0)
    seq.foldLeft(init) { (mq, s) =>
      mq.flatMap(_.successor(s))
    }
  }

  def simulate(seq: Seq[Σ]): Option[E] = {
    for{
      d <- findReachableFinal(seq)
      if F.contains(d)
    } yield exitValue(d)
  }
}
