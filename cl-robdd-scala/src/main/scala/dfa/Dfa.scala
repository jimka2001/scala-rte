// Copyright (c) 2020 EPITA Research and Development Laboratory
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

class State[L,E](dfa:Dfa[L,E], val id:Int) {
  var transitions:Set[Transition[L,E]] = Set()
  def delta(label:L):State[L,E] = dfa.delta(this,label)

  override def toString():String = {
    s"q:$id"
  }
}

case class Transition[L,E](source:State[L,E], label:L, destination:State[L,E]) {}

class Dfa[L,E](Qids:Set[Int], q0id:Int, Fids:Set[Int], protoDelta:Set[(Int,L,Int)], val combineLabels:(L,L)=>L, fMap:Map[Int,E]) {
  // q0id is in Qids
  require(Qids.contains(q0id))
  // each element of Fids is in Qids
  require(Fids.subsetOf(Qids))
  // each triple in protoDelta is of the form (x,_,y) where x and y are elements of Qids
  require(protoDelta.forall { case (from: Int, label: L, to: Int) => Qids.contains(from) && Qids.contains(to) })
  // fMap maps each element of Fids to some object of type E
  require(fMap.map{case (q,_) => q}.toSet == Fids)

  def exitValue(q:State[L,E]):E =  fMap(q.id)
  def findState(id: Int): State[L,E] = {
    Q.find(s => s.id == id).get
  }
  def delta(s:State[L,E], label:L):State[L,E] = {
    s.transitions.find(tr => tr.label == label).get.destination
  }
  val Q: Set[State[L,E]] = Qids.map((id:Int) => new State[L,E](dfa=this,id))
  for{
    (from , fromTriples) <- protoDelta.groupBy(_._1)
    fromState = findState(from)
    (to, toFromTriples) <- fromTriples.groupBy(_._3)
    toState = findState(to)
    label = toFromTriples.map(_._2).reduce(combineLabels)
  } fromState.transitions += new Transition(fromState,label,toState)

  val q0: State[L,E] = findState(q0id)
  val F: Set[State[L,E]] = Fids.map(findState)

  override def toString():String = Serialize.serializeToString(dfa=this)
}
