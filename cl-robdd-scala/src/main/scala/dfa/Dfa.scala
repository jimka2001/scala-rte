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
