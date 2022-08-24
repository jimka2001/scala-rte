package nfa

object Nfa {
  def compactify(initials: Set[Int], finals: Set[Int], transitions : Array[(Int, Char, Int)])
  : (Set[Int], Set[Int], Array[(Int, Char, Int)]) = {

    val newTransitions : Array[(Int, Char, Int)] = Array.fill[(Int, Char, Int)](transitions.length)((-1,'a',-1))
    var newInit : Set[Int]=  Set()
    var newFinals : Set[Int] = Set()
    var states = initials
    states ++= finals
    for(i<-transitions.indices)
    {
      states += transitions(i)._1
      states += transitions(i)._3
    }
    val min = states.min
    val max = states.max
    var map = Map(min->0)
    var temp = 0
    for(i<-Range(1,max-min+1))
    {
      if(states.contains(min+i)){
        map = map ++ Map((min+i)->(i-temp))
      }
      else{
        temp+=1
      }
    }
    var mylist = initials.toList
    for(i<-Range(0,mylist.size)){
      newInit += map(mylist(i))
    }
    mylist = finals.toList
    for(i<-Range(0,mylist.size)){
      newFinals +=map(mylist(i))
    }
    for(i<-transitions.indices)
    {
      newTransitions(i) = (map(transitions(i)._1),transitions(i)._2,map(transitions(i)._3))
    }
  (newInit, newFinals,newTransitions)
  }

  def rebase(initials: Set[Int], finals: Set[Int], transitions : Array[(Int, Char, Int)], rebaseNumber:Int)
  : (Set[Int], Set[Int], Array[(Int, Char, Int)]) = {
    val newTransitions : Array[(Int, Char, Int)] = Array.fill[(Int, Char, Int)](transitions.length)((-1,'a',-1))
    var newInit : Set[Int]=  Set()
    var newFinals : Set[Int] = Set()
    var mylist = initials.toList
    for(i<-Range(0,mylist.size)){
      newInit += mylist(i)-rebaseNumber
    }
    mylist = finals.toList
    for(i<-Range(0,mylist.size)){
      newFinals +=mylist(i)-rebaseNumber
    }
    for(i<-transitions.indices)
    {
      newTransitions(i) = (transitions(i)._1-rebaseNumber,transitions(i)._2,transitions(i)._3-rebaseNumber)
    }
    (newInit, newFinals,newTransitions)


  }
  def append() : () ={}

  /*def findUseless() : () = {}
  def remove() :() = {}
  def canonicalize() : () = {}
  */
  def main(args : Array[String]) :Unit =
  {
    val myNFAInit= Set(2,3)
    val myNFAFinal=Set(4,6)
    val myNFATransitions = Array((2, 'a', 3), (4, 'b', 6), (3, 'b', 6))
    compactify(myNFAInit,myNFAFinal,myNFATransitions)


  }
}
