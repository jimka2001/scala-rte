package nfa

object Nfa {
  def compactify[L](initials: Set[Int], finals: Set[Int], transitions : Array[(Int, L, Int)])
  : (Set[Int], Set[Int], Array[(Int,L, Int)]) = {

    val newTransitions : Array[(Int, L, Int)] = Array.fill[(Int, L, Int)](transitions.length)((-1,null,-1))
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

  def rebase[L](initials: Set[Int], finals: Set[Int], transitions : Array[(Int, L, Int)], rebaseNumber:Int)
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    val newTransitions : Array[(Int, L, Int)] = Array.fill[(Int, L, Int)](transitions.length)((-1,null,-1))
    var newInit : Set[Int]=  Set()
    var newFinals : Set[Int] = Set()
    var mylist = initials.toList
    for(i<-Range(0,mylist.size)){
      newInit += mylist(i)+rebaseNumber
    }
    mylist = finals.toList
    for(i<-Range(0,mylist.size)){
      newFinals +=mylist(i)+rebaseNumber
    }
    for(i<-transitions.indices)
    {
      newTransitions(i) = (transitions(i)._1+rebaseNumber,transitions(i)._2,transitions(i)._3+rebaseNumber)
    }
    (newInit, newFinals,newTransitions)


  }
  def append[L](init1 : Set[Int], fin1 : Set[Int],transit1 : Array[(Int, L, Int)],
             init2 : Set[Int], fin2 : Set[Int],transit2 : Array[(Int, L, Int)])
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) ={
    val newTransitions : Array[(Int, L, Int)] = Array.fill[(Int, L, Int)](transit1.length+transit2.length)((-1,null,-1))
    var (newInit1,newFinal1,newTrans1) = compactify(init1,fin1,transit1)
    val (newInit2,newFinal2,newTrans2) = compactify(init2,fin2,transit2)
    var states = newInit1
    states ++= newFinal1
    for(i<-newTrans1.indices)
    {
      states += newTrans1(i)._1
      states += newTrans1(i)._3
    }

    val (newInit3,newFinal3,newTrans3)=rebase(newInit2,newFinal2,newTrans2,states.size)
    var newInit = newInit1
    newInit ++= newInit3
    var newFinals = newFinal1
    newFinals++=newFinal3
    for(i<-newTrans1.indices)
    {
      newTransitions(i) = newTrans1(i)
    }
    for(i<-newTrans3.indices)
    {
      newTransitions(i+newTrans1.length) = newTrans3(i)
    }
    (newInit,newFinals,newTransitions)
  }

  def findUseless[L](initials: Set[Int], finals: Set[Int], transitions : Array[(Int, L, Int)])
  : Set[Int] = {

    var useless :Set[Int] = Set()
    var allstates = initials
    var useful = initials
    allstates ++= finals
    useful++= finals
    for(i<-transitions.indices)
    {
      allstates+= transitions(i)._1
      allstates += transitions(i)._3
    }
    var queue = initials
    var a = 0
    while(queue.nonEmpty)
    {
      a= queue.head
      for(i<-transitions.indices)
      {
        if(transitions(i)._1==a)
        {
          if(!useful.contains(transitions(i)._3)){
            queue += transitions(i)._3
          }
        }
      }
      useful += a
      queue -= a
    }
    queue = finals
    while(queue.nonEmpty)
    {
      a= queue.head
      for(i<-transitions.indices)
      {
        if(transitions(i)._3==a)
        {
          if(!useful.contains(transitions(i)._1)){
            queue += transitions(i)._1
          }
        }
      }
      useful+=a
      queue -=a
    }
    val mylist = allstates.toList
    for(i<-mylist.indices)
    {
      if(!useful.contains(mylist(i)))
      {
        useless+=mylist(i)
      }
    }
    useless
  }
  def remove[L](initials: Set[Int], finals: Set[Int], transitions : Array[(Int, L, Int)])
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    var useless: Set[Int] = Set()
    var allstates = initials
    var access: Set[Int] = Set()
    allstates ++= finals
    for (i <- transitions.indices) {
      allstates += transitions(i)._1
      allstates += transitions(i)._3
    }
    var queue = initials
    var a = 0
    while (queue.nonEmpty) {
      a = queue.head
      for (i <- transitions.indices) {
        if (transitions(i)._1 == a) {
          if (!access.contains(transitions(i)._3)) {
            queue += transitions(i)._3
          }
        }
      }
      access+= a
      queue -= a
    }
    var coaccess: Set[Int]= Set()

    queue = finals
    while (queue.nonEmpty) {
      a = queue.head
      for (i <- transitions.indices) {
        if (transitions(i)._3 == a) {
          if (!coaccess.contains(transitions(i)._1)) {
            queue += transitions(i)._1
          }
        }
      }
      coaccess+=a
      queue -= a
    }
    val useful = access intersect coaccess

    var newInit = initials intersect useful
    var newFinals = finals intersect useful
    var count : Set[Int] = Set()
    for(i<-transitions.indices)
    {
      if(useful.contains(transitions(i)._1) && useful.contains(transitions(i)._3))
      {
        count += i
      }
    }
    val newTransitions : Array[(Int, L, Int)] = Array.fill[(Int, L, Int)](count.size)((-1,NULL,-1))
    val mylist2 = count.toList
    for(i<-mylist2.indices)
    {
      newTransitions(i)=transitions(mylist2(i))
    }
  (newInit,newFinals, newTransitions)
  }
  //def canonicalize() : () = {}

  def main(args : Array[String]) :Unit =
  {

    val myNFAInit= Set(2,3)
    val myNFAFinal=Set(4,6)
    val myNFATransitions = Array((2, 'a', 3), (4, 'b', 6), (3, 'b', 6),(7,'b',8))
    val use = findUseless(myNFAInit,myNFAFinal,myNFATransitions)
    println(use)
    /*val myNFAInit2= Set(12,13)
    val myNFAFinal2=Set(14,16)
    val myNFATransitions2 = Array((12, 'a', 13), (14, 'b', 16), (13, 'b', 16))
    var (a,b,c) =append(myNFAInit, myNFAFinal,myNFATransitions, myNFAInit2, myNFAFinal2, myNFATransitions2)
    println(a)
    println(b)
    for(i<-c.indices)
    {
      println(c(i))
    }*/
  }
}
