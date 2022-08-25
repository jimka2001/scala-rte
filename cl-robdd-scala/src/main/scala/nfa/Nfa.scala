package nfa

import scala.collection.immutable.Queue

object Nfa {
  def compactify[L](initials: Set[Int], finals: Set[Int], transitions : Array[(Int, L, Int)])
  : (Set[Int], Set[Int], Array[(Int,L, Int)]) = {
    rebase(initials,finals,transitions,0)}
  def rebase[L](initials: Set[Int], finals: Set[Int], transitions : Array[(Int, L, Int)], rebaseNumber:Int)
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    val all : Set[Int] = initials ++ finals ++ transitions.flatMap(tr=>Seq(tr._1,tr._3))
    val mymap = all.zip(Range(rebaseNumber,all.size+rebaseNumber)).toMap
    (initials.map(mymap),finals.map(mymap), transitions.map(tr=>(mymap(tr._1),tr._2,mymap(tr._3))))
  }
  def append[L](init1 : Set[Int], fin1 : Set[Int],transit1 : Array[(Int, L, Int)],
             init2 : Set[Int], fin2 : Set[Int],transit2 : Array[(Int, L, Int)])
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) ={
    val (newInit1,newFinal1,newTrans1) = compactify(init1,fin1,transit1)
    val states =newInit1 ++ newFinal1 ++ newTrans1.flatMap(tr=>Seq(tr._1,tr._3))
    val (newInit2,newFinal2,newTrans2) = rebase(init2,fin2,transit2,states.size)
    (newInit1++newInit2,newFinal1++newFinal2,newTrans1++newTrans2)
  }
  def accessible[L](initials: Set[Int], transitions : Array[(Int, L, Int)]) : Set[Int]=
  {
    def delta(S : Int): Set[Int]=
    {
      var myset : Set[Int] = Set()
      for(i<-transitions.indices)
      {
        if(transitions(i)._1==S)
        {
          myset += transitions(i)._3
        }
      }
      myset
    }
    def recur(here : Set[Int], done: Set[Int]) : Set[Int] =
    {
      if(here.nonEmpty)
      {
        return recur(here.flatMap(delta).diff(done),done++here)
      }
      done
    }
    recur(initials,Set())
  }
  def coaccessible[L](finals: Set[Int], transitions : Array[(Int, L, Int)]) : Set[Int]=
  {
    def delta(S : Int): Set[Int]=
    {
      var myset : Set[Int] = Set()
      for(i<-transitions.indices)
      {
        if(transitions(i)._3==S)
        {
          myset += transitions(i)._1
        }
      }
      myset
    }
    def recur(here : Set[Int], done: Set[Int]) : Set[Int] =
    {
      if(here.nonEmpty)
      {
        return recur(here.flatMap(delta).diff(done),done++here)
      }
      done
    }
    recur(finals,Set())
  }
  def findUseless[L](initials: Set[Int], finals: Set[Int], transitions : Array[(Int, L, Int)])
  : Set[Int] = {
    val allstates = transitions.flatMap(tr=>Seq(tr._1,tr._3)).toSet
    allstates.diff(accessible(initials,transitions).union(coaccessible(finals,transitions)))
  }
  def remove[L](initials: Set[Int], finals: Set[Int], transitions : Array[(Int, L, Int)])
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    val allstates = initials ++ finals ++ transitions.flatMap(tr=>Seq(tr._1,tr._3)).toSet
    val useful = allstates.intersect(accessible(initials,transitions).intersect(coaccessible(finals,transitions)))
    val newInit = initials intersect useful
    val newFinals = finals intersect useful
    var newTransitions : Array[(Int, L, Int)] = Array()
    for(i<-transitions.indices)
    {
      if(useful.contains(transitions(i)._1) && useful.contains(transitions(i)._3))
      {
        newTransitions = newTransitions.appended(transitions(i))
      }
    }
  (newInit,newFinals, newTransitions)
  }

 /* def fchar[L](c1 : L, c2 : L) : Boolean =
  {
    if(c2 > c1)
    {
      return false
    }
     true
  }*/
  def canonicalize[L](initials : Set[Int], finals: Set[Int],transitions : Array[(Int,L,Int)], f: (L,L)=> Boolean)
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    if (initials.size != 1) {
      return (initials, finals, transitions)
    }
    var count = 1
    var a: (Int, Queue[Int]) = (0, Queue())
    var queue: Queue[Int] = Queue()
    val mylist = initials.toList
    var mymap: Map[Int, Int] = Map(mylist(0)->0)
    queue = queue.enqueue(mylist(0))
    var myarray : Array[(Int,L,Int)]= Array()
    while (queue.nonEmpty) {
      println(count)
      a = queue.dequeue
      queue = a._2
      for (i <- transitions.indices) {
        if (transitions(i)._1 == a._1) {
          if ((!mymap.contains(transitions(i)._3)) || (!queue.contains(transitions(i)._3))) {
            myarray = myarray ++ Array(transitions(i))
          }
        }
      }

      for(i<-Range(0,myarray.length-1))
      {
       if(!f(myarray(i)._2,myarray(i+1)._2)) {
          val temp = myarray(i+1)
          myarray(i+1)=myarray(i)
         myarray(i)= temp
       }
      }

      for (i <- myarray.indices) {
        queue = queue.enqueue(myarray(i)._3)
        mymap ++= Map(myarray(i)._3 -> count)
        count += 1
      }
      myarray = Array()

    }

    var newTransitions : Array[(Int,L,Int)] = Array()
    for(i<-transitions.indices)
    {
      if(mymap.contains(transitions(i)._1))
      {
        newTransitions = newTransitions++ Array((mymap(transitions(i)._1), transitions(i)._2,mymap(transitions(i)._3)))
      }
    }
    (initials.map(mymap), finals.map(mymap), newTransitions)
  }



  def main(args : Array[String]) :Unit =
  {

    val myNFAInit= Set(2)
    val myNFAFinal=Set(4,6)
    val myNFATransitions = Array((2, 'a', 3), (2,'c',5) ,(3, 'b',4), (3, 'b', 6),(5,'b',6),(7,'a',6))
    /*val (a,b,c) = canonicalize(myNFAInit,myNFAFinal,myNFATransitions,fchar)
    println(a)
    println(b)
    for(i<-c.indices){
      println(c(i))
    }
    val myNFAInit2= Set(12,13)
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
