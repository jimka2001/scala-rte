package nfa

import scalafx.scene.input.KeyCode.S

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

 /* def canonicalize[L](initials : Set[Int], finals: Set[Int],transitions : Array[(Int,L,Int)])
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    if(initials.size!=1)
    {
      var count = 0
      var mymap : Map[Int,Int] = Map()
      var a =0
      var queue = initials
      while(queue.nonEmpty)
      {
        a = queue.head
        for (i <- transitions.indices) {
          if (transitions(i)._1 == a) {
            if (!mymap.contains(transitions(i)._3)) {
              queue += transitions(i)._3
            }
          }
        }
        mymap ++= Map(a->count)
        queue -= a
      }
      return(initials,finals,transitions)
    }


  }


  */
  def main(args : Array[String]) :Unit =
  {

    val myNFAInit= Set(2,3)
    val myNFAFinal=Set(4,6)
    val myNFATransitions = Array((2, 'a', 3), (5,'c',5) ,(4, 'b', 6), (3, 'b', 6),(7,'b',8))
    val (a,b,c) = remove(myNFAInit,myNFAFinal,myNFATransitions)
    println(a)
    println(b)
    for(i<-c.indices){
      println(c(i))
    }
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
