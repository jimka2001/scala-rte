package xymbolyco

import com.sun.javaws.exceptions.InvalidArgumentException
import genus.{RandomType, SimpleTypeD}

object RandomDFAGautier {
  def RandomDFA[Σ, L, E](num: Int, finals: Int, transitions: Int, transrange: Int,
                         exitValue: E, typedepth: Int, restrictions: Option[Boolean]): Dfa[Any, SimpleTypeD, E] = {
    val r = scala.util.Random
    if(finals+1>num || transitions+transrange>num-1)
    {
      throw new InvalidArgumentException(Array("cannot have more transitions per state than state, or more final states than states"))
    }
    val qids: Set[Int] = Range(0, num).toSet
    var fids: Set[Int] = Set(num - 1)
    while (fids.size < finals +1) {
      fids += r.nextInt(num)
    }
    if(transrange==0){
      throw new IllegalArgumentException("transrange must be positive")
    }
    val protoDelta = (for {i <- Range(0, num)
                           mylist = randomTransition(transitions,transrange, i, num).toList
                           j <- mylist.indices
                           label = randomTypeD(restrictions, r.nextInt(typedepth))
                           mytuple = (i, label, mylist(j))
                           }
      yield mytuple).toSet
    Dfa.apply(qids, 0, fids, protoDelta, xymbolyco.GenusLabeler(), fids.map { i => i -> exitValue }.toMap)
  }

  def randomTypeD(rest: Option[Boolean], depth: Int): SimpleTypeD = {
    var myType = RandomType.randomType(depth)
    var myBool = myType.inhabited
    while (!rest.contains(true) && !myBool.contains(true) && ((rest.isEmpty && (myBool.contains(false) || myBool.isEmpty))
      || (rest.contains(false) && myBool.contains(false))))
    {
      myType = RandomType.randomType(depth)
      myBool = myType.inhabited
    }
    myType
  }

  def randomTransition(t: Int, trrange:Int, i: Int, max: Int): Set[Int] = {
    var nt = t
    if(trrange!=0){
      nt+= scala.util.Random.nextInt(trrange*2)}

    var mySet: Set[Int] = Set()
    while (mySet.size < nt) {
      mySet += randomVertex(i, max)
    }
    mySet
  }

  def randomVertex(current: Int, max: Int): Int = {
    val r = scala.util.Random

    val temp = r.nextInt(100)
    if (temp < 60) {
      val res = r.nextInt(3) + current+1
      if (res >= max) {
        max - 1
      }
      else res
    }
    else if(temp < 65) {
      current
    }
    else if (temp < 90) {
      r.nextInt(max  - current) + current
    }
    else if(current!=0){
      current - r.nextInt(current)
    }
    else current
  }
}
