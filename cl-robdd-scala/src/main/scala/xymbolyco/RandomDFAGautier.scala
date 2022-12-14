package xymbolyco

import adjuvant.Adjuvant._
import genus.{RandomType, SimpleTypeD}
import adjuvant.BellmanFord._
object RandomDFAGautier {

  def RandomDFA[E](num: Int = 5, finals: Int = scala.util.Random.nextInt(3), transitions: Int = 2, transrange: Int = 1,
                   exitValue: E = 42, typedepth: Int = 1, filter: Option[Boolean] = None): Dfa[Any, SimpleTypeD, E] = {
    val r = scala.util.Random
    if (finals + 1 > num || transitions + transrange > num - 1) {
      throw new Exception("cannot have more transitions per state than state, or more final states than states")
    }
    val qids: Set[Int] = Range(0, num).toSet

    val fids = generateVerticesSet(finals, num) + (num - 1)
    if (transrange == 0) {
      throw new IllegalArgumentException("transrange must be positive")
    }
    val protoDelta = (for {i <- Range(0, num)
                           mylist = randomTransition(transitions, transrange, i, num).toList
                           j <- mylist.indices
                           label = RandomType.randomType(r.nextInt(typedepth), filter)
                           mytuple = (i, label, mylist(j))
                           }
      yield mytuple).toSet
    Dfa.apply(qids, 0, fids, protoDelta, xymbolyco.GenusLabeler(), fids.map { i => i -> exitValue }.toMap)
  }

  def randomTransition(t: Int, trrange: Int, i: Int, num: Int): Set[Int] = {

    if (trrange != 0) {
      generateVerticesSet(t + util.Random.nextInt(trrange * 2), num)
    }
    else {
      generateVerticesSet(t, num)
    }
  }

  def randomVertex(current: Int, max: Int): Int = {
    val r = scala.util.Random

    val temp = r.nextInt(100)
    if (temp < 60) {
      val res = r.nextInt(3) + current + 1
      if (res >= max) {
        max - 1
      }
      else res
    }
    else if (temp < 65) {
      current
    }
    else if (temp < 90) {
      r.nextInt(max - current) + current
    }
    else if (current != 0) {
      current - r.nextInt(current)
    }
    else current
  }
}
