// TODO remove this file and this package,
//    this was a pedagogical project.  It does not belong in the master branch.
//    Are there any test cases associated with it, if so make sure they are
//    removed as well.

package nfa

import scala.collection.immutable.Queue
import scala.util.Random
import scala.util.matching.Regex

object Nfa {
  def append[L](init1: Set[Int], fin1: Set[Int], transit1: Array[(Int, L, Int)],
                init2: Set[Int], fin2: Set[Int], transit2: Array[(Int, L, Int)])
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    val (newInit1, newFinal1, newTrans1) = compactify(init1, fin1, transit1)
    val states = newInit1 ++ newFinal1 ++ newTrans1.flatMap(tr => Seq(tr._1, tr._3))
    val (newInit2, newFinal2, newTrans2) = rebase(init2, fin2, transit2, states.size)
    (newInit1 ++ newInit2, newFinal1 ++ newFinal2, newTrans1 ++ newTrans2)
  }

  def compactify[L](initials: Set[Int], finals: Set[Int], transitions: Array[(Int, L, Int)])
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    rebase(initials, finals, transitions, 0)
  }

  def rebase[L](initials: Set[Int], finals: Set[Int], transitions: Array[(Int, L, Int)], rebaseNumber: Int)
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    val all: Set[Int] = initials ++ finals ++ transitions.flatMap(tr => Seq(tr._1, tr._3))
    val mymap = all.zip(Range(rebaseNumber, all.size + rebaseNumber)).toMap
    (initials.map(mymap), finals.map(mymap), transitions.map(tr => (mymap(tr._1), tr._2, mymap(tr._3))))
  }

  def findUseless[L](initials: Set[Int], finals: Set[Int], transitions: Array[(Int, L, Int)])
  : Set[Int] = {
    val allstates = transitions.flatMap(tr => Seq(tr._1, tr._3)).toSet
    allstates.diff(accessible(initials, transitions).union(coaccessible(finals, transitions)))
  }

  def accessible[L](initials: Set[Int], transitions: Array[(Int, L, Int)]): Set[Int] = {
    def delta(S: Int): Set[Int] = {
      transitions.flatMap(tr => if (tr._1 == S) {
        Set(tr._3)
      } else {
        Set()
      }).toSet
    }

    def recur(here: Set[Int], done: Set[Int]): Set[Int] = {
      if (here.nonEmpty) {
        recur(here.flatMap(delta).diff(done), done ++ here)
      }
      else {
        done
      }
    }

    recur(initials, Set())
  }

  def coaccessible[L](finals: Set[Int], transitions: Array[(Int, L, Int)]): Set[Int] = {
    def delta(S: Int): Set[Int] = {
      transitions.flatMap(tr => if (tr._3 == S) {
        Set(tr._1)
      } else {
        Set()
      }).toSet
    }

    def recur(here: Set[Int], done: Set[Int]): Set[Int] = {
      if (here.nonEmpty) {
        recur(here.flatMap(delta).diff(done), done ++ here)
      }
      else {
        done
      }
    }

    recur(finals, Set())
  }

  def remove[L](initials: Set[Int], finals: Set[Int], transitions: Array[(Int, L, Int)])
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    val allstates = initials ++ finals ++ transitions.flatMap(tr => Seq(tr._1, tr._3)).toSet
    val useful = allstates.intersect(accessible(initials, transitions).intersect(coaccessible(finals, transitions)))
    val newInit = initials intersect useful
    val newFinals = finals intersect useful
    val newTrans = transitions.flatMap(tr => if (ftrans(useful, tr)) {
      Set(tr)
    } else {
      Set()
    })
    (newInit, newFinals, newTrans)
  }

  def ftrans[L](useful: Set[Int], trans: (Int, L, Int)): Boolean = {
    useful.contains(trans._1) && useful.contains(trans._3)
  }

  def fchar(c1: Char, c2: Char): Boolean = {
    (c1 < c2)
  }

  def transsort[L](tr1: (Int, L, Int), tr2: (Int, L, Int), f: (L, L) => Boolean): Boolean = {
    f(tr1._2, tr2._2)
  }

  def generate[L](rangeOfStates: Int, rangeOfConnections: Int, rangeOfFinals: Int, alphabet: Array[L], determinism: Boolean): Seq[(Set[Int], Set[Int], Array[(Int, L, Int)])] = {
    for {x <- 1 to rangeOfStates
         y <- 1 to rangeOfConnections
         z <- 1 to rangeOfFinals
         if z < x}
      yield generateNFA(x, alphabet, z, y, determinism)
  }

  def generateNFA[L](nb: Int, Alpha: Array[L], nbfinal: Int, nbconnect: Int, isDeterministic: Boolean): (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    val r = scala.util.Random
    var finals: Set[Int] = Set()
    var transitions: Array[(Int, L, Int)] = Array()
    while (finals.size < nbfinal) {
      finals += r.nextInt(nb)
    }
    if (!isDeterministic) for (i <- Range(0, nb)) {
      for (j <- Range(0, r.nextInt(nbconnect) + 1)) {
        transitions = transitions ++ Array((i, Alpha(r.nextInt(Alpha.length)), r.nextInt(nb)))
      }
    }
    else {
      for (i <- Range(0, nb)) {
        var count = 0
        while (count < r.nextInt(nbconnect) + 1) {
          for (j <- Alpha.indices) {
            if (r.nextBoolean()) {
              transitions = transitions ++ Array((i, Alpha(j), r.nextInt(nb)))
              count += 1
            }
          }
        }
      }
    }
    (Set(r.nextInt(nb)), finals, transitions)
  }

  def generate2[L](g: (L, L) => Boolean, rangeOfStates: Int, rangeOfConnections: Int, rangeOfFinals: Int, alphabet: Array[L], determinism: Boolean): Unit = {
    for (x <- 1 to rangeOfStates)
      for (y <- 1 to rangeOfConnections)
        for (z <- 1 to rangeOfFinals)
          if (z < x) {
            val (a, b, c) = generateNFA(x, alphabet, z, y, determinism)
            val (d, e, f) = generateisomorphic(a, b, c)
            println(isIsomorphic(g, a, d, b, e, c, f))
          }
  }

  def generateisomorphic[L](initials: Set[Int], finals: Set[Int], transitions: Array[(Int, L, Int)]): (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    val r = scala.util.Random
    val allstates = initials ++ finals ++ transitions.flatMap(tr => Seq(tr._1, tr._3)).toSet
    var myset: Set[Int] = Set()
    while (myset.size != allstates.size) {
      myset += r.nextInt(allstates.size * 2)
    }
    val mymap = allstates.zip(myset).toMap
    (initials.map(mymap), finals.map(mymap), transitions.map(tr => (mymap(tr._1), tr._2, mymap(tr._3))))
  }

  def isIsomorphic[L](f: (L, L) => Boolean, init1: Set[Int], init2: Set[Int], final1: Set[Int], final2: Set[Int], trans1: Array[(Int, L, Int)], trans2: Array[(Int, L, Int)]): Boolean = {
    val (i1, f1, t1) = canonicalize(init1, final1, trans1, f)
    val (i2, f2, t2) = canonicalize(init2, final2, trans2, f)
    val myset = i1 intersect i2
    if (myset.size != i1.size) {
      false
    }
    else {
      val myset2 = f1 intersect f2
      if (myset2.size != f1.size) {
        false
      }
      else {
        val myarray = t1 intersect t2
        if (myarray.length != t1.length) {
          false
        }
        else true
      }
    }
  }

  def canonicalize[L](initials: Set[Int], finals: Set[Int], transitions: Array[(Int, L, Int)], f: (L, L) => Boolean)
  : (Set[Int], Set[Int], Array[(Int, L, Int)]) = {
    if (initials.size != 1) {
      (initials, finals, transitions)
    }
    else {
      def delta(S: Int): Set[(Int, L, Int)] = {
        transitions.flatMap(tr => if (tr._1 == S) {
          Set(tr)
        } else {
          Set()
        }).toSet
      }

      def recur(next: List[Int], visited: List[Int]): List[Int] = {
        if (next.nonEmpty) {
          val a: List[Int] = delta(next.head).toList.sortWith((a, b) => f(a._2, b._2)).flatMap(a => if (!next.contains(a._3) && !visited.contains(a._3)) {
            List(a._3)
          }
          else List()
                                                                                               )
          recur(next.tail ++ a, visited :+ next.head)
        }
        else visited
      }

      val mylist: List[Int] = recur(List(initials.head), List(): List[Int])
      val mymap2: Map[Int, Int] = mylist.zip(0 until mylist.size).toMap
      (initials.map(mymap2), finals.flatMap(a => if (mymap2.contains(a)) {
        Set(mymap2(a))
      } else Set()),
        transitions.flatMap(tr => if (ftrans2(mymap2, tr)) {
          Set((mymap2(tr._1), tr._2, mymap2(tr._3)))
        } else Set()))
    }
  }

  def ftrans2[L](useful: Map[Int, Int], trans: (Int, L, Int)): Boolean = {
    useful.contains(trans._1)
  }

  def main(args: Array[String]): Unit = {
    println(foo()())
  }

  def foo(a: Int = 3)(b: Int = 2): Int = {
    a + b
  }
}
