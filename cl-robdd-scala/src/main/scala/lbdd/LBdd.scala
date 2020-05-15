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


package lbdd

import bdd.Assignment


sealed abstract class LBdd {

  def findSatisfyingAssignment(): Option[(Assignment, Assignment)] = {
    def recur(b: LBdd, assignTrue: Assignment, assignFalse: Assignment): Option[(Assignment, Assignment)] = {
      b match {
        case LBddFalse => None
        case LBddTrue => Some((assignTrue, assignFalse))
        case LBddNode(label, positive, middle, negative) =>
          recur(positive, Assignment(assignTrue.trueVariables + label), assignFalse) orElse
          recur(negative, assignTrue, Assignment(assignTrue.trueVariables + label)) orElse
          recur(unlazify(middle), assignTrue, assignFalse)
      }
    }
    recur(this, Assignment(Set[Int]()), Assignment(Set[Int]()))
  }


  def fold[R](z: R)(f: (R, LBdd) => R): R = {
    def bfsWalk(f: LBdd => Unit): Unit = {
      def recur(generation: List[LBdd], nextGeneration: List[LBdd], done: Set[LBdd]): Unit = {
        // BFS walk of bdd, calling f exactly once on each node
        (generation, nextGeneration) match {
          case (Nil, Nil) => Unit
          case (Nil, _) => recur(nextGeneration, Nil, done)
          case (h :: tail, _) if done.contains(h) => recur(tail, nextGeneration, done)
          case (h :: tail, _) =>
            f(h)
            recur(tail,
              h match {
                case LBddNode(_, pos, None, neg) => pos :: neg :: nextGeneration
                case LBddNode(_, pos, Some(mid), neg) => pos :: mid() :: neg :: nextGeneration
                case _ => nextGeneration
              },
              done + h
            )
        }
      }

      recur(List(this), Nil, Set())
    }

    var ret: R = z
    bfsWalk { b: LBdd =>
      ret = f(ret, b)
    }
    ret
  }

  def size():Int = {
    var visited: Set[LBdd] = Set()

    def recur(bdd: LBdd, n: Int): Int = {
      if (visited.contains(bdd))
        n
      else {
        visited = visited + bdd
        bdd match {
          case _: LBddTerm => 1 + n
          case bdd: LBddNode => {
            recur(bdd.negative, recur(bdd.middle.get(), recur(bdd.positive, 2 + n)))
          }
        }
      }
    }

    recur(this, 0)
  }


  def apply(assignment: Assignment): Boolean
}

///////////////////////////////
// LBdd Terminal nodes
///////////////////////////////

sealed abstract  class LBddTerm extends LBdd {
}

object LBddTrue extends LBddTerm {
  override def toString = "T"

  override def apply(assignments: Assignment): Boolean = {
    true
  }
}

object LBddFalse extends LBddTerm {
  override def toString = "F"

  def apply(assignment: Assignment): Boolean = {
    false
  }
}



///////////////////////////////
// LBdd Internal nodes
///////////////////////////////



case class LBddNode(label: Int, positive: LBdd,
                    middle: lazyNode, negative: LBdd) extends LBdd {
  override def equals(that: Any): Boolean = {
    that match {
      case b: LBdd => this eq b
      case _ => false
    }
  }

  LBdd.counter += 1

  def validateChild(child: LBdd): Unit = {
    child match {
      case child: LBddNode => assert(child.label > label)
      case _: LBddTerm => Unit
    }
  }

  validateChild(positive)
  validateChild(negative)

  def apply(assignment: Assignment): Boolean = {
    if (assignment.value(label))
      positive(assignment) || unlazify(middle)(assignment)
    else
      negative(assignment) || unlazify(middle)(assignment)
  }

  override def toString: String = {
    (positive, middle, negative) match {
      case (LBddTrue, None, LBddFalse) => label.toString
      case (LBddFalse, None, LBddTrue) => "!" + label.toString

        // TODO : Better printer
      case _ if middle.nonEmpty => label + positive.toString + middle.get() + negative.toString
      case _ => label + positive.toString + "N" + negative.toString

    }
  }
}


///////////////////////////////
// The main LBdd object
///////////////////////////////

object LBdd {

  var counter: Int = 0

  implicit def int2lbdd(raw: Int): LBdd = LBdd(raw)

  def apply(label: Int, b1: LBdd, b2: LBdd): LBdd = {
    LBdd(label, b1, None, b2)
  }

  def apply(label: Int): LBdd = {
    require(label != 0)
    if (label > 0)
      LBdd(label, LBddTrue, LBddFalse)
    else
      LBdd(-label, LBddFalse, LBddTrue)
  }

  def apply(label: Int, positive: LBdd,
            middle: lazyNode, negative: LBdd): LBdd = {
    if (positive == negative && middle.isEmpty)
      positive
    else
      LBddNode(label, positive, middle, negative)
  }
}


object LBddTest {

  def main(args: Array[String]): Unit = {
    LBdd(1)
    println(LBdd.counter)

    Or(LBdd(1), LBdd(2))
    println(LBdd.counter)
    And(Or(LBdd(1), LBdd(2)), Or(LBdd(1), LBdd(-2)))
    println(LBdd.counter)
    And(LBdd(1), LBdd(2), LBdd(3), LBdd(4))
    println(LBdd.counter)
  }
}
