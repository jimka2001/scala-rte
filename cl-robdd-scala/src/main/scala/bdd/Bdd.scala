// Copyright (c) 2019 EPITA Research and Development Laboratory
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

package bdd

import scala.annotation.tailrec

sealed abstract class Bdd {

  def subsetp(that: Bdd): Boolean = {
    BddFalse eq AndNot(this, that)
  }

  def supersetp(that: Bdd): Boolean = {
    that.subsetp(this)
  }

  def equivalentBySubset(that: Bdd): Boolean = {
    subsetp(that) && supersetp(that)
  }

  // DFS walk the given bdd, calling the client function on each Assignment
  // terminating by a BddTrue node.   I.e., each variable assignment which makes
  // the corresponding Boolean function true.
  // The given client function is called an each such assignment.
  //   TODO, we need a cousin function of visitTerms which rather than
  //      visiting every assignment which satisfies the Boolean equation,
  //      rather just return any such assignment.
  def visitSatisfyingAssignments(client:(Assignment,Assignment)=>Unit):Unit = {
    def recur(bdd: Bdd, assignTrue: Assignment, assignFalse:Assignment): Unit = {
      bdd match {
        case BddFalse => ()
        case BddTrue => client(assignTrue,assignFalse)
        case BddNode(label, positive, negative) => {
          recur(positive, Assignment(assignTrue.trueVariables + label),assignFalse)
          recur(negative, assignTrue, Assignment(assignFalse.trueVariables + label))
        }
      }
    }
    recur(this, Assignment(Set[Int]()), Assignment(Set[Int]()))
  }

  def findSatisfyingAssignment():Option[(Assignment,Assignment)] = {
    def recur(bdd: Bdd, assignTrue: Assignment, assignFalse:Assignment): Option[(Assignment,Assignment)] = {
      bdd match {
        case BddFalse => None
        case BddTrue => Some((assignTrue,assignFalse))
        case BddNode(label, positive, negative) => {
          recur(positive, Assignment(assignTrue.trueVariables + label),assignFalse) orElse
            recur(negative, assignTrue, Assignment(assignFalse.trueVariables + label))
        }
      }
    }
    recur(this, Assignment(Set[Int]()), Assignment(Set[Int]()))
  }

  def toDnf():String = toDnf("x")
  def toDnf(x:String):String = {
    import Assignment.toMinTerm
    import accumulators.Accumulators.withCollector

    val substrings:List[String] = withCollector(collect =>
      visitSatisfyingAssignments{(t:Assignment,f:Assignment) => collect(toMinTerm(x,t,f))})

    substrings.mkString(" \u2228 ") // concatenate minterms with logical V operator as delimiter.
  }

  def fold[R](z: R)(f: (R, Bdd) => R): R = {
    def bfsWalk(f: Bdd => Unit): Unit = {
      def recur(generation: List[Bdd], nextGeneration: List[Bdd], done: Set[Bdd]): Unit = {
        // BFS walk of bdd, calling f exactly once on each node
        (generation, nextGeneration) match {
          case (Nil, Nil) => Unit
          case (Nil, _) => recur(nextGeneration, Nil, done)
          case (h :: tail, _) if done.contains(h) => recur(tail, nextGeneration, done)
          case (h :: tail, _) =>
            f(h)
            recur(tail,
                  h match {
                    case BddNode(_, pos, neg) => pos :: neg :: nextGeneration
                    case _ => nextGeneration
                  },
                  done + h
                  )
        }
      }

      recur(List(this), Nil, Set())
    }

    var ret: R = z
    bfsWalk { b: Bdd =>
      ret = f(ret, b)
    }
    ret
  }

  def size():Int = {
    var visited: Set[Bdd] = Set()

    def recur(bdd: Bdd, n: Int): Int = {
      if (visited.contains(bdd))
        n
      else {
        visited = visited + bdd
        bdd match {
          case _: BddTerm => 1 + n
          case bdd: BddNode => {
            recur(bdd.negative, recur(bdd.positive, 1 + n))
          }
        }
      }
    }

    recur(this, 0)
  }


  def apply(assignments: Assignment): Boolean

}
////////////////////////////////
// Bdd Terminal nodes
////////////////////////////////

sealed abstract class BddTerm extends Bdd {
}

object BddTrue extends BddTerm {
  override def toString = "\u22a4"

  override def apply(assignments:Assignment):Boolean = {
    true
  }

}

object BddFalse extends BddTerm {
  override def toString = "\u22A5"

  def apply(assignments:Assignment):Boolean = {
    false
  }
}

/////////////////////////////
// Bdd Internal Nodes
/////////////////////////////


case class BddNode(label:Int, positive:Bdd, negative:Bdd) extends Bdd {
  // equals and hashCode methods copied from here:
  //   https://users.scala-lang.org/t/what-does-equivalence-of-instances-mean/4368/11?u=jimka
  override def equals(that: Any): Boolean = {
    that match {
      case b: Bdd => this eq b
      case _ => false
    }
  }

  override def hashCode(): Int =
    System.identityHashCode(this)

  def validateChild(child: Bdd): Unit = {
    // a BddTerm may be a child of any BddNode
    // but if a BddNode is a child, then the ident of the child must be strictly > ident of parent
    child match {
      case child: BddNode => assert(child.label > label,
                                    "expecting child.label > this.label, got " + child.label + "<=" + label)
      case _: BddTerm => Unit
    }
  }

  validateChild(positive)
  validateChild(negative)

  override def toString:String = {
    (positive, negative) match {
      case (BddTrue, BddFalse) => label.toString
      case (BddFalse, BddTrue) => "!" + label
      case (BddTrue, _) => "{" + label + "-" + negative + "}"
      case (_, BddFalse) => "{" + label + "+" + positive + "}"
      case (_, _) => "{" + label + "+" + positive + "-" + negative + "}"
    }
  }

  def apply(assignment: Assignment): Boolean = {
    // Given a set of assignments, evaluate the underlying Boolean function
    //  to arrive at true or false.  This is done tail-recursively by
    //  marching down exactly one path from the root node to some terminal
    //  node.  at each internal node walking left or right, depending on
    //  whether the variable corresponding to the current node has a
    //  true or false assignment, i.e., whether that variable is a member
    //  of the assignments set or not.
    if (assignment.value(label))
      positive(assignment) // call the apply method of the positive child bdd node
    else
      negative(assignment) // call the apply method of the negative child bdd node
  }
}

//////////////////////////////
// The main Bdd object
//////////////////////////////

object Bdd {
  implicit def int2bdd(raw: Int): Bdd = Bdd(raw) // allow varargs And,Or,etc e.g., And(1), Or(1,2,3), And(1,Or(2,3))

  def apply(label: Int): Bdd = {
    require(label != 0)
    if (label > 0)
      Bdd(label, BddTrue, BddFalse) // calls Bdd.apply 3-arg version
    else // ( label < 0)
      Bdd(-label, BddFalse, BddTrue) // calls Bdd.apply 3-arg version
  }

  import scala.util.DynamicVariable

  type BDD_HASH = scala.collection.mutable.Map[(Int, Bdd, Bdd), BddNode]
  val maybeNodeHash = new DynamicVariable[Option[BDD_HASH]](None)
  var numAllocations = new DynamicVariable[Long](0L)

  // this function is provided for debug purposes, to allow memory allocation
  // monitoring during intense computations.
  def getBddSizeCount():Option[(Long,Long)] = {
    for{ hash <- maybeNodeHash.value}
      yield (hash.size, numAllocations.value)
  }

  // wrapper for all code which needs to calculate with Bdds.  The Bdd
  //   calculations need a hash map to enforce structural identity.
  //   Attempt to construct a Bdd outside the dynamic extend of this
  //   function results in an error.
  def withNewBddHash[A](code: => A): A = {
    numAllocations.withValue(0L) {
      maybeNodeHash.withValue(Some(newHash())) {
        code
      }
    }
  }

  def newHash(): BDD_HASH = {
    // this code was suggested by Patrick Römer
    // https://users.scala-lang.org/t/id-like-a-weak-hash-table-which-allows-gc-when-value-is-otherwise-unreferenced/4681/8?u=jimka
    import org.jboss.util.collection._

    import scala.collection.JavaConverters._
    new WeakValueHashMap[(Int, Bdd, Bdd), BddNode].asScala
  }

  // constructor Bdd(var,bddPositive,bddNegative)
  def apply(label: Int, positive: Bdd, negative: Bdd): Bdd = {
    if (positive == negative)
      positive
    else {
      maybeNodeHash.value match {
        case None => sys.error("Bdd constructor called outside dynamic extent of withNewBddHash(...)")
        case Some(hash) =>
          hash.get((label, positive, negative)) match {
            case Some(bdd: BddNode) => bdd
            case None | Some(null) =>
              val bdd = BddNode(label, positive, negative)
              numAllocations.value = 1L + numAllocations.value
              //if (0 == numAllocations.value % 1000000)
              //  println(s"numAllocations = ${numAllocations.value} hash=${hash.size} " + 100 * hash.size.toDouble/numAllocations.value)
              hash((label, positive, negative)) = bdd
              bdd
          }
      }
    }
  }
}

// Obligatory object with main

object BddTest {

  def main(args: Array[String]): Unit = {

    Bdd.withNewBddHash {
      Bdd(1)
      println(BddTrue)
      println(BddFalse)
      println(Or(Bdd(1),Bdd(2)).toDnf())
      println(Or(Bdd(1),Bdd(2)).toDnf())
      println(And(Or(Bdd(1),Bdd(2)),Or(Bdd(1),Bdd(-2))).toDnf())
      println(And(Or(Bdd(1),Bdd(2)),Or(Bdd(147),Bdd(-23))).toDnf("A"))
    }
  }
}

