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


package treereduce

import adjuvant.MyFunSuite
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

class TreeReduceSuite extends MyFunSuite {
  test("lorem ipsum") {

    val loremBlock = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    val loremWords = loremBlock.split(" ")

    val byFold = loremWords.map(_.length).foldLeft(0)(_ + _)

    locally {
      val byRef = Reference.treeMapReduce(loremWords)(0)(_.length, _ + _)
      assert(byFold == byRef)
    }

    locally {
      // This imports the TreeReducible instances.
      import treereduce.TreeReducible._
      // This imports the obj.treeMapReduce() syntax.
      import treereduce.TreeReduce._

      // by type class val byTypeClass =
      // IntelliJ marks the following as error, but it works fine.  This must be an IntelliJ bug.
      assert(byFold == loremWords.treeMapReduce(0)(_.length, _ + _))
      assert(byFold == loremWords.toList.treeMapReduce(0)(_.length, _ + _))
      assert(byFold == loremWords.treeMapReduce(0)(_.length, _ + _))
    }
  }
  test("sets") {
    // This imports the TreeReducible instances.
    import treereduce.TreeReducible._
    // This imports the obj.treeMapReduce() syntax.
    import treereduce.TreeReduce._
    import scala.collection.parallel.CollectionConverters._
    def id(i: Int): Int = i

    for {
      max <- 1 to 100
      nums = 1 to max
      sum = nums.sum
    } {
      assert(sum == nums.fold(0) {
        _ + _
      })
      assert(sum == nums.toSet.sum)
      assert(sum == nums.treeMapReduce(0)(id, _ + _))
      assert(sum == nums.toList.treeMapReduce(0)(id, _ + _))
      assert(sum == nums.toSet.treeMapReduce(0)(id, _ + _))
      assert(sum == nums.iterator.treeMapReduce(0)(id, _ + _))
      assert(sum == nums.par.toIterator.treeMapReduce(0)(id, _ + _))

      assert(sum == nums.par.treeMapReduce(0)(id, _ + _))
      assert(sum == nums.toList.par.treeMapReduce(0)(id, _ + _))
      assert(sum == nums.iterator.treeMapReduce(0)(id, _ + _))
    }

  }


  test("intervals") {
    def id(i: (Int, Int)): (Int, Int) = i

    def mergeIntervals(interval1: (Int, Int), interval2: (Int, Int)): (Int, Int) = {
      // merge intervals is a non-commutative function. we thus test whether
      //   treeMapReduce respects the order of the arguments in the same way
      //   that foldLeft does.
      // (1 3) merge (3 9) --> (1 9)
      // (1 3) merge (4 6) --> error, can only merge if intermediate bound is equal
      // (3 1) merge x --> error, not an interval
      // x merge (3 1) --> error, not an interval
      val (a_lower, a_upper) = interval1
      val (b_lower, b_upper) = interval2
      assert(a_upper == b_lower, s"adjacent intervals not compatible: $interval1 $interval2")
      assert(a_upper >= a_lower, s"not an interval $interval1")
      assert(b_upper >= b_lower, s"not an interval $interval2")
      (a_lower, b_upper)
    }

    for {
      upper <- 1 to 500
      intervals = (0 to upper).map(i => (i, i + 1))
    } {
      val byFold = intervals.foldLeft((0, 0))(mergeIntervals)
      locally {
        import treereduce.TreeReduce._
        import treereduce.TreeReducible._

        assert(byFold == Reference.treeMapReduce(intervals)((0, 0))(id, mergeIntervals))
        assert(byFold == intervals.treeMapReduce((0, 0))(id, mergeIntervals))
      }
    }
  }

  def time[R](_name: String, block: => R): R = {
    //val t0 = System.nanoTime()
    val result = block // call-by-name
    //val t1 = System.nanoTime()
    //    println(s"$name: Elapsed time: ${(t1 - t0) / 1.0e6} ms")
    result
  }

  test("rational numbers") {
    import spire.implicits._
    import spire.math._
    import treereduce.TreeReduce._
    import treereduce.TreeReducible._

    val zero = Rational(0,1)
    for {r <- List(10, 20, 40, 75, 100, 200, 400, 750, 1000, 2000, 4000)} {
      val piercedInterval = (-r to -1) ++ (1 to r)
      time(s"$r  treeMapReduce", {
        val sum = piercedInterval.treeMapReduce(Rational(0, 1))(Rational(1, _), _ + _)
        assert(sum === zero)
      })
//      time(s"$r treeMapReduce2", {
//        val sum = bdd.Aux.treeMapReduceIntern2(piercedInterval.toList)(zero)(Rational(1,_), _+_)
//        assert(sum === zero)
//      })
      time(s"$r           fold", {
        val sum = piercedInterval.map {
          Rational(1, _)
        }.foldLeft(Rational(0, 1))(_ + _)
        assert(sum === zero)
      })
    }
  }
  test("tree map reduce 1 item"){
    import treereduce.TreeReducible._
    // This imports the obj.treeMapReduce() syntax.
    import treereduce.TreeReduce._
    val sample: List[Int] = List(13)

    def addInt(a: Int, b: Int): Int = a + b

    def id(i: Int): Int = i

    assert(sample.treeMapReduce(0)(id, addInt) == 13)
  }
  test("tree map reduce 2 items"){
    import treereduce.TreeReducible._
    // This imports the obj.treeMapReduce() syntax.
    import treereduce.TreeReduce._
    val sample: List[Int] = List(13,14)

    def addInt(a: Int, b: Int): Int = a + b

    def id(i: Int): Int = i

    assert(sample.treeMapReduce(0)(id, addInt) == 27)
  }

  test("tree map reduce 166") {
    // This imports the TreeReducible instances.
    import treereduce.TreeReducible._
    // This imports the obj.treeMapReduce() syntax.
    import treereduce.TreeReduce._
    def addInt(a: Int, b: Int): Int = a + b
    def id(i: Int): Int = i

    for {hi <- 1 to 32
         sample = (1 to hi).toList
         } {
      //println("n=" + sample.size)
      assert(sample.treeMapReduce(0)(id, addInt) == sample.sum)
    }
  }

  test("tree map reduce 1") {
    // This imports the TreeReducible instances.
    import treereduce.TreeReducible._
    // This imports the obj.treeMapReduce() syntax.
    import treereduce.TreeReduce._
    val sample: List[Int] = (0 to 1000).toList

    def addInt(a: Int, b: Int): Int = a + b

    def id(i: Int): Int = i

    sample.tails.foreach { nums => {
      assert(nums.foldLeft(0)(addInt) == nums.treeMapReduce(0)(id, addInt))
    }
    }
    assert(List[Int]().treeMapReduce(0)(id, addInt) == 0)
  }

  test("tree map reduce non-commutative") {
    // This imports the TreeReducible instances.
    import treereduce.TreeReducible._
    // This imports the obj.treeMapReduce() syntax.
    import treereduce.TreeReduce._
    def id(i: (Int, Int)): (Int, Int) = i

    def mergeIntervals(interval1: (Int, Int), interval2: (Int, Int)): (Int, Int) = {
      // merge intervals is a non-commutative function. we thus test whether
      //   treeMapReduce respects the order of the arguments in the same way
      //   that foldLeft does.
      val (a_lower, a_upper) = interval1
      val (b_lower, b_upper) = interval2
      assert(a_upper == b_lower, s"adjacent intervals not compatible: $interval1 $interval2")
      assert(a_upper >= a_lower)
      assert(b_upper >= b_lower)
      (a_lower, b_upper)
    }

    for {
      upper <- 1 to 1000
      intervals = (0 to upper).map(i => (i, i + 1)) //.toList
    } {
      assert(intervals.foldLeft((0, 0))(mergeIntervals) == intervals.treeMapReduce((0, 0))(id, mergeIntervals))
    }
  }
  test("tree map reduce 3") {
    // This imports the TreeReducible instances.
    import treereduce.TreeReducible._
    // This imports the obj.treeMapReduce() syntax.
    import treereduce.TreeReduce._

    val loremBlock = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    val loremWords:Array[String] = loremBlock.split(" ")
    // IntelliJ thinks treeMapReduce is an unresolved symbol, but that's a bug in IntelliJ
    //   similar for length and +
    assert(loremWords.map(_.length).foldLeft(0)(_ + _) == loremWords.treeMapReduce(0)(_.length, _ + _))
  }
}


object Reference {
  // This is the reference implementation of treeMapReduce.
  // This is in fact the code I would like to remove once it has been generalized as a type class.
  // For the moment the code remains because it us used in the tests to assert the same results
  //   are computed.
  def treeMapReduce[A, B](objects: IterableOnce[A])(init: B)(seqOp: A => B, combOp: (B, B) => B): B = {
    // we have to assume that combOp is NOT commutative, so we have to be
    // careful and apply the arguments in the correct order.
    // because the stack is storing items in *reverse* order, we have to reverse the
    // the arguments of combOp to have combOp(a2,a1) rather than combOp(a1,a2)

    @tailrec
    def consumeStack(stack: List[(Int, B)]): List[(Int, B)] = {
      stack match {
        case (i, a1) :: (j, a2) :: tail if i == j => consumeStack((i + 1, combOp(a2, a1)) :: tail)
        case _ => stack
      }
    }

    val stack = objects.iterator.foldLeft((1, init) :: Nil) { (stack: List[(Int, B)], ob: A) =>
      (1, seqOp(ob)) :: consumeStack(stack)
    }
    // there may be up to log_2 of objects.size many pairs left on the stack
    // but there is not precisely 0 such pairs on the stack.
    assert(stack != Nil)
    // stack.map(_._2) has type List[B] (independent of the type of objects),
    // so we just need to fold those B's with revCombOp.  Since stack is not Nil
    // we can use reduce rather than fold
    stack.map(_._2).reduce{(a1,a2) => combOp(a2,a1)}
  }
}
