//// Copyright (c) 2019 EPITA Research and Development Laboratory
////
//// Permission is hereby granted, free of charge, to any person obtaining
//// a copy of this software and associated documentation
//// files (the "Software"), to deal in the Software without restriction,
//// including without limitation the rights to use, copy, modify, merge,
//// publish, distribute, sublicense, and/or sell copies of the Software,
//// and to permit persons to whom the Software is furnished to do so,
//// subject to the following conditions:
////
//// The above copyright notice and this permission notice shall be
//// included in all copies or substantial portions of the Software.
////
//// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
//// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
//// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
//// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
//// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
//// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
//// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
////
//// A significant portion of this typeclass definition was supplied by
//// Justin du Coeur @jducoeur.
//// The original text https://scastie.scala-lang.org/K2ar8VdMTpSUXqeJLkZcsQ
//// was modified by @jducoeur https://scastie.scala-lang.org/9EFL87qySHSNMmG93LIvMQ
////
//
//package typeclassconflicts
//
//// This file contains a pedagogical example taken from
////  https://scastie.scala-lang.org/jimka2001/zJFwwRBeT3uPiZdwSJjT4A
//// The demonstration is how adding new implicit instances can reduce
//// the applicability at call sites, in an unintuitive way.
//
//
//// This is the Typeclass. It is all about defining types that can be fed *into* TreeReduce.
//trait TreeReducable[M[_]] {
//  def foldLeft[A, B](m: M[A])(z: B)(op: (B, A) => B): B
//}
//
//object TreeReducable {
//  // implicit #1
//  implicit def traversableTreeReducable[T[X] <: TraversableOnce[X]] = new TreeReducable[T] {
//    override def foldLeft[A, B](m: T[A])(z: B)(op: (B, A) => B):B = m.foldLeft(z)(op)
//  }
//
//  // implicit #2
//  implicit val arrayTreeReducable: TreeReducable[Array] = new TreeReducable[Array] {
//    override def foldLeft[A, B](m: Array[A])(z: B)(op: (B, A) => B):B = m.foldLeft(z)(op)
//  }
//
//  // implicit #3
//  //implicit def iterableTreeReducable[T[X] <: Iterable[X]]: TreeReducable[T] = new TreeReducable[T] {
//  //  override def foldLeft[A, B](m: T[A])(z: B)(op: (B, A) => B): B = m.foldLeft(z)(op)
//  //}
//
//  // implicit #4
//  //implicit def seqTreeReducable[T[X] <: Seq[X]]: TreeReducable[T] = new TreeReducable[T] {
//  //  override def foldLeft[A, B](m: T[A])(z: B)(op: (B, A) => B): B = m.foldLeft(z)(op)
//  //}
//}
//
//object TreeReduce {
//  // The actual function. Note that it takes as parameter `m` any type for which there exists an implicit TreeReducable, but you
//  // have to pass that *as* a parameter.
//  def treeMapReduce[A, B, M[_]](m: M[A])(init: B)(seqop: A => B, combop: (B, B) => B)(implicit reducable: TreeReducable[M]): B = {
//
//    def revCombop(a:B,b:B):B = combop(b,a) // reverse arguments of combop because stack reverses order of elements
//
//    def consumeStack(stack: List[(Int, B)]): List[(Int, B)] = {
//      stack match {
//        case (i, a1) :: (j, a2) :: tail if i == j => consumeStack((i + 1, revCombop(a1, a2)) :: tail)
//        case _ => stack
//      }
//    }
//
//    val stack = reducable.foldLeft(m)((1, init) :: Nil) { (stack: List[(Int, B)], ob: A) =>
//      (1, seqop(ob)) :: consumeStack(stack)
//    }
//    assert(stack != Nil)
//    // stack.map(_._2) has type List[B] (indendent of the type of `this`),
//    // so we just need to fold those B's with revCombop.  Since stack is not Nil
//    // we can split it into stack.head and stack.tail for the fold.
//    stack.tail.map(_._2).fold(stack.head._2)(revCombop)
//  }
//
//  implicit class RichReducable[A, M[_]: TreeReducable](m: M[A]) {
//    def doReduce[B](init: B)(seqop: A => B, combop: (B, B) => B): B =
//      treeMapReduce(m)(init)(seqop, combop)
//  }
//}
//
//
//object Testing {
//  def main(argv: Array[String]): Unit = {
//    locally {
//      val loremBlock = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
//      val loremWords = loremBlock.split(" ")
//
//      val byFold = loremWords.map(_.length).foldLeft(0)(_ + _)
//
//
//      locally {
//        // This imports the TreeReducable instances.
//        import TreeReducable._
//        // This imports the doReduce() syntax.
//        import TreeReduce._
//
//        val byTypeClass = loremWords.doReduce(0)(_.length, _ + _)
//        assert(byFold == byTypeClass)
//
//
//        assert(byFold == loremWords.par.doReduce(0)(_.length, _ + _))
//        assert(byFold == loremWords.toList.par.doReduce(0)(_.length, _ + _))
//        assert(byFold == loremWords.toList.par.doReduce(0)(_.length, _ + _))
//
//
//        val range = 1 until 5
//        val reduced = range.doReduce(0)(_, _ + _)
//
//      }
//    }
//  }
//}
//
