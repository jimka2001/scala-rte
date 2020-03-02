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
//
// A significant portion of this typeclass definition was supplied by
// Justin du Coeur @jducoeur.
// The original text https://scastie.scala-lang.org/K2ar8VdMTpSUXqeJLkZcsQ
// was modified by @jducoeur https://scastie.scala-lang.org/9EFL87qySHSNMmG93LIvMQ
//

package treereduce
import scala.language.higherKinds // prevents IntelliJ from warning about [M[_]]

// This is the Typeclass. It is all about defining types that can be fed *into* TreeReduce.
trait TreeReducible[M[_]] {
  // This is the abstract function that you must define for any collection type M that you want to work with.
  // Note that the function takes an instance of M -- an actual collection instance -- as a parameter.
  def foldLeft[A, B](m: M[A])(z: B)(op: (B, A) => B): B
}

// This is where we define the instances of TreeReducible. Note that we have to define a separate one for
// Array, since that isn't a TraversableOnce.
object TreeReducible {
  // suggestion by sangamon, Patrick Römer
  // to use GenTraversableOnce to encompass Seq, ParSeq, and TraversableOnce
  // https://users.scala-lang.org/t/which-implicit-will-be-used/4863/9?u=jimka
  implicit def traversableOnceTreeReducible[T[X] <: scala.collection.GenTraversableOnce[X]]: TreeReducible[T] = new TreeReducible[T] {
    override def foldLeft[A, B](m: T[A])(z: B)(op: (B, A) => B): B = m.foldLeft(z)(op)
  }

  implicit val arrayTreeReducible: TreeReducible[Array] = new TreeReducible[Array] {
    override def foldLeft[A, B](m: Array[A])(z: B)(op: (B, A) => B): B = m.foldLeft(z)(op)
  }

//  implicit val mapTreeReducible: TreeReducible[Map] = new TreeReducible[Map] {
//    override def foldLeft[K,V,B](m: Map[K,V])(z:B)(op:(B,(K,V))=>B):B = m.foldLeft(z)(op)
//  }
}

// This is where we define the treeMapReduce() function itself. Note that it can be, and frequently is, in a totally
// separate place from TreeReducible. That's the point: we're separating the concerns, and keeping the function
// that *consumes* TreeReducible from the definition of TreeReducible and from the instances of it.
object TreeReduce {
  // The actual function. Note that it takes as parameter `m` any type for which there exists an implicit
  // TreeReducible, but you have to pass that *as* a parameter.
  def treeMapReduceIntern[A, B, M[_]](m: M[A])(init: B)(seqOp: A => B, combOp: (B, B) => B)(implicit reducible: TreeReducible[M]): B = {

    def consumeStack(stack: List[(Int, B)]): List[(Int, B)] = {
      stack match {
          //  Make sure to call combOp with b2,b1 (order reversed) because, stack has
          //    stored the elements in reverse order.  We don't know that combOp is
          //    commutative, so we have to maintain left-to-right as given in the original m.
        case (i, b1) :: (j, b2) :: tail if i == j => {
          consumeStack((i + 1, combOp(b2, b1)) :: tail)
        }
        case _ => stack
      }
    }

    val stack = reducible.foldLeft(m)((1, init) :: Nil) { (stack: List[(Int, B)], ob: A) =>
      consumeStack((1, seqOp(ob)) :: stack)
    }
    assert(stack != Nil)
    //println(s"    treeReduce cleanup: ${stack.length}")
    // At this point stack is a length of pairs of type (Int,B).
    //   The Ints are increasing from left to right, but not necessarily increasing by 1.
    //   The Int at head is >= 1, and the Int in the final element is <= log_2 N were N=length(m).
    //   So here we call reduce on a List[B] whose length <= log_2(N) << N.
    // stack.map(_._2) has type List[B] (independent of the type of `this`),
    // so we just need to fold those B's with combOp.  Since stack is not Nil
    // we can use reduce rather than fold.
    stack.map(_._2).reduce{(b1:B,b2:B) =>
      combOp(b2,b1)}
  }

  implicit class RichReducible[A, M[_]: TreeReducible](m: M[A]) {
    // The OO-syntax version of treeMapReduce().
    //
    // Changing the name here, to make clear what's what. Note that treeMapReduce() is just syntax sugar on top of
    // treeMapReduceIntern() -- the definition in here is what you have to call in order to do it without that
    // syntax sugar. treeMapReduce() could be named absolutely anything. All it does is let you use the
    // m.treeMapReduce() syntax, instead of having to say treeMapReduceIntern(m).
    def treeMapReduce[B](init: B)(seqOp: A => B, combOp: (B, B) => B): B =
      treeMapReduceIntern(m)(init)(seqOp, combOp)
  }
}
