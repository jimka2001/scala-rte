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

package treereduce

import scala.annotation.tailrec

object ListSpecificTreeReduce{
  def paired[A](data: List[A]): (List[(A, A)], Option[A]) = {
    val none: Option[A] = None
    val nil: List[(A, A)] = Nil
    data.foldLeft((nil, none)) {
      case ((stack: List[(A, A)], Some(b)), a) => (((b, a) :: stack), none)
      case ((stack: List[(A, A)], None), a) => (stack, Some(a))
    } match {
      case (stack, leftover) => (stack.reverse, leftover)
    }
  }

  def pairWiseFold[A](z: A)(mList: List[A], f: (A, A) => A): A = {
    val (pairs: List[(A, A)], leftover: Option[A]) = paired(mList)
    if (mList.isEmpty)
      z
    else {
      @scala.annotation.tailrec
      def recur(li: List[(A, A)], maybeB: Option[A]): A = {
        val reduced: List[A] = li.map { case (b1, b2) => f(b1, b2) }
        if (reduced.tail.isEmpty)
          maybeB match {
            case None => reduced.head
            case Some(b) => f(reduced.head, b)
          }
        else {
          val (pairs: List[(A, A)], leftover: Option[A]) = paired(reduced)
          val last: Option[A] = (leftover, maybeB) match {
            case (Some(b1), Some(b2)) => Some(f(b1, b2))
            case (None, Some(_)) => maybeB
            case (Some(_), None) => leftover
            case (None, None) => None
          }
          recur(pairs, last)
        }
      }

      recur(pairs, leftover)
    }
  }

  def treeFold[A](m: List[A])(z: A)(f: (A, A) => A): A = {

    @tailrec
    def consumeStack(stack: List[(Int, A)]): List[(Int, A)] = {
      stack match {
        case (i, b1) :: (j, b2) :: tail if i == j => consumeStack((i + 1, f(b2, b1)) :: tail)
        case _ => stack
      }
    }

    val stack = m.foldLeft((1, z) :: Nil) { (stack, ob) =>
      consumeStack((1, ob) :: stack)
    }

    stack.map(_._2).reduce { (a1, a2) => f(a2, a1) }
  }
}
