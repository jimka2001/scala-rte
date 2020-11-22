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
import scala.language.higherKinds // prevents IntelliJ from warning about [M[_]]
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.mutable.ParArray


object TreeParallelReduce {
  def id[A](a: A): A = a

  def addInt(a: Int, b: Int): Int = a + b

  trait Pairable[F[_]] {

    def map[A, B: scala.reflect.ClassTag](p: F[A], f: A => B): F[B]

    def foldLeft[A, B](p: F[A], z: B)(f: (B, A) => B): B

    def paired[A, B](tree: F[A], f: A => B): (List[(B, B)], Option[B]) = {
      val none: Option[B] = None
      val nil: List[(B, B)] = Nil
      foldLeft(tree, (nil, none)) {
        case ((stack: List[(B, B)], Some(b)), a) => (((b, f(a)) :: stack), none)
        case ((stack: List[(B, B)], None), a) => (stack, Some(f(a)))
      } match {
        case (stack, leftover) => (stack.reverse, leftover)
      }
    }
  }
  import scala.collection.immutable.IndexedSeq
  implicit val indexedSeqPairable: Pairable[IndexedSeq] = new Pairable[IndexedSeq] {
    override def map[A, B: scala.reflect.ClassTag](it: IndexedSeq[A], f: A => B): IndexedSeq[B] = it.map(f)

    override def foldLeft[A, B](it: IndexedSeq[A], z: B)(f: (B, A) => B): B = it.foldLeft(z)(f)
  }
  implicit val iteratorPairable: Pairable[Iterator] = new Pairable[Iterator] {
    override def map[A, B: scala.reflect.ClassTag](it: Iterator[A], f: A => B): Iterator[B] = it.map(f)

    override def foldLeft[A, B](it: Iterator[A], z: B)(f: (B, A) => B): B = it.foldLeft(z)(f)
  }

  implicit val listPairable: Pairable[List] = new Pairable[List] {
    override def map[A, B: scala.reflect.ClassTag](seq: List[A], f: A => B): List[B] = seq.map(f)

    override def foldLeft[A, B](seq: List[A], z: B)(f: (B, A) => B): B = seq.foldLeft(z)(f)
  }
  implicit val parSeqPairable: Pairable[ParSeq] = new Pairable[ParSeq] {
    override def map[A, B: scala.reflect.ClassTag](seq: ParSeq[A], f: A => B): ParSeq[B] = seq.map(f)

    override def foldLeft[A, B](seq: ParSeq[A], z: B)(f: (B, A) => B): B = seq.foldLeft(z)(f)
  }
  implicit val parArrayPairable: Pairable[ParArray] = new Pairable[ParArray] {
    override def map[A, B: scala.reflect.ClassTag](seq: ParArray[A], f: A => B): ParArray[B] = seq.map(f)

    override def foldLeft[A, B](seq: ParArray[A], z: B)(f: (B, A) => B): B = seq.foldLeft(z)(f)
  }
  implicit val arrayPairable: Pairable[Array] = new Pairable[Array] {
    override def map[A, B: scala.reflect.ClassTag](arr: Array[A], f: A => B): Array[B] = arr.map(f)

    override def foldLeft[A, B](arr: Array[A], z: B)(f: (B, A) => B): B = arr.foldLeft(z)(f)
  }

  def pairMapReduce[M[_], A, B](m: M[A])(init: B, seqOp: A => B, combOp: (B, B) => B)(implicit pairable: Pairable[M]): B = {
    // This is a special case of pairReduce_* for which seqOp is the identity,
    //   and thus we don't need to pass it along and don't need to call it.
    // Combine adjacent objects using combOp so as to reduce the number of objects to 50%
    // e.g., List(1,2,3,4,5,6,7,8) --> List(1+2,3+4,5+6,7+8) --> List(3,7,11,15)
    // This reduction is done in parallel using toList.par.map.
    // And then make recursive call to continue the reduction until only one
    // object remains.
    val (mList: List[(B, B)], leftover: Option[B]) = pairable.paired(m, seqOp)
    if (mList.isEmpty)
      init
    else {
      @scala.annotation.tailrec
      def recur(li: List[(B, B)], maybeB: Option[B]): B = {
        val reduced: List[B] = li.map { case (b1: B, b2: B) => combOp(b1, b2) }
        if (reduced.tail.isEmpty)
          maybeB match {
            case None => reduced.head
            case Some(b) => combOp(reduced.head, b)
          }
        else {
          val (pairs: List[(B, B)], leftover: Option[B]) = listPairable.paired[B,B](reduced,id)
          val last: Option[B] = (leftover, maybeB) match {
            case (Some(b1), Some(b2)) => Some(combOp(b1, b2))
            case (None, Some(_)) => maybeB
            case (Some(_), None) => leftover
            case (None, None) => None
          }
          recur(pairs, last)
        }
      }
      recur(mList, leftover)
    }
  }

  def main(argv: Array[String]): Unit = {
    import scala.collection.parallel.CollectionConverters._

    val data = (1 to 100).toList

    println("result Nil List = " + pairMapReduce(List[Int]())(init = 0, id, addInt))
    println("result Nil Array = " + pairMapReduce(Array[Int]())(init = 0, id, addInt))
    println("result List    =" + pairMapReduce(data)(init = 0, id, addInt))
    println("result Array   =" + pairMapReduce(data.toArray)(init = 0, id, addInt))
    println("result ParList  =" + pairMapReduce(data.par)(init = 0, id, addInt))
    println("result ParArray=" + pairMapReduce(data.toArray.par)(init = 0, id, addInt))
    //println("result ?? range=" + pairMapReduce(1 to 100)(init = 0, id, addInt))
    println("result Iterator=" + pairMapReduce((1 to 100).iterator)(init = 0, id, addInt))
    println("result Intern3 =" + pairMapReduce(data)(0, id, addInt))
  }
}
