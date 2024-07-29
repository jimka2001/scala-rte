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

package dimacs

import scala.annotation.tailrec

object perfTest{
  import dimacs.QmVec._

  def genCNF(numVars: Int, numClauses: Int, density: Int): CNF = {
    //println(s"generating $numClauses of $density terms each using $numVars variables")
    // density = number of terms per clause
    val prng = scala.util.Random

    @tailrec
    def randomList[A](size: Int, acc: Set[A], gen: () => A): List[A] = {
      if (size == acc.size)
        acc.toList
      else
        randomList(size, acc + gen(), gen)
    }

    def randomClause():ClauseAsList = {
      canonicalizeClause(for {
        i <- randomList(density, Set[Int](), () => 1 + prng.nextInt(numVars))
        sign = if (0 == prng.nextInt(2)) 1 else -1
      } yield sign * i)
    }

    randomList(numClauses, Set[ClauseAsList](), randomClause _)
  }


  def main(args: Array[String]): Unit = {

  }}
