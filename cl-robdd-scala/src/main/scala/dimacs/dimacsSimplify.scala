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

import dimacs.QmVec._

object dimacsSimplify {
  //                         clause, posCount, length, rectified
  type HashUpdateFunction = (ClauseAsList, Int, Int, ClauseAsList) => Unit

  def isCompatible2(clause1: ClauseAsList, clause2: ClauseAsList): Boolean = {
    // this function is used for testing, should be called in live application
    val rectified: ClauseAsList = rectify(clause1)
    assert(rectified == rectify(clause2))
    isCompatible(clauseToBitSet(clause1), clauseToBitSet(clause2))
  }

  def isCompatible(clause1: ClauseAsBitSet, clause2: ClauseAsBitSet): Boolean = {
    // calculate exclusive or, and test its size to be exactly 1
    (clause1 ^ clause2).size == 1
  }
  // Given two clauses (e.g., (1 -2 3 4) and (1 2 3 4),
  // generate a new clause which contains the integers which are common to the two clauses.
  // e.g., (1 2 4).  clause1 and clause2 are represented by a BitSets of positions of positive
  // integers in the rectified Clause.  For example if rectified=List(1,4,5,8), clause1=BitSet(0,1,2),
  // represents List(1,4,5,-8) and clause2=(0,2) represents List(1,-4,5,-8)
  def reduceOneVar(rectified: ClauseAsList, clause1: ClauseAsBitSet, clause2: ClauseAsBitSet): (ClauseAsBitSet, ClauseAsList) = {
    val badIndex = (clause1 ^ clause2).head
    val (prefix,suffix) = rectified.splitAt(badIndex)
    ((clause1 & clause2).map{p => if (p < badIndex) p else p-1}, // new clause
      prefix ++ suffix.tail // new rectified
    )
  }

  def quineMcCluskeyReduceAndSortResults(vec: QmVec): CNF = {
    vec.qmReduce()

    var clauses: CNF = Nil
    vec.mapClauses((clause: ClauseAsList) => clauses = clause :: clauses)
    clauses.sortWith((clause1, clause2) =>
                       if (clause1.length != clause2.length)
                         clause1.length <= clause2.length
                       else clauseLess(clause1, clause2))
  }

  def quineMcCluskeyReduce(clauses: CNF): CNF = {
    quineMcCluskeyReduceAndSortResults(QmVec(clauses))
  }

  // The function, crossCompatiblize, effectively calculates the
  // following quadratic complexity concentric foreach loop, calling
  //    removeFunction and addFunction whenever it finds aa and bb which are qmIsCompatible
  // Even though this foreach/foreach loop is theoretically n^2 complexity
  //   the overall goal of our implementation of the Quine McCluskey algorithm
  //   is to limit the size of these sequences so that you have many small n^2 searches
  //   rather than one large one.
  def crossCompatiblize(cla: Set[ClauseAsBitSet], clb: Set[ClauseAsBitSet], posCount: Int, length: Int, rectified: ClauseAsList): RemoveAdd = {
    val compatiblePairs = for {
      aa <- cla.toIterator
      bb <- clb
      if isCompatible(aa, bb)
    } yield (aa, bb)

    compatiblePairs.foldLeft(RemoveAdd.nil) { case (RemoveAdd(removes, adds), (aa, bb)) =>
      val (reducedClause,reducedRectified) = reduceOneVar(rectified,aa, bb)
      RemoveAdd(ClauseDesignator(aa, posCount, length, rectified) :: ClauseDesignator(bb, posCount - 1, length, rectified) :: removes,
                ClauseDesignator(reducedClause, posCount - 1, length - 1, reducedRectified) :: adds)
    }
  }

  def main(argv: Array[String]): Unit = {
    val vec = QmVec(Nil)
    vec.addClause1(List(1, 2, 3, -4))
    println(vec.hash)
    vec.addClause1(List(1, 2, -3, 4))
    println(vec.hash)
    vec.addClause1(List(1, 2, -3, -4))
    println(vec.hash)
  }
}
