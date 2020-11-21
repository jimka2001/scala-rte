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

import cl.CLcompat._
import dimacs.QmVec._

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.math._

case class ClauseDesignator(clause:ClauseAsBitSet, posCount:Int, length:Int, rectified:ClauseAsList)
case class RemoveAdd(removes:List[ClauseDesignator], adds:List[ClauseDesignator])
object RemoveAdd {
  val nil: RemoveAdd = RemoveAdd(Nil, Nil)
}

case class Problem(problemType:String, numVars:Int, numClauses:Int)
object Problem {
  val nil: Problem = Problem("", 0, 0)
}
case class DimacsFile(problem:Problem, clauses:List[ClauseAsList])

class QmVec() {

  type RECTHASH = mutable.HashMap[ClauseAsList, Set[ClauseAsBitSet]]
  type LENGTHHASH = mutable.HashMap[Int, RECTHASH]
  val hash: mutable.HashMap[Int, LENGTHHASH] = new mutable.HashMap

  // Add the given clause, in the correct place, into hash.   This function,
  // addClause, takes redundant information in its arguments.  We assume this redundant
  // information is correct (not contradictory).  Use other arity versions of addClause
  // in the case you need to calculate posCount, length, or rectified.
  // * posCount is the number of positive integers in addMe
  // * length is the length of addMe
  // * rectified is a Clause of all positive integers.
  //        E.g., if addMe=List(1,-2,-3), then rectified=List(1,2,3)
  def addClause(designator:ClauseDesignator): Unit = {
    val ClauseDesignator(addMe, posCount, length, rectified) = designator
    val lengthHash = hash.getOrElseUpdate(posCount, new mutable.HashMap)
    val rectHash = lengthHash.getOrElseUpdate(length, new mutable.HashMap)

    rectHash(rectified) = rectHash.get(rectified) match {
      case None => Set(addMe)
      case Some(clauses) => clauses + addMe
    }
  }

  // the unary interface to addClause, which calculates posCount, length, and rectified
  def addClause1(addMe: ClauseAsList): Unit = {
    // we assume the addMe clause as already been canonicalized via canonicalizeClause
    addClause(ClauseDesignator(clauseToBitSet(addMe), countPositive(addMe), addMe.length, rectified = rectify(addMe)))
  }

  // remove the given clause, removeMe, from the appropriate position in hash.
  // clean up the hash if this removal makes a key map to empty hash or empty set
  def removeClause(designator:ClauseDesignator):Unit = {
    val ClauseDesignator(removeMe, numPos, length, rectified) = designator
    for {
      lengthHash <- hash.get(numPos)
      rectHash <- lengthHash.get(length)
      _ <- rectHash.get(rectified)
    } {
      rectHash(rectified) -= removeMe
      if (rectHash(rectified).isEmpty ){
        rectHash -= rectified
        if (lengthHash(length).isEmpty) {
          lengthHash -= length
          if (hash(numPos).isEmpty)
            hash -= numPos
        }
      }
    }
  }

  // the unary interface to removeClause, which calculates posCount, length, and rectified
  def removeClause1(removeMe: ClauseAsList): Unit = {
    // we assume the removeMe clause as already been canonicalized via canonicalizeClause

    removeClause(ClauseDesignator(clauseToBitSet(removeMe), countPositive(removeMe), removeMe.length, rectify(removeMe)))
  }

  // Call the given function, consume, on all the clauses in the hash.
  // This function is called for side-effect only.
  def mapClauses(consume: ClauseAsList => Unit): Unit = {
    for {
      (_, lengthHash) <- hash
      (_, rectHash) <- lengthHash
      (rectified, set: Set[ClauseAsBitSet]) <- rectHash
      b <- set
    } consume(bitSetToClause(rectified,b))
  }

  // Given an integer, posCount, compare the clauses with posCount=posCount and posCount=posCount-1
  // to find pairs of clauses which differ by exactly one component, and more precisely differ
  // only in absolute value.  E.g., If (1,2,3,4) and (1,2,-3,4) are both clauses in the hash
  // then this pair can be reduced to (1,2,4) because the two clauses differ only by the fact that
  // one contains 3 and the other contains -3.  Other than that difference, they clauses
  // are the same length (length=4) and all the other elements (1,2,4) are common to both.
  // This assumes that the clauses have already been sorted in increasing order by absolute value.
  // Once such a pair is found (1,2,3,4) and (1,2,-3,4) we need to delete them and add (1,2,4).
  // However, we can't delete them immediately because that would interfere with finding other pairs
  // such as (1,2,3,4) and (1,2,3,-4).  So rather than deleting immediately, instead we schedule them
  // for deletion, by calling removeFunction.   However, (1,2,4) is added immediately, by calling
  // addFunction.
  def reducePosCountClauses(posCount: Int): RemoveAdd = {
    import accumulators.Accumulators._
    import dimacs.dimacsSimplify.crossCompatiblize

    foldUps(withCollector(collect =>
                            for {
                              lengthHashB <- hash.get(posCount - 1)
                              lengthHashA <- hash.get(posCount)
                              (length, rectHashA) <- lengthHashA
                              rectHashB <- lengthHashB.get(length)
                              (rectified, cla) <- rectHashA
                              if cla.nonEmpty
                              clb <- rectHashB.get(rectified)
                              if clb.nonEmpty
                            } collect(crossCompatiblize(cla, clb, posCount, length, rectified))))
  }

  def reducePosCountClausesIncremental(posCount: Int, length: Int, rectified: ClauseAsList): RemoveAdd = {
    import dimacs.dimacsSimplify.crossCompatiblize
    val maybeClb = for {
      lengthHash <- hash.get(posCount - 1)
      rectHash <- lengthHash.get(length)
      cl <- rectHash.get(rectified)
    } yield cl

    maybeClb match {
      case None => RemoveAdd.nil
      case Some(clb) if clb.nonEmpty =>
        // by checking maybeClb first (before maybeCla) then we avoid
        //  actually generating cla in the case that clb is the empty set
        val maybeCla = for {
          lengthHash <- hash.get(posCount)
          rectHash <- lengthHash.get(length)
          cl <- rectHash.get(rectified)
        } yield cl
        maybeCla match {
          case Some(cla) if cla.nonEmpty =>
            crossCompatiblize(cla, clb, posCount, length, rectified)
          case _ =>
            RemoveAdd.nil
        }
    }
  }

  def qmReduce(): Unit = {
    val maxPosCount: Int = hash.foldLeft(0) { case (acc, (k, _)) => max(k, acc) }

    val posCounts: Iterable[Int] = for {
      (posCount, _) <- hash
      if posCount <= maxPosCount && posCount > 0
    } yield posCount
    import scala.collection.parallel.CollectionConverters._
    val RemoveAdd(removes, adds) = foldUps(posCounts.par.map(reducePosCountClauses).toList)

    def calcNextPhase(removes: List[ClauseDesignator], adds: List[ClauseDesignator]): Set[(Int, Int, ClauseAsList)] = {

      removes.foreach(removeClause)
      adds.foreach(addClause)

      adds.map { case ClauseDesignator(_, posCount, length, rectified) => (posCount, length, rectified) }.toSet
    }

    @tailrec
    def loop(posCounts: Set[(Int, Int, ClauseAsList)]): Unit = {

      if (posCounts.nonEmpty) {
        val RemoveAdd(removes, adds) = foldUps(posCounts.par.map((reducePosCountClausesIncremental _).tupled).toList)
        loop(calcNextPhase(removes, adds))
      }
    }

    loop(calcNextPhase(removes, adds))
  }

  def printVec(): Unit = {
    for ((posCount, lengthHash) <- hash) {
      println("posCount:" + posCount)
      for ((length, clauses) <- lengthHash) {
        println("  " + length + ": " + clauses)
      }
    }
  }
  def writeDimacs(fileName:String):(Int,Int) = {
    writeDimacs(fileName,None)
  }
  def writeDimacs(fileName:String, comment:Option[List[String]]):(Int,Int) = {
    import java.io._

    import accumulators.Accumulators.{withSetCollector, withSummer}

    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    val numVars:Int = withSetCollector{collect:(Int =>Unit) => for {
      (_, lengthHash) <- hash
      (_, rectHash) <- lengthHash
      (rectified, _) <- rectHash
      literal <- rectified
    } collect(literal) }.size
    val numClauses:Int = withSummer(0, (a:Int,b:Int)=>a+b)(sum => for {
      (_, lengthHash) <- hash
      (_, rectHash) <- lengthHash
      (_, clauses: Set[ClauseAsBitSet]) <- rectHash
    } sum(clauses.size) )
    // write comments
    for{
      comments <- comment
      line <- comments
    } bw.write(s"c $line\n")
    // write problem line
    bw.write(s"p cnf $numVars $numClauses\n")
    def writeClause(clause:ClauseAsList):Unit = {
      bw.write(clause.mkString(" ") + " 0\n")
    }
    for {
      (_, lengthHash) <- hash
      (_, rectHash) <- lengthHash
      (rectified, bitsets) <- rectHash
      b <- bitsets
    } writeClause(bitSetToClause(rectified,b))

    bw.close()
    (numVars, numClauses)
  }
}

object QmVec {
  type ClauseAsBitSet = BitSet
  type ClauseAsList = List[Int]
  type CNF = List[ClauseAsList]

  def apply(dimacs:DimacsFile): QmVec = {
    apply(dimacs.clauses)
  }
  def apply(clauses: CNF): QmVec = {
    val vec = new QmVec
    clauses foreach (clause => vec.addClause1(canonicalizeClause(clause)))
    vec
  }

  def clauseToBitSet(clause: ClauseAsList): ClauseAsBitSet = {
    clause.zipWithIndex.foldLeft(BitSet()) { case (acc, (literal, index)) =>
      if (literal > 0)
        acc + index
      else
        acc
    }
  }

  def bitSetToClause(rectified: ClauseAsList, b: ClauseAsBitSet): ClauseAsList = {
    rectified.zipWithIndex.map { case (literal: Int, index: Int) =>
      if (b.contains(index))
        literal
      else
        -literal
    }
  }

  // enforce that the clause in in absLess order.  E.g., (1 -2 3 -4)
  def canonicalizeClause(clause: ClauseAsList): ClauseAsList = {
    clause.sortWith(absLess)
  }

  def canonicalizeClause(clause: Set[Int]): ClauseAsList = canonicalizeClause(clause.toList)

  def absLess(a: Int, b: Int): Boolean = {
    abs(a) < abs(b)
  }

  // clause1 < clause2 means that the corresponding elements
  // are < in absolute value (e.g., 1 < 2, and 2 < -3), but in the case
  // the values have they have the same absolute value then
  // e.g. -3 < 3.
  // some examples: (1 2 -3) < (1 2 3)
  //                (-1 2 3) < (1 2 3)
  //                (1 2 3) < (1 -3 4)
  @tailrec
  def clauseLess(clause1: ClauseAsList, clause2: ClauseAsList): Boolean = {
    (clause1, clause2) match {
      case (Nil, Nil) => false
      case (c1 :: c1s, c2 :: c2s) if c1 == c2 => clauseLess(c1s, c2s)
      case (c1 :: _, c2 :: _) if equalAbs(c1, c2) => c1 < c2
      case (c1 :: _, c2 :: _) => absLess(c1, c2)
    }
  }

  def equalAbs(x: Int, y: Int): Boolean = {
    // abs(x) == abs(y)
    x == y || x == -y
  }

  // Decide whether two given Clauses (List[Int]) have the same
  //  corresponding elements in terms of absolute value
  //  e.g.,   List(1, 2, -3) == List(1, -2, 3)
  // We are assuming for efficiency that the lists have the same length.
  // This equal-length restriction is enforced elsewhere.
  def equalAbsList(clause1: ClauseAsList, clause2: ClauseAsList): Boolean = {
    every(clause1, clause2)(equalAbs)
  }

  // A rectified version of a Clause, such as List(1,-2,-3,4)
  // is List(1,2,3,4) having applied abs to each element of the Clause
  def rectify(clause: ClauseAsList): ClauseAsList = {
    clause.map(abs)
  }

  def countPositive(clause: ClauseAsList): Int = {
    clause.count(_ >= 0)
  }

  def maxClauses(n: Int, k: Int): Long = {
    // calculate n! / (n-k)!
    val nChooseK = (k + 1 to n).foldLeft(1L) { (acc: Long, i: Int) => acc * i }
    math.pow(2, k).toLong * nChooseK
  }

  def foldUps(ups: Iterable[RemoveAdd]): RemoveAdd = {
    ups.fold(RemoveAdd.nil) {
      case (RemoveAdd(r, a), RemoveAdd(removes, adds)) => RemoveAdd(removes ++ r, adds ++ a)
    }
  }

}
