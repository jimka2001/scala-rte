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

import adjuvant.CLcompat._
import dimacs.QmVec._
import dimacs.dimacsParse._
import dimacs.dimacsSimplify._
import org.scalatest.funsuite.AnyFunSuite


class DimacsSuite extends AnyFunSuite {
  test("sample test") {
    assert(1 == 1, "sample assertion")
  }

  test("every") {

    assert(every(List(10, 20, 30), List(1, 2, 3))(_ > _), "test 1")
    assert(every(List(1), List(2))(_ < _), "test 2")
    assert(!every(List(2), List(3))(_ > _), "test 3")
  }

  test("equalAbs") {
    assert(equalAbs(1, -1), "test1")
    assert(!equalAbs(1, -2), "test2")
  }
  test("equalAbsList") {
    assert(equalAbsList(List(1, 2, 3), List(-1, 2, 3)), "test1")
    assert(equalAbsList(List(1, 2, 3), List(1, -2, 3)), "test2")
    assert(equalAbsList(List(1, 2, -3), List(-1, -2, 3)), "test3")
    assert(equalAbsList(List(), List()), "test4")
  }

  test("isCompatible") {
    assert(isCompatible2(List(1), List(-1)), "test1")
    assert(isCompatible2(List(1, 2), List(-1, 2)), "test2")
    assert(!isCompatible2(List(1, 2), List(-1, -2)), "test3")
    assert(isCompatible2(List(1, 2, 3, 4, 5), List(1, 2, 3, -4, 5)), "test4")
    assert(isCompatible2(List(1,-2,3), List(1,-2,-3)), "test5")
    assert(isCompatible2(List(1,-2,3,4), List(1,-2,-3,4)), "test6")
    assert(!isCompatible2(List(1,-2,3,-4), List(1,-2,-3,4)), "test7")
  }
  test("absLess") {
    assert(absLess(-1, 2), "test1")
    assert(absLess(-1, -2), "test2")
    assert(absLess(1, -2), "test3")
    assert(!absLess(2, -1), "test4")
    assert(!absLess(-2, 1), "test5")
    assert(!absLess(-2, -1), "test6")
  }
  test("clauseLess") {
    assert(clauseLess(List(1, 2, 3), List(1, 2, 4)), "test1")
    assert(clauseLess(List(1, -2, 3), List(1, 2, 4)), "test2")
    assert(clauseLess(List(1, -2, 3), List(1, 2, 3)), "test3")
    assert(clauseLess(List(1, 2, 3), List(1, -3, 4)), "test4")
  }
  test("merge") {
    val less = clauseLess _
    assert(List() == merge(List(), List(), less), "test0")
    assert(List(List(1, 2), List(2, 3), List(3, 4), List(4, 5))
             == merge(List(List(1, 2), List(3, 4)),
                      List(List(2, 3), List(4, 5)),
                      less), "test1")
    assert(List(List(1, 2), List(1, 2), List(2, 3), List(3, 4), List(4, 5))
             == merge(List(List(1, 2), List(3, 4)),
                      List(List(1, 2), List(2, 3), List(4, 5)),
                      less), "test2")
  }

  def getClauses(vec: QmVec, posCount: Int, length: Int): Set[List[Int]] = {
    (for {
      lengthHash <- vec.hash.get(posCount).toIterable
      rectHash <- lengthHash.get(length).toIterable
      (rectified, clauses) <- rectHash
      clause <- clauses
    } yield bitSetToClause(rectified,clause)).toSet
  }

  test("QmVec factory") {
    val vec = new QmVec

    vec.addClause1(List(1, 2, 3))
    vec.addClause1(List(-1, 2, 3))
    assert(Set(List(-1,2, 3)) == getClauses(vec, 2, 3), "test0")
    assert(Set(List(1, 2, 3)) == getClauses(vec, 3, 3), "test1")

    vec.addClause1(List(-1, 2, 4))
    assert(Set(List(-1, 2, 3), List(-1, 2, 4)) == getClauses(vec, 2, 3), "test2")

    vec.removeClause1(List(-1, 2, 3))
    assert(Set(List(-1, 2, 4)) == getClauses(vec, 2, 3), "test3")
    assert(Set(List(1, 2, 3)) == getClauses(vec, 3, 3), "test4")
  }

  test("Qm reduce") {
    assert(List(List(1)) == quineMcCluskeyReduce(List(List(1, 2), List(1, -2))), "test 0")
    assert(List(List(1, 3)) == quineMcCluskeyReduce(List(List(1, 2, 3), List(1, -2, 3))), "test 1")
    assert(List(List(1, 3), List(2, 3)) == quineMcCluskeyReduce(List(List(1, 2, 3), List(-1, 2, 3), List(1, -2, 3))), "test 2")
    assert(quineMcCluskeyReduce(List(List(1, 2, 3),
                                     List(-1, 2, 3),
                                     List(-2, 3))) == List(List(3)))
    assert(quineMcCluskeyReduce(List(List(1, 2, 3, 4),
                                     List(1, 2, 3, -4),
                                     List(-1, 2, 3),
                                     List(-2, 3))) == List(List(3)))
    assert(quineMcCluskeyReduce(List(List(1, 2, 3, 4, 5),
                                     List(1, 2, 3, 4, -5),
                                     List(1, 2, 3, -4),
                                     List(-1, 2, 3),
                                     List(-2, 3))) == List(List(3)))
  }

  test("QM v1 v2") {
    import dimacs.dimacsSimplify.quineMcCluskeyReduceAndSortResults

    val cnf1 = List(List(1, 2, -3), List(-1, 2, 3), List(1, 2, 3), List(1, -2, -3), List(-1, -2, 3), List(-1, -2, -3), List(1, -2, 3))
    val cnf2 = List(List(1), List(-2), List(3))
    assert(quineMcCluskeyReduceAndSortResults(QmVec(cnf1)) == quineMcCluskeyReduceAndSortResults(QmVec(cnf2)), s"failed for cnf2=$cnf2")

  }

  test("QM v1 v2 b") {

    val cnf0 = List(List(1, 2, 3),
                    List(1, -2, 3), List(1, 2, -3), List(-1, 2, 3),
                    List(1, -2, -3), List(-1, -2, 3),
                    List(-1, -2, -3))
    assert(List(List(1), List(-2), List(3)) == quineMcCluskeyReduce(cnf0))

    val cnf1 = List(List(1, 3), List(1, 2), List(2, 3),
                    List(1, -2), List(-2, 3), List(1, -3), List(-1, 3),
                    List(-2, -3), List(-1, -2))
    assert(List(List(1), List(-2), List(3)) == quineMcCluskeyReduce(cnf1))

    val cnf2 = List(List(1), List(-2), List(3))
    assert(List(List(1), List(-2), List(3)) == quineMcCluskeyReduce(cnf2))
  }

  def semanticsStressTest(): Unit = {
    semanticsStressTest(14)
  }

  def semanticsStressTest(maxNumVars: Int): Unit = {
    // generate many CNFs and reduce them in two ways.
    //    1. using the Quine McCluskey method to first simply them, then convert the result to a BDD
    //    2. convert the original CNF to a BDD
    //    3. assert that the two BDDs are identical
    // Thus the QM method maintains the Boolean function represented by the CNF.
    import bdd.Bdd._
    import bdd.DimacsBdd._
    import dimacs.perfTest.genCNF

    withNewBddHash {
      for {
        numVars <- 2 to maxNumVars
        density <- 2 to numVars
        numClauses <- 2 to math.min(numVars * numVars, maxClauses(numVars, density)).toInt
      } {
        val cnf = genCNF(numVars, numClauses, density)
        val reduced = quineMcCluskeyReduceAndSortResults(QmVec(cnf))
        val bddOfCnf = cnfToBdd(cnf)
        val bddOfReduced = cnfToBdd(reduced)
        assert(bddOfCnf eq bddOfReduced)
      }
    }
  }
  test("reduceOneVar"){
    import scala.collection.immutable.BitSet

    assert(reduceOneVar(List(1, 4, 5, 8), clauseToBitSet(List(1,4,-5,-8)), clauseToBitSet(List(1,-4,-5,-8))) == (BitSet(0), List(1,5,8)))
    locally{
      val (clause:ClauseAsBitSet,rectified:List[Int]) = reduceOneVar(List(1, 4, 5, 8), clauseToBitSet(List(1,4,5,-8)), clauseToBitSet(List(1,-4,5,-8)))
      assert(rectified == List(1,5,8))
      assert(bitSetToClause(rectified,clause) == List(1,5,-8))
      assert(clause==BitSet(0,1))
    }

    assert(reduceOneVar(List(1, 4, 5, 8), clauseToBitSet(List(1,4,5,-8)), clauseToBitSet(List(1,-4,5,-8))) == (BitSet(0,1), List(1,5,8)))
    assert(reduceOneVar(List(1, 4, 5, 8), clauseToBitSet(List(1,4,5,-8)), clauseToBitSet(List(1,-4,5,-8))) == (BitSet(0,1), List(1,5,8)))
    val (reduced,rectified) = reduceOneVar(List(1, 4, 5, 8), clauseToBitSet(List(1,4,5,-8)), clauseToBitSet(List(1,-4,5,-8)))
    assert(rectified == List(1,5,8))
    assert(bitSetToClause(rectified,reduced) == List(1,5,-8))
  }
  test("reduceOneVar 2") {
    val literals = List(3, 5, 7, 11, 17)
    for {
      p <- 0 until literals.length
      clause1 = literals
      (prefix: List[Int], suffix: List[Int]) = literals.splitAt(p)
      clause2: List[Int] = prefix ++ (-(suffix.head) :: suffix.tail)
      (reduced, newRectified) = reduceOneVar(literals, clauseToBitSet(clause1), clauseToBitSet(clause2))
    } {
      // we are calling reduceOneVar on two clauses, one is a copy of literals, and one has been negated in the p'th position
      // this should result in the p'th position being removed in the newly calculated rectified, and also the reduced clause
      // will have all positive literals.
      assert(newRectified.size == literals.length - 1)
      assert(reduced.size == literals.length - 1)
    }
  }
  test("crossCompatiblize") {
    semanticsStressTest(8)
  }
  test("parse dimacs") {
    val dimacs = parseStringContent("""c  simple_v3_c2.cnf
c
p cnf 3 2
1 -3 0 2 3 -1 0""")

    assert(dimacs.problem.problemType == "cnf")
    assert(dimacs.problem.numVars == 3)
    assert(dimacs.problem.numClauses == 2)
    assert(List(List(-1,3,2), List(-3, 1)) == dimacs.clauses)
  }
  test("parse dimacs 2") {

    List(
      // example from https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
      """c  simple_v3_c2.cnf
        |c
        |p cnf 3 2
        |1 -3 0
        |2 3 -1 0""".stripMargin,
      // missing final 0
      """c  simple_v3_c2.cnf
        |c
        |p cnf 3 2
        |1 -3 0
        |2 3 -1 """.stripMargin,
      // space at the end of some lines
      """c  simple_v3_c2.cnf
        |c
        |p cnf 3 2
        |1 -3 0
        |2 3 -1 """.stripMargin,
      // missing final 0 and missing space
      """c  simple_v3_c2.cnf
        |c
        |p cnf 3 2
        |1 -3 0
        |2 3 -1""".stripMargin,
      // missing newline between clauses
      """c  simple_v3_c2.cnf
        |c
        |p cnf 3 2
        |1 -3 0 2 3 -1 0""".stripMargin
      ).foreach { str =>
      val cnf = parseStringContent(str).clauses
      assert(cnf == List(List(-1,3,2), List(-3, 1)), s"failed for str=$str")
    }
  }
  test("read dimacs benchmark files") {
    val base = this.getClass().getResource(".").toString.drop(5) // skip "file:" 5 characters
    val rel = "../../../../../cl-robdd/data"
    // bench marks
    List("aim-100-1_6-no-1.cnf",
         "aim-50-1_6-yes1-4.cnf",
         "bf0432-007.cnf",
         "dtba-sat.cnf",
         "dubois20.cnf",
         "dubois21.cnf",
         "dubois22.cnf",
         "hole6.cnf",
         "par8-1-c.cnf",
         "quinn.cnf",
         "sat-33ZzxW.cnf",

         "sat-dMt1DH.cnf", // too big
         "simple_v3_c2.cnf",
         "zebra_v155_c1135.cnf"
         ).foreach { fname =>

      dimacsConvertFile(fname,
                        fname => base + rel + "/" + fname,
                        fname => s"/tmp/reduced-$fname")
    }
  }
}
