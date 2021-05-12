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

object BitFiddle {

  import bdd.DimacsBdd._

  private val prng = scala.util.Random

  def minTerm(n: Int, i: Long): List[Int] = {
    // Calculate the minTerm as a list of positive and negative integers.
    // View i as an n-bit integer.  Look at each bit in that integer.
    // If the j'th bit is 0, take j, else take -j, and build a list
    //   of these +/1 j's.
    // E.g., if n=3 and i=6, that's binary 110; so build the list
    //    List(3, 2, -1).  The list is actually built with the most
    //    significant bit at the left, and least significant at the right.
    // Each minTerm contains all the integers 1 <= |j| < n in either their
    // positive or negative form.  I.e., if n=3, then such a minTerm is
    // List(3,-2,1) but List(3,-3,2,1) is not and List(3,1) is not.

    val nil: List[Int] = Nil

    (1 to n).foldLeft((nil, i)) { case ((stack: List[Int], bitMask), position) =>
      ((if (bitMask % 2 == 1)
        position
      else
        -position) :: stack, bitMask / 2)
    }._1
  }

  // return m number of randomly chosen integers between 1 (inclusive) and n (inclusive)
  def genSample(n:Int, m:Int):Set[Int] = {
    require(n >= m)
    require(n >= 0)
    require(m >= 0)

    @tailrec
    def take(m: Int, chosen: Set[Int]): Set[Int] = {
      val sample = prng.nextInt(n) + 1  // 1 <= sample <= n
      if (m == 0)
        chosen
      else if (chosen.contains(sample))
        take(m, chosen)
      else
        take(m - 1, chosen + sample)
    }

    // when taking m samples between 1 and n-1, if m >= n/2
    //   we assume it is faster to take n-m samples which is
    //   < n/2, then invert the output.
    //   E.g., if we want to take 99 unique samples of 0 to 99
    //   it is hard, but to take 1 sample is easy.
    if (m < n / 2)
      take(m, Set())
    else {
      import adjuvant.Accumulators.withSetCollector
      val inverse = take(n - m, Set())
      withSetCollector { collect =>
        (1 to n).foreach { i =>
          if (!inverse.contains(i))
            collect(i)
        }
      }
    }
  }

  // Generate the minterms of a Boolean function of n variables.  The
  // Boolean function has a truth table with 2^n rows.
  // We generate a dnf which is a list of min-terms (each a return value of minTerm(n,k)).
  // The min-terms included are exactly those for which f returns true,
  //   when f is called with the Int designating the row of the truth table.
  //   Truth table rows are numbered from 0 at the top, corresponding to FFF...F
  //   to (2^n)-1 at the bottom corresponding to TTT...T
  def genDnfFromBitMask(n: Int, maxNumBits:Int, maxNumTerms:Int, f: Int => Boolean): IterableOnce[List[Int]] = {
    require(n >= maxNumBits)
    import adjuvant.Accumulators.withCollector

    if (n == maxNumBits) {
      withCollector { collect =>
        (0 to Math.min(1 << n , maxNumTerms)).foreach { truthTableRow =>
          if (f(truthTableRow))
            collect(minTerm(n, truthTableRow))
        }
      }
    }
    else {
      //  i.e. how many maxNumBits-element subsets of a set of size n ?
      //  n * (n-1) * (n-2) * ... * (n-maxNumBits+1), a product of maxNumBits integers
      // e.g.   how many 3 element subsets of a size-7 set?   7*6*5, product of 3 integers
      //   but each integer can be positive or negative, so multiply by 2^maxNumBits
      val numSamplesPossible:Int = Math.min(maxNumTerms,
                                            (1L << maxNumBits) * (0 until maxNumBits).foldLeft(1L) { (acc, j) => acc * (n - j) })
        .toInt
      withCollector { collect =>
        (0 until numSamplesPossible).foreach { _ =>
          val bitSet = genSample(n, maxNumBits)
          val term = bitSet.map { i => if (prng.nextBoolean()) i else -i }
          if (f(0))
            collect(term.toList)
        }
      }
    }
  }

  def genRandomDnf(n: Int): IterableOnce[List[Int]] = genRandomDnf(n, n, 1<<n - 1, 0.5)

  def genRandomDnf(n: Int, maxNumBits:Int, maxNumTerms:Int, odds: Double): IterableOnce[List[Int]] = {
    // generate a dnf containing roughly odds (as a fraction) randomly selected
    //   of the possible minterms of n-variables.
    genDnfFromBitMask(n, maxNumBits, maxNumTerms, _ => prng.nextDouble() < odds)
  }

  // Generate a Bdd of n variables.  The boolean function
  // for such a Bdd has a truth table with 2^n rows.
  // We generate a dnf which is a list of min-terms (each a return value of minTerm(n,k)).
  // The min-terms included are exactly those for which f returns true,
  //   when f is called with the Int designating the row of the truth table.
  //   Truth table rows are numbered from 0 at the top, corresponding to FFF...F
  //   to (2^n)-1 at the bottom corresponding to TTT...T
  def genBddFromBitMask(n: Int, maxNumBits:Int, maxNumTerms:Int, f: Int => Boolean): Bdd = {
    dnfToBdd(genDnfFromBitMask(n, maxNumBits, maxNumTerms, f))
  }

  // Generate randomly selected BDD of n variables
  // in terms of truth table, this is equivalent to choosing a number between 0 and 2^n-1 where
  // each 1 in k'th position of the binary representation indicates a T in the k'th row
  // of the truth table.
  def genRandomBdd(n: Int): Bdd = genRandomBdd(n, n, 0.5)

  def genRandomBdd(n: Int, maxNumBits:Int, odds: Double): Bdd = {
    genBddFromBitMask(n, maxNumBits, 1<<n , _ => prng.nextDouble() < odds)
  }

  def isRowSet(truthTable:Long,truthTableRow:Int):Boolean = {
    (truthTable & (1L << truthTableRow)) != 0
  }
  // if the 1 bits of the binary representation of truthTable denote the T
  // rows in the truth table, with the least significant bit representing
  // variable x1. (there is no x0)
  // truthTable is interpreted as a 2^n-bit binary number. 0 <= truthTable < 2^(2^n)
  def genKthBdd(numVars: Int, truthTable: Long): Bdd = {
    require(numVars < 8) // cannot represent truthTable for numVars=8 in  a Long
    require(truthTable >= 0 , s"truthTable=$truthTable")
    require(numVars > 0)
    require(truthTable < (BigInt(1)<<(1<<numVars)))
    // we interpret truthTable as a 'function' mapping the range 0<=i<2^n to Boolean
    //   whose value is true if there is a 1 in the ith position of truthTable,
    //   considering the 0'th position as the least significant bit.
    genBddFromBitMask(numVars, maxNumBits=numVars, maxNumTerms=1<<numVars, isRowSet(truthTable, _))
  }
}
