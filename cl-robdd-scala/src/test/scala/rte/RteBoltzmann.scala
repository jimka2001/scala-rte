// copyright (c) 2021 epita research and development laboratory
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

package rte

import genus.GenusSpecifications.naiveGenGenus
import org.scalacheck.Gen

import scala.util.Random

/**
* /!\ This generator is not usable to generate RTEs. It is still under testing!
* /!\ You can also not use this generator for a control parameter that's above 0.07735. This is because the Generating Function of Rte(x) is valid only for values of z between [0, 0.07735].
* This generator is currently parametered for size 25. If you want to use it for another size, follow the instructions in the findGeneratorParameter() function
*
* Implementation of a Boltzmann generator for Rte, according to this paper: https://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf
* A Boltzmann generator generates in linear time a Rte with size size in [(size - epsilon); (size + epsilon)]
*/
// FIXME: Refactor: And, Or and Cat share the same generator
// FIXME: Refactor: Not and Star share the same generator
class RteBoltzmann(var size: Int, stdDepth: Int) {
  private val x = findGeneratorParameter(size) // The parameter of the generator
  private val oracle = new RteOracle(x)
  private val rand = new RandTools()

  // Implementation of a generation in ScalaCheck's sense, by using the Boltzmann generation technique
  def RteBoltzmannGen(): Gen[Rte] = Gen.lzy {
    if (rand.Bernouilli(oracle.leaf / (oracle.leaf + oracle.internalnode)))
      genLeaf()
    else
      genInternalNode()
  }

  def genLeaf(): Gen[Rte] = Gen.lzy {
    val weights = Array(oracle.emptyset, oracle.epsilon, oracle.sigma, oracle.std)
    val gens = Array[Rte](EmptySet, EmptyWord, Sigma, Singleton(naiveGenGenus(stdDepth).sample.get))

    var i = 0
    var found = false
    while (i < weights.length && !found) {
      val rest = for {
        j <- i until weights.length
      } yield weights(j)

      val coef = weights(i) / (weights(i) + rest.sum)
      if (rand.Bernouilli(coef)) {
        found = true
      } else {
        i += 1
      }
    }

    if (i == gens.length)
      i -= 1

    Gen.const[Rte] (gens(i))
  }

  def genInternalNode(): Gen[Rte] = Gen.lzy {
    val weights = Array(oracle.and, oracle.or, oracle.cat, oracle.star, oracle.not)
    val gens = Array[Gen[Rte]](genAnd(), genOr(), genCat(), genStar(), genNot()) // The lambda form is an attempt to not execute the content of the function. This is not a satisfactory solution, and should be changed in the future

    var i = 0
    var found = false
    while (i < weights.length && !found) {
      val rest = for {
        j <- i until weights.length
      } yield weights(j)

      val coef = weights(i) / (weights(i) + rest.sum)
      if (rand.Bernouilli(coef)) {
        found = true
      } else {
        i += 1
      }
    }

    if (i == gens.length)
      i -= 1

    Gen.const[Rte] (gens(i).sample.get)
  }

  // Boltzmann generator of the And
  def genAnd(): Gen[Rte] = Gen.lzy {
    val size = rand.Geometric(oracle.rte)
    val args = for {i <- 1 to 2 + size} yield RteBoltzmannGen().sample.get // with specify 2 + size to have at least 2 children

    And(args)
  }

  // Boltzmann generator of the Or
  def genOr(): Gen[Rte] = Gen.lzy {
    val size = rand.Geometric(oracle.rte)
    val args = for {i <- 1 to 2 + size} yield RteBoltzmannGen().sample.get // with specify 2 + size to have at least 2 children

    Or(args)
  }

  // Boltzmann generator of the Cat
  def genCat(): Gen[Rte] = Gen.lzy {
    val size = rand.Geometric(oracle.rte)
    val args = for {i <- 1 to 2 + size} yield RteBoltzmannGen().sample.get // with specify 2 + size to have at least 2 children

    Cat(args)
  }

  // Boltzmann generator of the Not
  def genNot(): Gen[Rte] = Gen.lzy {
    Not(RteBoltzmannGen().sample.get)
  }

  // Boltzmann generator of the Star
  def genStar(): Gen[Rte] = Gen.lzy {
    Star(RteBoltzmannGen().sample.get)
  }

  // Generate a parameter for the Boltzmann generator based on the expected size we want
  def findGeneratorParameter(size: Int): Double = {
    // This value has been generated by the following python script:
    // ```python
    // from paganini import *
    //
    // # -- to change
    // #insert the expected size of the rte
    // size = 25
    // # --
    // do not change anything past this comment
    //
    // sp = Specification()
    // z , r = Variable(size) , Variable()
    // sp.add(r, z * r ** 2 * Seq(r) + z * r ** 2 * Seq(r) + z * r ** 2 * Seq(r) + z * r + z * r + z + z + z + z)
    // sp.run_tuner(r)
    //
    // print(z.value)
    // ```
    // For size 25. If you want to change the expected size, you can use the same script, and change the size variable AND NOTHING ELSE
    // We make us of the paganini (https://github.com/maciej-bendkowski/paganini) python library to tune the parameter
    0.0773244464365775
  }
}

/*
Implementation of an oracle for the Boltzmann generator, according to this paper: https://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf
The oracle takes x as the parameter of the Boltzmann generator and precomputes the OGF function for all the components for the generator to be used, with 0 <= x <= ρ, ρ being the radius of convergence of the OGF of Rte.
*/
// FIXME: This class could us some reformating:
//  - All atoms have the same OGF
//  - And, Or and Cat share the same OGF
//  - Not and Star sahre the same OGF
// FIXME: The equation for Rte(z) is valid only for z in [0, 0.07735]
class RteOracle(var z: Double) {
  // OGF for the atoms
  val epsilon = z
  val emptyset = z
  val sigma = z
  val std = z

  // OGF for the internal nodes
  val and = And(z)
  val or = Or(z)
  val cat = Cat(z)
  val star = Star(z)
  val not = Not(z)

  val leaf = epsilon + emptyset + sigma + std
  val internalnode = and + or + cat + star + not

  val rte = leaf + internalnode

  // OGF for the Rte structure
  // Note: this solution works only if the parameter x is inferior or equal to (12 - sqrt(192))/(-24) ≃ 0.07735026918. This is the condition for the Rte(x) to have solutions in R
  private def Rte(z: Double): Double = {
    val delta = - 12 * Math.pow(z, 2) - 12 * z + 1
    (- (1 + 2 * z) + Math.sqrt(delta)) / (- 2 * (1 + z))
  }

  // OGF for the And structure
  // And is defined by the following algebraic type: and z Seq(Rte)
  private def And(z: Double): Double = {
    z * (Math.pow(Rte(z), 2) / (1 - Rte(z)))
  }

  // OGF for the Or structure
  // Or is defined by the following algebraic type: or z Seq(Rte)
  private def Or(z: Double): Double = {
    z * (Math.pow(Rte(z), 2) / (1 - Rte(z)))
  }

  // OGF for the Cat structure
  // Cat is defined by the following algebraic type: cat z Seq(Rte)
  private def Cat(z: Double): Double = {
    z * (Math.pow(Rte(z), 2) / (1 - Rte(z)))
  }

  // OGF for the Star structure
  // Star is defined by the following algebraic type: star z Rte
  private def Star(z: Double): Double = {
    z * Rte(z)
  }

  // OGF for the Not structure
  // Not is defined by the following algebraic type: not z Rte
  private def Not(z: Double): Double = {
    z * Rte(z)
  }
}

/*
  Random generation tools as described in section 5. of : https://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf
 */
class RandTools {
  var rand = new Random()

  /**
   * Bernoulli distribution sampler
   * @param p probability of success
   * @return true if success, false if failure
   */
  def Bernouilli(p: Double): Boolean = {
    if (p < 0 || p > 1) throw new IllegalArgumentException(s"RteBoltzmann: Bernouilli: p must be between 0 and 1, but is $p")
    rand.nextDouble() < p
  }

  /**
   * Geometric distribution sampler
   * @param p probability of success
   * @return n with probability p * (1-p)^n
   */
  def Geometric(p: Double): Int = {
    if (p < 0 || p > 1) throw new IllegalArgumentException(s"RteBoltzmann: Geometric: p must be between 0 and 1, but is $p")
    def gen(acc: Int): Int = {
      if (rand.nextDouble() < p)
        acc
      else
        1 + acc
    }
    gen(0)
  }
}
