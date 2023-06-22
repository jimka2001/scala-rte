package rte

import genus.GenusSpecifications.naiveGenGenus
import org.scalacheck.Gen

import scala.util.Random

/*
Implementation of a Boltzmann generator for Rte, according to this paper: https://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf
A Boltzmann generator generates in linear time a Rte with size size in [(size - epsilon); (size + epsilon)]
*/
// FIXME: Refactor: And, Or and Cat share the same generator
// FIXME: Refactor: Not and Star share the same generator
class RteBoltzmann(var size: Int) {
  private val x = findGeneratorParameter(size) // The parameter of the generator
  private val oracle = new RteOracle(x)
  private val rand = new RandTools()
  private val stdDepth = 5;

  // Implementation of a generation in ScalaCheck's sense, by using the Boltzmann generation technique
  def RteBoltzmannGen(): Gen[Rte] = Gen.lzy {
    if (rand.Bernouilli(oracle.leaf / (oracle.leaf + oracle.internalnode)))
      genLeaf()
    else
      genInternalNode()
  }

  // TODO: Figure out a non-currifying way to do this gen
  // FIXME: Implement
  // Boltzmann generator of the atoms
  def genLeaf(): Gen[Rte] = Gen.lzy {
    val weights = Array(oracle.emptyset, oracle.epsilon, oracle.sigma, oracle.std)
    val gens = Array(EmptySet, EmptyWord, Sigma, naiveGenGenus(stdDepth))

    // FIXME: Make this exit the function when line 43 is triggered. Make the whole yield the returned value. Same for internalnodeGen
    for {
      i <- 1 until weights.length
      rest = for {
        j <- i until weights.length
      } yield weights(j)

      coef = weights(i) / (weights(i) + rest.sum)
    } if (rand.Bernouilli(coef)) gens(i)

    gens(weights.length - 1)
  }

  // TODO: Figure out a non-currifying way to do this gen
  // FIXME: Implement
  // Boltzmann generator of the internal nodes
  def genInternalNode(): Gen[Rte] = Gen.lzy {
    val weights = Array(oracle.and, oracle.or, oracle.cat, oracle.star, oracle.not)
    val gens = Array(genAnd(), genOr(), genCat(), genStar(), genNot()) // The lambda form is an attempt to not execute the content of the function. This is not a satisfactory solution, and should be changed in the future

    for {
      i <- 1 until weights.length
      val rest = for {
        j <- i until weights.length
      } yield weights(j)

      val coef = weights(i) / (weights(i) + rest.sum)
    } if (rand.Bernouilli(coef))
      gens(i)

    gens(weights.length - 1)
  }

  // Boltzmann generator of the And
  def genAnd(): Gen[Rte] = Gen.lzy {
    val size = rand.Geometric(oracle.rte)
    val args = for {i <- 1 to size} yield RteBoltzmannGen().sample.get

    And(args)
  }

  // Boltzmann generator of the Or
  def genOr(): Gen[Rte] = Gen.lzy {
    val size = rand.Geometric(oracle.rte)
    val args = for {i <- 1 to size} yield RteBoltzmannGen().sample.get

    Or(args)
  }

  // Boltzmann generator of the Cat
  def genCat(): Gen[Rte] = Gen.lzy {
    val size = rand.Geometric(oracle.rte)
    val args = for {i <- 1 to size} yield RteBoltzmannGen().sample.get

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

  // TODO: Find Rte(x) and Rte'(x)
  // FIXME: Implement this method
  // You can find x by solving the equation: (x * Rte'(x)) / Rte(x) = size, with Rte(x) being the OGF for Rte, and Rte'(x) the derivative of Rte(x)
  def findGeneratorParameter(size: Int): Float = {
    0.1f
  }
}

/*
Implementation of an oracle for the Boltzmann generator, according to this paper: https://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf
The oracle takes x as the parameter of the Boltzmann generator and precomputes the OGF function for all the components for the generator to be used, with 0 <= x <= ρ, ρ being the radius of convergence of the OGF of Rte.
*/
// FIXME: This class could us some reformating:
//  - All atomes have the same OGF
//  - And, Or and Cat share the same OGF
//  - Not and Star sahre the same OGF
class RteOracle(var x: Float) {
  val rte = Rte(x)

  // OGF for the atoms
  val epsilon = 1
  val emptyset = 1
  val sigma = 1
  val std = 1

  // OGF for the internal nodes
  val and = And(x)
  val or = Or(x)
  val cat = Cat(x)
  val star = Star(x)
  val not = Not(x)

  val leaf = epsilon + emptyset + sigma + std
  val internalnode = and + or + cat + star + not

  // TODO: Compute Rte's OGF
  // FIXME: Implement this method
  private def Rte(x: Float): Long = {
    1
  }

  // OGF for the And structure
  // And is defined by the following algebraic type: and x Seq(Rte)
  private def And(x: Float): Long = {
    1 * (1 / (1 - Rte(x)))
  }

  // OGF for the Or structure
  // Or is defined by the following algebraic type: or x Seq(Rte)
  private def Or(x: Float): Long = {
    1 * (1 / (1 - Rte(x)))
  }

  // OGF for the Cat structure
  // Cat is defined by the following algebraic type: cat x Seq(Rte)
  private def Cat(x: Float): Long = {
    1 * (1 / (1 - Rte(x)))
  }

  // OGF for the Star structure
  // Star is defined by the following algebraic type: star x Rte
  private def Star(x: Float): Long = {
    1 * Rte(x)
  }

  // OGF for the Not structure
  // Not is defined by the following algebraic type: not x Rte
  private def Not(x: Float): Long = {
    1 * Rte(x)
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
  def Bernouilli(p: Float): Boolean = {
    if (p < 0 || p > 1) throw new IllegalArgumentException("RteBoltzmann: Bernouilli: p must be between 0 and 1")
    rand.nextFloat() < p
  }

  /**
   * Geometric distribution sampler
   * @param p probability of success
   * @return n with probability p * (1-p)^n
   */
  def Geometric(p: Float): Int = {
    if (p < 0 || p > 1) throw new IllegalArgumentException("RteBoltzmann: Geometric: p must be between 0 and 1")
    def gen(acc: Int): Int = {
      if (rand.nextFloat() < p)
        acc
      else
        1 + acc
    }
    gen(0)
  }
}
