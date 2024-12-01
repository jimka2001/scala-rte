// Copyright (©) 2021 EPITA Research and Development Laboratory
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

import genus._
import adjuvant.Adjuvant._
import genus.Types._
import xymbolyco.{Indeterminate, Satisfiable, Unsatisfiable}

import scala.annotation.tailrec

//noinspection RedundantDefaultArgument
abstract class Rte {
  def toMachineReadable(): String = toString

  def |(r: Rte): Rte = Or(this, r)

  def &(r: Rte): Rte = And(this, r)

  def ++(r: Rte): Rte = Cat(this, r)

  def -(r: Rte): Rte = AndNot(this, r)

  def unary_! : Rte = Not(this)

  def ? : Rte = Optional(this) // postfix operator, r.?

  def * : Rte = Star(this) // postfix operator, r.*

  def + : Rte = Plus(this) // postfix operator,   r.+

  def ^(n: Short): Rte = Exponent(this, n) //  r^4 = r ++ r ++ r ++ r


  // can this and that be proven to be equivalent?
  def ~=(that: Rte): Boolean = {
    isomorphic(that).contains(true)
  }

  // Determine whether two Rtes are equivalent,
  //   i.e., represent the same language.
  //   Sometimes this is impossible because of transitions whose
  //   satisfiability we cannot determine.
  // To determine whether two Rtes are isomorphic we try several things:
  //   1. are they syntactically equal
  //   2. are they both And(...) or both Or(...) whose arguments are syntactically
  //      equal but maybe in different orders
  //      equal but maybe in different orders
  //   3. construct the Dfa of the XOR of the two Rtes,
  //         and see if that Dfa has no final states.
  //         As a last resort if it has final states, then there is still a chance
  //         that those final states are not accessible because of the satisfiability
  //         of the transitions.
  //         So we call dfa.spanningPath.
  // RETURNS:
  //   Some(true) if we can determine that the two Rtes accept the same language
  //   Some(false) if we can determine that the two Rtes DO NOT accept the same language.
  //   None, if every path from q0 to a final state traverses at least one transition,(q,td,q')
  //     for which td.inhabited == None (i.e. inhabited = dont-know), then return None.
  def isomorphic(that: Rte): Option[Boolean] = {
    (this, that) match {
      case (x, y) if x == y => Some(true)
      // compare the arguments of And and Or in any order
      case (Or(Seq(r1s@_*)), Or(Seq(r2s@_*))) if r1s.toSet == r2s.toSet => Some(true)
      case (And(Seq(r1s@_*)), And(Seq(r2s@_*))) if r1s.toSet == r2s.toSet => Some(true)
      case (rt1, rt2) =>
        val dfa = Or(And(rt1, Not(rt2)),
                     And(rt2, Not(rt1))).canonicalize.toDfa()
        if (dfa.F.isEmpty) // no final states
          Some(true)
        else dfa.spanningPath match {
          case None => Some(true) // no path exists to a final state
          case Some((Satisfiable, _)) => Some(false) // exists satisfiable path to a final state
          case Some((Indeterminate,_)) => None // exists semi-satisfiable path to a final state
          case Some((Unsatisfiable, _)) => Some(true) // all paths to final state are unsatisfiable
        }
    }
  }

  def toDot(): String = this.toString()

  def toLaTeX(): String

  def nullable: Boolean

  def firstTypes: Set[SimpleTypeD]

  def canonicalize: Rte = {
    fixedPoint[Rte](this,
                    (r: Rte) => r.canonicalizeOnce,
                    (r1: Rte, r2: Rte) => r1 == r2)
  }

  // This method is used for debugging to print the different
  //   stages of development for canonicalize.
  def canonicalizeDebug(n: Int, f: (Rte, Rte) => Unit): Rte = {
    var r1 = this
    println(s"starting with $this")
    for {i <- 0 to n} {
      val r2 = r1.canonicalizeOnce
      println(s"  $i: " + r1)
      println(s"    --> " + r2)
      f(r1, r2)
      r1 = r2
    }
    r1
  }

  def canonicalizeDebug(n: Int, samples: Seq[Seq[Any]]): Rte = {
    canonicalizeDebug(n,
                      (r1: Rte, r2: Rte) => {
                        val dfa1 = r1.toDfa(true)
                        val dfa2 = r2.toDfa(true)
                        println(s"r1 = " + r1)
                        println(s"r2 = " + r2)
                        for {v <- samples} {
                          println(s"dfa1.simulate($v) -> " + dfa1.simulate(v))
                          println(s"dfa2.simulate($v) -> " + dfa2.simulate(v))
                          assert(dfa1.simulate(v) == dfa2.simulate(v),
                                 s"\nv=$v" +
                                   s"\n   r1=$r1" +
                                   s"\n   r2=$r2")
                        }
                      })
  }

  def canonicalizeDebug(n: Int): Rte = {
    canonicalizeDebug(n, (_: Rte, _: Rte) => ())
  }

  def canonicalizeOnce: Rte = this

  def derivative1(wrt: Option[SimpleTypeD]): Rte =
    derivative(wrt, List[SimpleTypeD](), List[SimpleTypeD]())

  def derivative(wrt: Option[SimpleTypeD], factors: List[SimpleTypeD], disjoints: List[SimpleTypeD]): Rte = {
    // factors are known supertypes of the tyep in wrt
    wrt match {
      case None => this
      case Some(td) if td.inhabited.contains(false) => EmptySet
      case Some(td) => derivativeDown(td, factors, disjoints)
    }
  }

  def derivativeDown(wrt: SimpleTypeD, factors: List[SimpleTypeD], disjoints: List[SimpleTypeD]): Rte

  // Computes a pair of Vectors: (Vector[Rte], Vector[Seq[(SimpleTypeD,Int)]])
  //   Vector[Rte] is a mapping from Int to Rte designating the states
  //      of a Dfa.  Each state, i, corresponds to the i'th Rte in this vector.
  //  Vector[Seq[(SimpleTypeD,Int)]] designates the transitions from each state.
  //      the i'th component designates a Seq of transitions, each of the form
  //      (td:SimpleTypeD,j:Int), indicating that in state i, an object of type
  //      td transitions to state j.
  def derivatives(): (Vector[Rte], Vector[Seq[(SimpleTypeD, Int)]]) = {
    import adjuvant.Adjuvant.traceGraph
    def edges(rt: Rte): Seq[(SimpleTypeD, Rte)] = {
      val fts = rt.firstTypes

      val wrts = mdtd(fts)
      def check_for_thread_interrupt(): Unit = {
        // if this code is being called from a unit test with a timeout
        // when we have to explicitly check for thread interrupt
        // if the assertion fails, we simply cause a failed unit test
        assert(!Thread.currentThread().isInterrupted,
               s"Thread interrupted in Extract.extractRte()")
      }
      wrts.map { case (td, (factors, disjoints)) => (td,
        // Here we call rt.derivative, but we pass along the correct-by-construction
        //   factors and disjoint types.  The Singleton:disjointDown method takes
        //   advantage of these trusted lists to easily determine whether the type
        //   in question is a subtype or a disjoint type of the type in question.

        try locally {
          check_for_thread_interrupt()
          rt.derivative(Some(td),
                        factors.toList.sortBy(_.toString),
                        disjoints.toList.sortBy(_.toString)).canonicalize
        }
        catch {
          case e: CannotComputeDerivative =>
            throw new CannotComputeDerivatives(msg = Seq("when generating derivatives of",
                                                         s"  this=$this",
                                                         "when computing edges of ",
                                                         s"  rt=$rt\n",
                                                         s"  which canonicalizes to " + this.canonicalize,
                                                         s"  computing derivative of ${e.rte}",
                                                         s"  wrt=${e.wrt}",
                                                         "  derivatives() reported: " + e.msg).mkString("\n"),
                                               rte = this,
                                               firstTypes = rt.firstTypes,
                                               mdtd = mdtd(rt.firstTypes)
                                               )
        }
      )
      }.toSeq
    }

    traceGraph(this, edges)
  }

  import xymbolyco.Dfa

  def toDfa[E](exitValue: E = true, verbose:Boolean=false): Dfa[Any, SimpleTypeD, E] = {
    //println(s"toDfa: $this")
    val (rtes, edges) = try {
      derivatives()
    }
    catch {
      case e: CannotComputeDerivatives =>
        throw new CannotComputeDfa(msg = Seq("While computing Dfa of",
                                             s"   this = $this",
                                             s"   firstTypes = " + e.firstTypes,
                                             s"   mdtd = " + e.mdtd,
                                             "  toDfa reported:",
                                             e.msg).mkString("\n"),
                                   rte = this)
    }
    if(verbose) {
      println(s"toDfa: ")
      for {i <- rtes.indices} {
        println(s"   rtes[$i] = ${rtes(i)}")
        println(s"   edges[$i]= ${edges(i)}")
      }
    }
    val qids = rtes.indices.toSet
    val fids = qids.filter(i => rtes(i).nullable)
    val fmap = fids.map { i => i -> exitValue }.toMap
    val protoDelta = (for {src <- rtes.indices
                           srcEdges: Seq[(SimpleTypeD, Int)] = edges(src)
                           // in the case that all the transitions from src go to the same destination
                           // then we know they all combine to 1 single transition labeled by STop
                           // because the Dfa is complete by construction.
                           trivial: Boolean = srcEdges.map(_._2).distinct.size == 1
                           (rt, dst) <- if (trivial) Seq((STop, srcEdges.head._2)) else srcEdges
                           } yield (src, rt, dst)).toSet
    Dfa(Qids = qids,
        q0id = 0,
        Fids = fids,
        protoDelta = protoDelta,
        labeler = xymbolyco.GenusLabeler(),
        fMap = fmap)
  }

  /*
   * given a sequence and an exit value, determine whether the sequence
   * matches the regular expression (RTE).  I.e., does the sequence
   * belong to the language of the RTE.
   * If a successful match is made, Some(exitValue) is returned,
   * else None is returned.
   */
  def simulate[E](exitValue: E, seq: Seq[Any], verbose:Boolean=false): Option[E] = {
    toDfa(exitValue,verbose=verbose).simulate(seq, verbose=verbose)
  }

  /**
    * predicate deciding whether a given sequence _matches_ the RTE this.
    * We would like to name this method `match`, but that name is used for
    * pattern matching.
    */
  def contains(seq:Seq[Any], verbose:Boolean=false): Boolean = {
    simulate(true, seq, verbose=verbose) match {
      case None => false
      case Some(true) => true
      case Some(false) => throw new Exception("internal error")
    }
  }

  // walk this Rte and find a node which satisfies the given predicate,
  //  returning Some(x) if x satisfies the predicate, and returning None otherwise.
  def search(test: Rte => Boolean): Option[Rte] = {
    Some(this).filter(test)
  }
}

// abstract class for grouping subclasses of Rte which do not
//   encapsulate other Rte instances
abstract class RteTerminal extends Rte

// abstract class for grouping subclasses of Rte which DO
//    encapsulate other Rte instances
abstract class RteNode extends Rte

object Rte {

  val sigmaStar: Rte = Star(Sigma)
  val sigmaSigmaStarSigma: Rte = Cat(Sigma, Sigma, sigmaStar)
  val notSigma: Rte = Or(sigmaSigmaStarSigma, EmptySeq)
  val notEmptySeq: Rte = Cat(Sigma, sigmaStar)
  val sigmaSigmaStar: Rte = notEmptySeq

  val flattenSingletonTypes =  false

  def isAnd(rt: Rte): Boolean = rt match {
    case And(Seq(_*)) => true
    case _ => false
  }

  def isCat(rt: Rte): Boolean = rt match {
    case Cat(Seq(_*)) => true
    case _ => false
  }

  def isPlus(rt: Rte): Boolean = rt match {
    case Cat(Seq(x, Star(y))) => x == y
    case Cat(Seq(Star(y), x)) => x == y
    case _ => false
  }

  def isStar(rt: Rte): Boolean = rt match {
    case Star(_) => true
    case _ => false
  }

  def isStarCat(rt: Rte): Boolean = rt match {
    case Star(Cat(Seq(_*))) => true
    case _ => false
  }

  def isOr(rt: Rte): Boolean = rt match {
    case Or(Seq(_*)) => true
    case _ => false
  }

  def isSingleton(rt: Rte): Boolean = rt match {
    case Singleton(_) => true
    case _ => false
  }

  // predicate to match form like this Cat(X, Y, Z, Star( Cat(X, Y, Z)))
  def catxyzp(rt: Rte): Boolean = rt match {
    case Cat(Seq(rs1@_*)) =>
      // at least length 2
      if (rs1.sizeIs >= 2) {
        val rightmost = rs1.last
        val leading = rs1.dropRight(1)
        rightmost match {
          case Star(Cat(Seq(rs2@_*))) => leading == rs2
          case _ => false
        }
      }
      else
        false
    case _ => false
  }

  def randomSeq(depth: Int, length: Int, option: Boolean = true): Seq[Rte] = {
    (0 until length).map { _ => randomRte(depth, option) }
  }

  def rteCase[E](seq: Seq[(Rte, E)],
                 handleUnreachable: Rte=>Unit=(rte=>())
                ): IterableOnce[Any] => Option[E] = {
    // take a sequence of pairs (Rte,E)
    // and return a function, f, from Seq[Any] => Option[E]
    // the function, f, can be called with a Seq[Any]
    // to determine which of the seq of Rte's the sequence matches,
    // and if found, then return the corresponding value of type Option[E]
    import xymbolyco.Dfa.dfaUnion
    def makeDisjoint(remaining: List[(Rte, E)],
                     previous: Rte): LazyList[(Rte, E)] = {
      remaining match {
        case Nil => LazyList.empty
        case (rte, e) :: pairs =>
          (And(rte, Not(previous))
            -> e) #:: makeDisjoint(pairs, Or(rte, previous))
      }
    }

    val caseDfa = makeDisjoint(seq.toList, Or())
      .flatMap { case (rte, e) =>
        val dfa = rte.toDfa(e)
        dfa.vacuous() match {
          case Some(true) =>
            handleUnreachable(rte)
            None
          case _ =>
            Some(dfa)
        }
      }
      .reduceLeft(dfaUnion(_, _, xymbolyco.Dfa.defaultArbitrate))

    // it is important that caseDfa is computed separately.
    // because when the caller calls the following function,
    // we want to make sure caseDfa does not get recomputed.
    // In fact the function is re-usable many times, knowing
    // that the Dfa will not get recomputed.
    seq => caseDfa.simulate(seq)
  }

  // rteIfThenElse return a function which we call "the arbitration function".
  // The caller of rteIfThenElse, may thereafter call the arbitration function
  // with a sequence (of type Seq[Any])
  // to selectively execute code corresponding to one of the Rtes provided,
  //   (i.e., provded to rteIfThenElse).
  // The code executed is the code corresponding to the Rte which matches
  // the given sequence.
  //
  // The syntax for calling this rteIfThenElse is awkward, but I have not yet
  // found a better way.
  // The first arg seq: is a sequence of pairs, normally written as follows:
  // Seq( rte1 -> (() => { body1 ... }),
  //      rte2 -> (() => { body2 ... }),
  //      ...
  //     )
  //  each of body1, body2, ... are bodies of code which will be
  //     executed in the case that a Seq[Any] (to be provided later)
  //     matches the Rte.  If the sequence matches more than one Rte,
  //     then the first applicably body will be executed, and its bottom-most
  //     value will be returned from the arbitration function.
  //
  // The second arg, otherwise: designates which code to evaluate
  //   if none of the Rtes match the sequence given to the arbitration function.
  //
  // The 3rd arg, handleUnreachable: is called if rteIfTheElse detects
  //   an Rte whose code is unreachable, I.e. if one of the Rtes is subsumed
  //   by the other Rtes preceding it in the input sequence.
  //   Typical values for handleUnreachable would be a function that does nothing,
  //   or a function which raises an exception, or prints a warning message.
  def rteIfThenElse[E](seq: Seq[(Rte, () => E)],
                       otherwise: () => E,
                       handleUnreachable: Rte=>Unit=(rte=>())
                      ): Seq[Any] => E = {
    val arbitrate1 = rteCase(seq,
                             handleUnreachable=handleUnreachable)

    def arbitrate2(seq: Seq[Any]): E = {
      arbitrate1(seq) match {
        case Some(e) => e()
        case _ => otherwise()
      }
    }

    arbitrate2
  }

  // Sort the sequence alphabetically according to toString
  def sortAlphabetically(seq: Seq[Rte]): Seq[Rte] = {
    seq.sortBy(_.toString)
  }

  //here we are passing along an avoidEmpty boolean that will be true when we do not wish there to be :
  // any ANDs or NOTs in the RTE, and that any of the SimpleTypeDs will not be empty either
  // this way we are also excluding the EmptySeq, EmptySet, and notSigma explicitly, while also not allowing
  // the recursive call for the randomTypeD to create any EmptyTypes
  def randomRte(depth: Int, avoidEmpty: Boolean = true): Rte = {
    import scala.util.Random
    val random = new Random

    val rteVector = Vector(notEmptySeq,
                           Sigma,
                           sigmaStar,
                           notSigma,
                           EmptySeq,
                           EmptySet)
    val generators: Seq[() => Rte] = Vector(
      () => rteVector(random.nextInt(rteVector.length - (if (avoidEmpty) 3 else 0))),
      () => Or(randomSeq(depth - 1, random.nextInt(3) + 2, avoidEmpty)),
      () => Star(randomRte(depth - 1, avoidEmpty)),
      () => Cat(randomSeq(depth - 1, random.nextInt(2) + 2, avoidEmpty)),
      () => Singleton(RandomType.randomType(0, Some(!avoidEmpty))),
      () => And(randomSeq(depth - 1, 2, avoidEmpty)),
      () => Not(randomRte(depth - 1, avoidEmpty)))

    if (depth <= 0)
      Singleton(RandomType.randomType(0, Some(!avoidEmpty)))
    else {
      val g = generators(random.nextInt(generators.length - (if (avoidEmpty) 2 else 0)))
      g()
    }
  }

  def rteView(rte: Rte,
              title: String = "",
              abbrev: Boolean = false,
              label: Option[String] = None,
              showSink: Boolean = true,
              dotFileCB: String => Unit = (_ => ()),
              givenLabels: Seq[SimpleTypeD] = Seq(),
              printLatex: Boolean = false): String = {

    xymbolyco.GraphViz.dfaView[Any,SimpleTypeD,Boolean](rte.toDfa(true),
                                                        title, abbrev, label, showSink,
                                                        dotFileCB, givenLabels, printLatex)
  }
}

class CannotComputeDerivative(val msg: String,
                              val rte: Rte,
                              val wrt: SimpleTypeD) extends Exception(msg) {}

class CannotComputeDerivatives(val msg: String,
                               val rte: Rte,
                               val firstTypes: Set[SimpleTypeD],
                               val mdtd: Map[SimpleTypeD, (Set[SimpleTypeD], Set[SimpleTypeD])]) extends Exception(msg) {}

class CannotComputeDfa(val msg: String, val rte: Rte) extends Exception(msg) {}
