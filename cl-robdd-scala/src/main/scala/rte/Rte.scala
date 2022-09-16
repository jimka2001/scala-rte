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

import scala.annotation.tailrec

//noinspection RedundantDefaultArgument
abstract class Rte {
  def toMachineReadable():String = toString
  def |(r: Rte): Rte = Or(this, r)
  def &(r: Rte): Rte = And(this, r)
  def ++(r: Rte): Rte = Cat(this,r)
  def unary_! : Rte = Not(this)
  def ? :Rte = Or(this,EmptyWord)
  def * :Rte = Star(this) // postfix operator
  def + :Rte = Cat(this, Star(this)) // postfix operator
  def ^(n:Short):Rte = {
    n match {
      case 0 => EmptyWord
      case 1 => this
      case i if i > 1 => Cat(Seq.fill(n)(this.canonicalize))
      case i if i < 0 => throw new Error("^ operator does not work with negative numbers: $n")
    }
  }

  // can this and that be proven to be equivalent?
  def ~=(that:Rte):Boolean = {
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
  //   3. construct the Dfa of the XOR of the two Rtes,
  //         and see if that Dfa has no final states.
  //         As a last resort if it has final states, then there is still a chance
  //         that those final states are not accessible because of the satisfiability
  //         of the transitions.
  //         So we call dfa.findSpanningPath(satisfiable), passing along the
  //         value of satisfiable given to isomorphic(that,satisfiable).
  //   If we can determine that the two Rtes accept the same language, then
  //     return Some(true)
  //   If we can determine that the two Rtes DO NOT accept the same language,
  //     return Some(false)
  //   If every path from q0 to a final state traverses at least one transition,(q,td,q')
  //     for which td.inhabited == None, then return None.
  def isomorphic(that:Rte):Option[Boolean] = {
    (this,that) match {
      case (x,y) if x == y => Some(true)
        // compare the arguments of And and Or in any order
      case (Or(Seq(r1s@_*)),Or(Seq(r2s@_*))) if r1s.toSet == r2s.toSet => Some(true)
      case (And(Seq(r1s@_*)),And(Seq(r2s@_*))) if r1s.toSet == r2s.toSet => Some(true)
      case (rt1,rt2) =>
        val dfa = Or(And(rt1,Not(rt2)),
          And(rt2,Not(rt1))).canonicalize.toDfa()
        if (dfa.F.isEmpty) // no final states
          Some(true)
        else if (dfa.findSpanningPath(Seq(Some(true))).nonEmpty) // exists satisfiable path to a final state
          Some(false)
        else if (dfa.findSpanningPath(Seq(None,Some(true))).nonEmpty) // exists semi-satisfiable path to a final state
          None
        else
          Some(false)
    }
  }

  def toLaTeX():String
  def nullable:Boolean
  def firstTypes:Set[SimpleTypeD]
  def canonicalize:Rte = {
    fixedPoint[Rte](this,
                    (r:Rte) => r.canonicalizeOnce,
                    (r1:Rte,r2:Rte)=>r1==r2)
  }

  // This method is used for debugging to print the different
  //   stages of development for canonicalize.
  def canonicalizeDebug(n:Int,f:(Rte,Rte)=>Unit):Rte = {
    var r1 = this
    println(s"starting with $this")
    for{i <- 0 to n}{
      val r2 = r1.canonicalizeOnce
      println(s"  $i: "+ r1)
      println(s"    --> " + r2)
      f(r1,r2)
      r1 = r2
    }
    r1
  }

  def canonicalizeDebug(n:Int,samples:Seq[Seq[Any]]):Rte = {
        canonicalizeDebug(n,
                          (r1:Rte,r2:Rte)=> {
                            val dfa1 = r1.toDfa(true)
                            val dfa2 = r2.toDfa(true)
                            println(s"r1 = " + r1)
                            println(s"r2 = " + r2)
                            for{v <- samples}{
                              println(s"dfa1.simulate($v) -> " + dfa1.simulate(v))
                              println(s"dfa2.simulate($v) -> " + dfa2.simulate(v))
                              assert(dfa1.simulate(v) == dfa2.simulate(v),
                                     s"\nv=$v" +
                                       s"\n   r1=$r1"+
                                       s"\n   r2=$r2")
                            }
                          })
  }
  def canonicalizeDebug(n:Int):Rte = {
    canonicalizeDebug(n,(_:Rte,_:Rte)=>())
  }

  def canonicalizeOnce:Rte = this

  def derivative1(wrt:Option[SimpleTypeD]):Rte =
    derivative(wrt,List[SimpleTypeD](),List[SimpleTypeD]())

  def derivative(wrt:Option[SimpleTypeD], factors:List[SimpleTypeD], disjoints:List[SimpleTypeD]):Rte = {
    val raw = wrt match {
      case None => this
      case Some(td) if td.inhabited.contains(false) => EmptySet
      case Some(td) => derivativeDown(td, factors, disjoints)
    }
    raw
  }
  def derivativeDown(wrt:SimpleTypeD, factors:List[SimpleTypeD], disjoints:List[SimpleTypeD]):Rte

  // Computes a pair of Vectors: (Vector[Rte], Vector[Seq[(SimpleTypeD,Int)]])
  //   Vector[Rte] is a mapping from Int to Rte designating the states
  //      of a Dfa.  Each state, i, corresponds to the i'th Rte in this vector.
  //  Vector[Seq[(SimpleTypeD,Int)]] designates the transitions from each state.
  //      the i'th component designates a Seq of transitions, each of the form
  //      (td:SimpleTypeD,j:Int), indicating that in state i, an object of type
  //      td transitions to state j.
  def derivatives():(Vector[Rte],Vector[Seq[(SimpleTypeD,Int)]]) = {
    import adjuvant.Adjuvant.traceGraph
    def edges(rt:Rte):Seq[(SimpleTypeD,Rte)] = {
      val fts = rt.firstTypes
      val wrts = Types.mdtd(fts)

      wrts.map{case (td, (factors,disjoints)) => (td,
        // Here we call rt.derivative, but we pass along the correct-by-construction
        //   factors and disjoint types.  The Singleton:disjointDown method takes
        //   advantage of these trusted lists to easily determine whether the type
        //   in question is a subtype or a disjoint type of the type in question.
        try rt.derivative(Some(td),factors.toList,disjoints.toList).canonicalize
        catch {
          case e: CannotComputeDerivative =>
            throw new CannotComputeDerivatives(msg=Seq("when generating derivatives of",
                                                       s"  this=$this",
                                                       "when computing edges of ",
                                                       s"  rt=$rt\n",
                                                       s"  which canonicalizes to " + this.canonicalize,
                                                       s"  computing derivative of ${e.rte}",
                                                       s"  wrt=${e.wrt}",
                                                       "  derivatives() reported: " + e.msg).mkString("\n"),
                                               rte=this,
                                               firstTypes = rt.firstTypes,
                                               mdtd = Types.mdtd(rt.firstTypes)
                                               )
        })}.toSeq
    }

    traceGraph(this,edges)
  }

  import xymbolyco.Dfa


  def toDfa[E](exitValue:E=true):Dfa[Any,SimpleTypeD,E] = {
    //println(s"toDfa: $this")
    val (rtes,edges) = try {
      derivatives()
    }
    catch {
      case e: CannotComputeDerivatives =>
        throw new CannotComputeDfa(msg=Seq("While computing Dfa of",
                                           s"   this = $this",
                                           s"   firstTypes = " + e.firstTypes,
                                           s"   mdtd = " + e.mdtd,
                                           "  toDfa reported:",
                                           e.msg).mkString("\n"),
                                   rte=this)
    }
    val qids = rtes.indices.toSet
    val fids = qids.filter(i => rtes(i).nullable)
    val fmap = fids.map{i => i -> exitValue}.toMap
    new Dfa(Qids = qids,
            q0id = 0,
            Fids = fids,
            protoDelta = (for {src <- rtes.indices
                               (rt, dst) <- edges(src)
                               } yield (src, rt, dst)).toSet,
            labeler = xymbolyco.GenusLabeler(),
            fMap = fmap)
  }
  def simulate[E](exitValue:E, seq:Seq[Any]):Option[E] = {
    toDfa(exitValue).simulate(seq)
  }

  // walk this Rte and find a node which satisfies the given predicate,
  //  returning Some(x) if x satisfies the predicate, and returning None otherwise.
  def search(test:Rte=>Boolean):Option[Rte] = {
    Some(this).filter(test)
  }
}

object Rte {

  val sigmaStar: Rte = Star(Sigma)
  val sigmaSigmaStarSigma:Rte = Cat(Sigma, Sigma, sigmaStar)
  val notSigma: Rte = Or(sigmaSigmaStarSigma, EmptyWord)
  val notEpsilon: Rte = Cat(Sigma, sigmaStar)
  val sigmaSigmaStar: Rte = notEpsilon

  def Member(xs: Any*):Rte = {
    Singleton(SMember(xs : _*))
  }
  def Eql(x: Any):Rte = {
    Singleton(SEql(x))
  }
  def Atomic(ct: Class[_]):Rte = {
    Singleton(SAtomic(ct))
  }

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
    case _ => false
  }

  def isStar(rt: Rte): Boolean = rt match {
    case Star(_) => true
    case _ => false
  }

  def isStarCat(rt: Rte):Boolean = rt match {
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
  def catxyzp(rt:Rte):Boolean = rt match {
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

  def randomSeq(depth: Int): Seq[Rte] = {
    val maxCompoundSize = 2
    (0 until maxCompoundSize).map { _ => randomRte(depth) }
  }

  def rteCase[E](seq:Seq[(Rte,E)]):xymbolyco.Dfa[Any,SimpleTypeD,E] = {
    @tailrec
    def excludePrevious(remaining:List[(Rte,E)], previous:List[Rte], acc:List[(Rte,E)]):Seq[(Rte,E)] = {
      remaining match {
        case Nil => acc
        case (rte,e)::pairs =>
          val excluded = And(rte,Not(Or.createOr(previous)))
          excludePrevious(pairs,
                          rte::previous,
                          (excluded.canonicalize,e)::acc)
      }
    }

    def funnyFold[X,Y](seq:Seq[X],f:X=>Y,g:(Y,Y)=>Y):Y = {
      assert(seq.nonEmpty)
      seq.tail.foldLeft(f(seq.head))((acc,x) => g(acc,f(x)))
    }
    def f(pair:(Rte,E)):xymbolyco.Dfa[Any,SimpleTypeD,E] = {
      val (rte,e) = pair
      val dfa = rte.toDfa(e)

      dfa
    }
    val disjoint:Seq[(Rte,E)] = excludePrevious(seq.toList,List(),List())
    funnyFold[(Rte,E),xymbolyco.Dfa[Any,SimpleTypeD,E]](disjoint,f,dfaUnion)
  }


  // Compute the intersection of two given types (SimpleTypeD) for the purpose
  // of creating a label on a Dfa transition.  So we return None if the resulting
  // label (is proved to be) is an empty type, and Some(SimpleTypeD) otherwise.
  def intersectLabels(td1:SimpleTypeD,td2:SimpleTypeD):Option[SimpleTypeD] = {
    val comb = SAnd(td1,td2).canonicalize()
    comb.inhabited match {
      case Some(false) => None
      case _ => Some(comb)
    }
  }

  def combineFmap[E](e1:Option[E],e2:Option[E]):Option[E] = {
    (e1,e2) match {
      case (None,None) => None
      case (Some(b),Some(c)) if c == b => Some(b)
      case (Some(b),Some(c)) =>
        println(s"combineFmap: warning loosing value $c, using $b")
        Some(b) // f-value of dfa1 has precedence over dfa2
      case (Some(b),None) => Some(b)
      case (None,Some(b)) => Some(b)
    }
  }
  // returns Some(true), Some(false), or None
  // Some(true) => the Dfas are provably equivalent, i.e., they both accept the
  //   same language
  // Some(false) => The Dfas are provably not equivalent.
  // None => It cannot be proven whether the Dfas are equivalent.  For example
  //   because it contains a transition which is not known to be inhabited.
  def dfaEquivalent[E](dfa1:xymbolyco.Dfa[Any,SimpleTypeD,E],
                       dfa2:xymbolyco.Dfa[Any,SimpleTypeD,E]):Option[Boolean] = {
    Rte.dfaXor(dfa1,dfa2).vacuous()
  }

  def dfaUnion[E](dfa1:xymbolyco.Dfa[Any,SimpleTypeD,E],
                  dfa2:xymbolyco.Dfa[Any,SimpleTypeD,E]):xymbolyco.Dfa[Any,SimpleTypeD,E] = {
    import xymbolyco.Minimize.sxp

    val dfa = sxp[Any,SimpleTypeD,E](dfa1,dfa2,
                                     intersectLabels, // (L,L)=>Option[L],
                                     (a:Boolean,b:Boolean) => a || b, // arbitrateFinal:(Boolean,Boolean)=>Boolean,
                                     combineFmap //:(E,E)=>E
                                     )
    dfa
  }

  def dfaXor[E](dfa1:xymbolyco.Dfa[Any,SimpleTypeD,E],
                dfa2:xymbolyco.Dfa[Any,SimpleTypeD,E]):xymbolyco.Dfa[Any,SimpleTypeD,E] = {
    import xymbolyco.Minimize.sxp

    val dfa = sxp[Any,SimpleTypeD,E](dfa1,dfa2,
                                     intersectLabels, // (L,L)=>Option[L],
                                     (a:Boolean,b:Boolean) => (a && !b) || (!a && b), // arbitrateFinal:(Boolean,Boolean)=>Boolean,
                                     combineFmap //:(E,E)=>E
                                     )
    dfa
  }
  def sortAlphabetically(seq:Seq[Rte]):Seq[Rte] = {
    seq.sortBy(_.toString)
  }
  def randomRte(depth: Int): Rte = {
    import scala.util.Random
    val random = new Random

    val rteVector = Vector(EmptySet,
                           EmptyWord,
                           Sigma,
                           sigmaStar,
                           notSigma,
                           notEpsilon)
    val generators: Seq[() => Rte] = Vector(
      () => rteVector(random.nextInt(rteVector.length)),
      () => Not(randomRte(depth - 1)),
      () => Star(randomRte(depth - 1)),
      () => And(randomSeq(depth - 1)),
      () => Cat(randomSeq(depth - 1)),
      () => Or(randomSeq(depth - 1)),
      () => Singleton(RandomType.randomType(0))
      )
    if (depth <= 0)
      Singleton(RandomType.randomType(0))
    else {
      val g = generators(random.nextInt(generators.length))
      g()
    }
  }
}

object sanityTest2 {
  def main(argv: Array[String]): Unit = {
    for {depth <- 5 to 7
         _ <- 1 to 2000
         rt = Rte.randomRte(depth)
         } {
      rt.toDfa()
    }
  }
}

object sanityTest {
  def main(argv: Array[String]):Unit = {

  }
}

class CannotComputeDerivative(val msg:String, val rte:Rte, val wrt:SimpleTypeD) extends Exception(msg) {}

class CannotComputeDerivatives(val msg:String,
                               val rte:Rte,
                               val firstTypes:Set[SimpleTypeD],
                               val mdtd:Map[SimpleTypeD,(Set[SimpleTypeD],Set[SimpleTypeD])]) extends Exception(msg) {}

class CannotComputeDfa(val msg:String, val rte:Rte ) extends Exception(msg){}
