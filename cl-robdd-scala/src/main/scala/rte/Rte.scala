// Copyright (c) 2021 EPITA Research and Development Laboratory
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

package rte

import genus.SimpleTypeD
import adjuvant.Adjuvant._

abstract class Rte {
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
  // isomorphic
  def ~=(that:Rte):Boolean = {
    (this,that) match {
      case (x,y) if x == y => true
        // compare the arguments of And and Or in any order
      case (Or(Seq(r1s@_*)),Or(Seq(r2s@_*))) if r1s.toSet == r2s.toSet => true
      case (And(Seq(r1s@_*)),And(Seq(r2s@_*))) if r1s.toSet == r2s.toSet => true
      case (rt1,rt2) =>
        val dfa = Or(And(rt1,Not(rt2)),
                     And(rt2,Not(rt1))).toDfa
        (dfa.F.isEmpty // no final states
          || dfa.findSpanningPath.isEmpty) // no path to a final state from q0
    }
  }
  def toLaTeX:String
  //override def toString:String = toLaTeX
  def nullable:Boolean
  def firstTypes:Set[genus.SimpleTypeD]
  def canonicalize:Rte = fixedPoint(this, (r:Rte) => r.canonicalizeOnce, (r1:Rte,r2:Rte)=>r1==r2)
  def canonicalizeOnce:Rte = this

  def derivative(wrt:Option[SimpleTypeD]):Rte = (wrt match {
    case None => this
    case Some(td) if td.inhabited.contains(false) => EmptySet
    case Some(td) => derivativeDown(td)
  }).canonicalize

  def derivativeDown(wrt:SimpleTypeD):Rte

  def derivatives():(Vector[Rte],Vector[Seq[(SimpleTypeD,Int)]]) = {
    import adjuvant.Adjuvant.traceGraph

    def edges(rt:Rte):Seq[(SimpleTypeD,Rte)] = {
      genus.Types.mdtd(rt.firstTypes)
        .map(td => (td,
          try rt.derivative(Some(td))
          catch {
            case e: CannotComputeDerivative =>
              throw new CannotComputeDerivatives(msg=Seq(s"when generating derivatives from $this",
                                                         "when computing edges of " + rt,
                                                         s"  which canonicalizes to " + this.canonicalize,
                                                         s"  computing derivative of ${e.rte}",
                                                         s"  wrt=${e.wrt}",
                                                         "  derivatives() reported: " + e.msg).mkString("\n"),
                                                 rte=this,
                                                 firstTypes = rt.firstTypes,
                                                 mdtd = genus.Types.mdtd(rt.firstTypes)
                                         )
          }))
    }

    traceGraph(this,edges)
  }

  import xymbolyco.Dfa

  def toDfa():Dfa[Any,SimpleTypeD,Boolean] = {
    val (rtes,edges) = try {
      derivatives()
    }
    catch {
      case e: CannotComputeDerivatives =>
        throw new CannotComputeDfa(msg=Seq(s"While trying to compute Dfa of $this ",
                                           s"   firstTypes = " + e.firstTypes,
                                           s"   mdtd = "+ e.mdtd,
                                           "  toDfa reported:",
                                           e.msg).mkString("\n"),
                                   rte=this)
    }

    val qids = rtes.indices.toSet
    val fids = qids.filter(i => rtes(i).nullable)
    val fmap = fids.map{i => i -> true}.toMap
    new Dfa(Qids=qids,
            q0id=0,
            Fids=fids,
            protoDelta = (for{ src <- rtes.indices
                            (rt,dst) <- edges(src)
            } yield (src, rt, dst)).toSet,
            labeler = xymbolyco.GenusLabeler(),
            fMap=fmap)
  }
}

object Rte {

  val sigmaStar: Rte = Star(Sigma)
  val sigmaSigmaStarSigma:Rte = Cat(Sigma, Sigma, sigmaStar)
  val notSigma: Rte = Or(sigmaSigmaStarSigma, EmptyWord)
  val notEpsilon: Rte = Cat(Sigma, sigmaStar)

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

  def randomSeq(depth: Int): Seq[Rte] = {
    val maxCompoundSize = 2
    (0 until maxCompoundSize).map { _ => randomRte(depth) }
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
      () => Singleton(genus.Types.randomType(0))
      )
    if (depth <= 0)
      Singleton(genus.Types.randomType(0))
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
      //dfaView(sdfa, abbreviateTransitions=true)
    }
  }
}

object sanityTest {
  def main(argv: Array[String]):Unit = {
    import genus._
    println(Or(And(Singleton(SAtomic(classOf[Integer])),
                   Not(Singleton(SAtomic(classOf[Long])))),
               Not(Singleton(SEql(42)))))

    import RteImplicits._
    println(Or(And(SAtomic(classOf[Integer]),
                   Not(SAtomic(classOf[Long]))),
               Not(SEql(43))))

    println(Or(And(classOf[Integer],
                   Not(SAtomic(classOf[Long]))),
               Not(SEql(44))))

    println(Rte.randomRte(2))
  }
}

class CannotComputeDerivative(val msg:String, val rte:Rte, val wrt:SimpleTypeD) extends Exception(msg) {}

class CannotComputeDerivatives(val msg:String,
                               val rte:Rte,
                               val firstTypes:Set[SimpleTypeD],
                               val mdtd:Seq[SimpleTypeD]) extends Exception(msg) {}

class CannotComputeDfa(val msg:String, val rte:Rte ) extends Exception(msg){}