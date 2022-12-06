// Copyright (c) 2020 EPITA Research and Development Laboratory
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

package genusbdd
import genus._
import genus.RandomType._
import bdd._

import scala.collection._
import NormalForm._
import genus.Types.evenType

case class GenusBdd(td:SimpleTypeD,tdToInt:mutable.Map[SimpleTypeD,Int]) {
  // Creation of a Bdd representing a SimpleTypeD
  // To do this we associate each unique type within the SimpleTypeD by an
  //   integer which we accumulate in the given tdToInt mutable.Map.
  //   The caller should re-use this mutable map if desiring to create more
  //   Bdds to combine together.   E.g., if the integer 12 corresponds to
  //   type java.lang.Integer, then 12 needs to correspond also to
  //   java.lang.Integer in any other Bdd in order to be compatible.

  // bdd is the Bdd representation of the given SimpleTypeD, however,
  //    the Bdd does not know anything about subtype/supertype relations
  val bdd: Bdd = toBdd(td)

  def toBdd(td: SimpleTypeD): Bdd = {
    td match {
      case SEmpty => BddFalse
      case STop => BddTrue
      case _:TerminalType => Bdd(tdToInt.getOrElseUpdate(td.canonicalize(), tdToInt.size + 1))
      case SNot(td) => Not(toBdd(td))
      case SAnd(tds@_*) => And(tds.map(toBdd): _*)
      case SOr(tds@_*) => Or(tds.map(toBdd): _*)
      case _ => throw new Exception(s"missing case for $td")
    }
  }

  lazy val intToTd: immutable.Map[Int, SimpleTypeD] = tdToInt.map(_.swap).toMap

  // dnf is a SimpleTypeD which represents the original td
  //   but in a Disjunctive Normal Form.   The DNF is an Or of Ands
  //   where each And has been reduced according to subtype/supertype
  //   relations, and according to disjoint types.
  lazy val dnf: SimpleTypeD = locally {
    GenusBdd.bddToDnf(bdd, intToTd)
  }

  // cnf is a SimpleTypeD which represents the original td
  //   but in a Conjunctive Normal Form.  The CNF is an And
  //   of Ors which has been computed by transforming the DNF.
  lazy val cnf: SimpleTypeD = dnf.canonicalize(nf = Some(Cnf))

  def typep(a:Any):Boolean = {
    bdd.directedWalk{
      case BddNode(label,_,_) => intToTd(label).typep(a)
      case BddTrue => true
      case BddFalse => false
    }
  }

  def labelToString(label:Int):String = {
    intToTd(label).toString
  }

  def satisfyingTypeDesignators(delta:Bdd):LazyList[SimpleTypeD] = {
    def satisfying(node:Bdd,lineage:SimpleTypeD):LazyList[SimpleTypeD] = {
      if (SEmpty == lineage) {
        // if the lineage is the empty type, then we can prune the recursion
        LazyList()
      } else
        node match {
          case BddFalse => LazyList()
          case BddTrue => LazyList(lineage)
          case node:BddNode =>
            val td = intToTd(node.label)
            val left = satisfying(node.positive,SAnd(td,lineage).canonicalize(Some(Dnf)))
            val right = satisfying(node.negative,SAnd(SNot(td),lineage).canonicalize(Some(Dnf)))
            left ++ right
        }
    }
    satisfying(delta,STop)
  }

  def subtypep(that:GenusBdd):Option[Boolean] = {
    assert(tdToInt eq that.tdToInt)
    AndNot(bdd,that.bdd) match {
      case BddFalse => Some(true)
      case delta:Bdd =>
        val allEmpty = satisfyingTypeDesignators(delta).forall{
          td => SEmpty == td.canonicalize()
        }
        if (allEmpty)
          Some(true)
        else
          None
    }
  }
}

object GenusBdd {

  def prettyAnd(terms: List[SimpleTypeD]): SimpleTypeD = {
    SAnd.createAnd(terms)
  }

  def prettyOr(terms: List[SimpleTypeD]): SimpleTypeD = {
    SOr.createOr(terms)
  }

  // returns an OR of ANDs representing the type expressed by the given Bdd.
  // AND terms containing disjoint types have been removed,
  // and each AND term is free of subtype/supertype pairs.
  // Singleton AND and OR terms have been reduced
  // SAnd(x) --> x,  SOr(x) --> x
  def bddToDnf(bdd     : Bdd,
               intToTd : immutable.Map[Int, SimpleTypeD]
               ): SimpleTypeD = {
    def extendChild(bdd: Bdd, t: SimpleTypeD, lineage: List[SimpleTypeD]): List[List[SimpleTypeD]] = {
      // take 1 lineage, which is a list of SimpleTypeD from the root of the Bdd to the current
      //   level but not including the current level.  The job is to add the type resented by the
      //   current level (if it makes sense to do so) and to recur.
      //   Returns a list of lineages representing all the paths from that point
      //   down to the true Bdd node.  lineages get optimized in case of disjoint types
      //   or subtypes, and recursion is pruned when disjoint types are found.
      if (prettyAnd(lineage).disjoint(t).contains(true)) {
        // if the type t is disjoint with something in the lineage, then prune the recursion
        List[List[SimpleTypeD]]()
      } else if (lineage.exists(sub => sub.subtypep(t).contains(true)))
      // if t is supertype of something in lineage, then don't add t to lineage, just recur
        extendPN(bdd, lineage)
      else {
        // remove supertypes from lineage, keeping types which are NOT a super type
        //   also keeping types for which the subtypep question returns dont-know
        val superTypes = lineage.filter(sup => t.subtypep(sup).contains(true))
        if (superTypes.isEmpty)
          extendPN(bdd, t :: lineage) // avoid allocating a new list
        else
          extendPN(bdd, t :: (lineage diff superTypes))
      }
    }

    def extendPN(bdd: Bdd, lineage: List[SimpleTypeD]): List[List[SimpleTypeD]] = {
      // given a bdd node and a lineage list expand the positive and negative children
      //   and append them together.
      bdd match {
        case BddFalse => List()
        case BddTrue => List(lineage)
        case BddNode(label, positive, negative) =>
          extendChild(positive, intToTd(label), lineage) ++ extendChild(negative, SNot(intToTd(label)), lineage)
      }
    }

    prettyOr(extendPN(bdd, List()).map(prettyAnd))
  }

  def main(argv: Array[String]): Unit = {
    Bdd.withNewBddHash {
      trait Trait1
      trait Trait2
      trait Trait3 extends Trait2
      val tds = Seq(SEmpty,
                    STop,
                    SAtomic(classOf[Trait1]),
                    SNot(classOf[Trait1]),
                    SEql(42),
                    SMember(1, 2, 3),
                    SMember(1, 3, 2),
                    evenType, // SSatisfies(evenp)
                    SAnd(classOf[Trait1], classOf[Trait2]),
                    SAnd(classOf[Trait1], classOf[Trait2], classOf[Trait3]),
                    SOr(classOf[Trait1], classOf[Trait2]),
                    SOr(classOf[Trait1], classOf[Trait2], classOf[Trait3]))
      val tdToInt: mutable.Map[SimpleTypeD, Int] = mutable.Map[SimpleTypeD, Int]()
      tds.foreach { td => println(GenusBdd(td, tdToInt).bdd) }
      println(tdToInt)
      tds.foreach { td =>
        val g = GenusBdd(td, tdToInt)
        val dnf = g.dnf
        val cnf = g.cnf
        val can = dnf.canonicalize()
        println() // blank
        println(s"td = $td")
        println(s"dnf           = $dnf")
        println(s"cnf           = $cnf")
        println(s"canonicalized = $can")
      }
    }
  }
}

