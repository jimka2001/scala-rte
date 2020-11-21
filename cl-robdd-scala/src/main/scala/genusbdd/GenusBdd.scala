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
import genus.Types._
import bdd._
import scala.collection._
import NormalForm._

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
  lazy val bdd: Bdd = toBdd(td)
  def toBdd(td:SimpleTypeD):Bdd = {
    td match {
      case SEmpty => BddFalse
      case STop => BddTrue
      case SAtomic(_)
           | SEql(_)
           | SMember(_*)
           | SCustom(_,_) => Bdd(tdToInt.getOrElseUpdate(td.canonicalize(), tdToInt.size+1))
      case SNot(td) => Not(toBdd(td))
      case SAnd(tds @ _*) => And(tds.map(toBdd) :_*)
      case SOr(tds @ _*) => Or(tds.map(toBdd) :_*)
      case _ => ???
    }
  }
  // dnf is a SimpleTypeD which represents the original td
  //   but in a Disjunctive Normal Form.   The DNF is an Or of Ands
  //   where each And has been reduced according to subtype/supertype
  //   relations, and according to disjoint types.
  lazy val dnf: SimpleTypeD = locally{
    val m:immutable.Map[Int,SimpleTypeD] = tdToInt.map(_.swap).toMap
    GenusBdd.BddToDnf(bdd,m)
  }

  // cnf is a SimpleTypeD which represents the original td
  //   but in a Conjunctive Normal Form.  The CNF is an And
  //   of Ors which has been computed by transforming the DNF.
  lazy val cnf: SimpleTypeD = dnf.canonicalize(nf=Some(Cnf))
}

object GenusBdd {

  def BddToDnf(bdd: Bdd,
               intToTd        : immutable.Map[Int, SimpleTypeD]): SimpleTypeD = {
    // We convert a Bdd to Dnf, using SimpleTypeD objects by starting at the
    //   root node of the Bdd, and walking to every BddTrue child.  Along each
    //   walk we accumulate a list of SimpleTypeD objects.  When passing through
    //   a negative child, we invert the object using SNot().  When arriving
    //   at a BddFalse terminal node, we do nothing, but terminate the recursion branch.
    //   When arriving at a BddTrue terminal node, we convert the list of type descriptors
    //   to their intersection using SAnd, and push this SimpleTypeD onto terms.
    //   When finished, we then use SOr to union the SAnd objects in terms.
    //   It might be some some objects in terms is not really an SAnd object because it
    //   has been reduced with a call to the canonicalize method which might convert
    //   it to a more fundamental type.
    var terms = List[SimpleTypeD]()

    def recur(bdd: Bdd, lineage: List[SimpleTypeD]): Any = {
      bdd match {
        case BddFalse => ()
        case BddTrue => terms = SAnd(lineage: _*).canonicalize() :: terms
        case BddNode(label, positive, negative) =>
          recur(positive, intToTd(label) :: lineage)
          recur(negative, SNot(intToTd(label)) :: lineage)
      }
    }

    recur(bdd, List[SimpleTypeD]())
    SOr(terms: _*)
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
                    evenType, // SCustom(evenp)
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

