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

case class Singleton(td:SimpleTypeD) extends Rte {
  override def toLaTeX(): String = td.toLaTeX()
  override def toMachineReadable():String = "Singleton(" + td.toMachineReadable() + ")"
  override def toString: String = "Singleton(" + td.toString + ")" //"<" + td.toString + ">"
  override def toDot():String = td.toDot()

  def nullable: Boolean = false

  def firstTypes: Set[SimpleTypeD] = Set(td)

  def inhabited: Option[Boolean] = td.inhabited
  val flattenTypes = Rte.flattenSingletonTypes
  override def canonicalizeOnce: Rte = {

    td.canonicalize() match {
      case STop => Sigma
      case SEmpty => EmptySet
      case td2 if !flattenTypes => Singleton(td2)
      // convert Singleton(SAnd(...)) to And(...)
      // i.e. convert an Rte of a conjunction type to a conjunction Rte
      case SAnd(operands@_*) => And.createAnd(operands.map(Singleton))
      // convert Singleton(SOr(...)) to Or(...)
      // i.e. convert an Rte of a disjunction type to a disjunction Rte
      case SOr(operands@_*) => Or.createOr(operands.map(Singleton))
      // convert Singleton(SNot(...)) to And(Not(...),Sigma)
      // i.e. convert an Rte of a complement type to a complement Rte
      //  However, the complement Rte matches the empty sequence and also
      //    sequences of length two or more; therefore to be correct we
      //    must intersect with Sigma to filter away empty word and
      //    words of length greater than one.
      case SNot(operand) => And(Not(Singleton(operand)),
                                Sigma)
      // otherwise, create a new Singleton from the canonicalized
      //   version of td, ie, td2
      case td2 => Singleton(td2)
    }
  }

  def derivativeDown(wrt: SimpleTypeD, factors: List[SimpleTypeD], disjoints: List[SimpleTypeD]): Rte = {
    // factors is a list of known supertypes of td (td, being the type encapsulated in this Singleton(td))
    //   factors is intended to be the list t1, t2, ... tn, such that td = SAnd(t1, t2, ..., tn)
    //   however td might have been canonicalized such that retrieving those factors is difficult
    //   or no longer possible.  When derivative is called from Rte:derivatives() these
    //   factors are used to construct td, so they are factors by construction.
    // Likewise, disjoints is a set of types known to be disjoint from td.  When derivative
    //   is called from Rte:derivatives, these disjoint types are generated by construction.
    // If Singleton:derivativeDown is called without such constructed factors and disjoints,
    //   it will attempt to use the .disjoint() and .subtypep() methods to determine
    //   the same thing.  However, sometimes this is impossible as retrieving factors
    //   after canonicalization is difficult or impossible.
    wrt match {
      case `td` => EmptyWord
      case STop => EmptyWord
      case _: SimpleTypeD if factors.contains(td) => EmptyWord
      case _: SimpleTypeD if disjoints.contains(td) => EmptySet
      case td2: SimpleTypeD if td2.disjoint(td).contains(true) => EmptySet
      case td2: SimpleTypeD if td2.subtypep(td).contains(true) => EmptyWord
      case SAnd(tds@_*) if tds.contains(SNot(td)) => EmptySet
      case SAnd(tds@_*) if tds.contains(td) => EmptyWord
      case _ => throw new CannotComputeDerivative(Seq("cannot compute derivative of ",
                                                      s" this = $this",
                                                      s" wrt = $wrt",
                                                      " disjoint= " + wrt.disjoint(td),
                                                      " subtypep = " + wrt.subtypep(td),
                                                      s" factors=$factors",
                                                      s" disjoints=$disjoints").mkString("\n"),
                                                  wrt = wrt,
                                                  rte = this)
    }
  }
  override def derivative(wrt:Option[SimpleTypeD], factors:List[SimpleTypeD], disjoints:List[SimpleTypeD]):Rte = (wrt,td) match {
    case (_,genus.SEmpty) => EmptySet.derivative(wrt, factors, disjoints)
    case (_,genus.STop) => Sigma.derivative(wrt, factors, disjoints)
    case (None,_) => this
    case (_,td) if td.inhabited.contains(false) => EmptySet.derivative(wrt,factors,disjoints)
    case _ => super.derivative(wrt,factors,disjoints)
  }
}