// Copyright (c) 2020,21 EPITA Research and Development Laboratory
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

package genus

import NormalForm._
import adjuvant.Adjuvant._
import scala.collection.mutable

/** A general type of our type system. */
abstract class SimpleTypeD { // SimpleTypeD
  def toMachineReadable():String = toString

  def ||(t: SimpleTypeD): SimpleTypeD = SOr(this, t).canonicalize(nf = Some(Dnf))

  def &&(t: SimpleTypeD): SimpleTypeD = SAnd(this, t).canonicalize(nf = Some(Dnf))

  def unary_! : SimpleTypeD = SNot(this).canonicalize(nf = Some(Dnf))

  def -(t: SimpleTypeD): SimpleTypeD = SAnd(this, SNot(t)).canonicalize(nf = Some(Dnf))

  def ^^(t: SimpleTypeD): SimpleTypeD = SOr(SAnd(this, SNot(t)),
                                            SAnd(SNot(this), t)).canonicalize(nf = Some(Dnf))

  /** Returns whether a given object belongs to this type.
   * It is a set membership test.
   *
   * @param a the object we want to check the type
   * @return a Boolean which is true is a is of this type
   */
  def typep(a: Any): Boolean


  val knownDisjoint:mutable.Map[(WorldView,SimpleTypeD),Option[Boolean]] =
    mutable.Map[(WorldView,SimpleTypeD),Option[Boolean]]()

  /** Returns whether a given type and this type are disjoint.
   * This might be undecidable. The disjointDown method is used to avoid
   * infinite loops.  Subclasses are expected to override disjointDown,
   * not disjoint.
   *
   * @param td the type we want to check whether it is disjoint to this type
   * @return an optional Boolean which is true if t and this type are disjoint
   */
  def disjoint(td: SimpleTypeD): Option[Boolean] = {
    knownDisjoint
      .getOrElseUpdate((SAtomic.getWorldView(),td), locally {
        val d1 = disjointDown(td)
        lazy val d2 = td.disjointDown(this)
        lazy val c1 = this.canonicalize()
        lazy val c2 = td.canonicalize()
        lazy val dc12 = c1.disjointDown(c2)
        lazy val dc21 = c2.disjointDown(c1)

        if (this == td && inhabited.nonEmpty)
          inhabited.map(!_)
        else if (d1.nonEmpty)
          d1
        else if (d2.nonEmpty)
          d2
        else if (c1 == this && c2 == td)
        // no need to continue searching if canonicalization failed to produce simpler forms
          None
        else if (c1 == c2 && c1.inhabited.nonEmpty)
          c1.inhabited.map(!_)
        else if (dc12.nonEmpty)
          dc12
        else {
          dc21
          // TODO we can also check whether c1.subtypep(c2) and c1.inhabited
          //    careful not to cause an infinite loop
        }
      })
  }

  // inhabitedDown should not be called directly, except as super.inhabitedDown,
  // rather the variable inhabited should be referenced, ensuring that the
  // same computation not be done twice.
  protected def inhabitedDown: Option[Boolean] = None

  lazy val inhabited: Option[Boolean] = inhabitedDown

  protected def disjointDown(t: SimpleTypeD): Option[Boolean] = {
    if (inhabited.contains(false)) // an empty type is disjoint to every other type
      Some(true)
    else
      None
  }

  val knownSubtypes:mutable.Map[(WorldView,SimpleTypeD),Option[Boolean]] =
    mutable.Map[(WorldView,SimpleTypeD),Option[Boolean]]()

  /** Returns whether this type is a recognizable subtype of another given type.
   * It is a subset test. This might be undecidable.
   *
   * @param t the type we want to check whether this type is included in
   * @return an optional Boolean which is true if this type is a subtype of t
   */
  def subtypep(t: SimpleTypeD): Option[Boolean] = {
    knownSubtypes
      .getOrElseUpdate((SAtomic.getWorldView(),t),
                       locally {
                         lazy val orResult = t match {
                           case SOr(args@_*) if args.exists(a => subtypep(a).contains(true)) => Some(true)
                           case _ => None
                         }
                         lazy val andResult = t match {
                           case SAnd(args@_*) if args.forall(a => subtypep(a).contains(true)) => Some(true)
                           case _ => None
                         }
                         if ((t.getClass eq this.getClass)
                           && (t == this))
                           Some(true)
                         else if (t.canonicalize() == STop)
                           Some(true)
                         else if (orResult.contains(true))
                           orResult
                         else if (andResult.contains(true))
                           andResult
                         else {
                           subtypepDown(t)
                         }
                       }
                       )
  }

  protected def subtypepDown(t: SimpleTypeD): Option[Boolean] = {
    if (t match {
      case SNot(b) if disjoint(b).contains(true) => true
      case _ => false
    }) {
      Some(true)
    }
      //   sub        super
    else (inhabited, t.inhabited) match {
      // empty set is subset of every set even if we don't know know whether the superset is inhabited or not
      case (Some(false),_) => Some(true)
      case (Some(true),Some(false)) => Some(false)
      case _ => None
      //case (None, _) => None
      //case (_, None) => None
      //case (_, Some(false)) => Some(false) // no inhabited type is a subtype of the empty type
      //
    }
  }
  
  lazy val toDnf: SimpleTypeD = computeDnf()

  def computeDnf(): SimpleTypeD = this

  lazy val toCnf: SimpleTypeD = computeCnf()

  def computeCnf(): SimpleTypeD = this

  def toNf(nf: Option[NormalForm] = None): SimpleTypeD = {
    nf match {
      case Some(Dnf) => toDnf
      case Some(Cnf) => toCnf
      case _ => this
    }
  }

  // canonicalizeN is useful for debugging,  with verbose=true it will print
  // the various *increments* of canonicalization, by calling canonicalizeOnce
  // n times.
  def canonicalizeN(n:Int, verbose:Boolean, nf: Option[NormalForm] = None): SimpleTypeD = {
    if (n < 0)
      this
    else {
      if (verbose)
        println(s"$n: $this")
      canonicalizeOnce(nf).canonicalizeN(n-1, verbose, nf)
    }
  }

  def canonicalizeOnce(nf: Option[NormalForm] = None): SimpleTypeD = this

  val canonicalizedHash: scala.collection.mutable.Map[Option[NormalForm], SimpleTypeD] = scala.collection.mutable.Map()

  def canonicalize(nf: Option[NormalForm] = None): SimpleTypeD = {
    canonicalizedHash
      .getOrElseUpdate(nf,
                       // if not already memoized, then perhaps compute a new object, using fixed-point
                       fixedPoint(this,
                                  (t: SimpleTypeD) => t.canonicalizeOnce(nf = nf),
                                  (a: SimpleTypeD, b: SimpleTypeD) => a.getClass == b.getClass && a == b))
    // tell the perhaps new object it is already canonicalized
    canonicalizedHash(nf).canonicalizedHash(nf) = canonicalizedHash(nf)
    // return the perhaps new object which knows it is canonicalized
    canonicalizedHash(nf)
  }

  /** Returns whether this type is a recognizable supertype of another given type.
   * It is a superset test. This might be undecidable.
   *
   * @param t the type we want to check the inclusion in this type
   * @return an optional Boolean which is true if this type is a supertype of t
   */
  def supertypep(t: SimpleTypeD): Option[Boolean] = t.subtypep(this)

  /* given another abject which we know is the same class as `this`,
  compare then in a way which is compatible with
  */
  def cmpToSameClassObj(t: SimpleTypeD): Boolean = {
    throw new Exception(s"cannot compare type designators ${this.getClass} vs ${t.getClass}")
  }

  def typeEquivalent(t: SimpleTypeD): Option[Boolean] = {
    lazy val can1 = canonicalize()
    lazy val can2 = t.canonicalize()

    lazy val sp1 = subtypep(t)
      .orElse(can1.subtypep(t))
      .orElse(can1.subtypep(can2))
    lazy val sp2 = t.subtypep(this)
      .orElse(can2.subtypep(this))
      .orElse(can2.subtypep(can1))

    if (sp1.contains(false) || sp2.contains(false))
      Some(false)
    else if (sp1.contains(true) && sp2.contains(true))
      Some(true)
    else
      None
  }
}
