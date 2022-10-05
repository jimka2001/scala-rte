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

package genus

import RandomType._
import NormalForm._
import adjuvant.Adjuvant.{findSimplifier, uniquify, diff, eql}

// The purpose of this class, SCombination, is to serve as a superclass
// of both SAnd and SOr, as there is quite a bit of common code between
// the two classes (SAnd and SOr).  In some cases factoring the code
// out means replacing SEmpty with STop, or vice versa, and
// replacing subtypep with supertypep, etc.
abstract class SCombination(val tds: SimpleTypeD*) extends SimpleTypeD {
  def create(tds: Seq[SimpleTypeD]): SimpleTypeD

  def createDual(tds: Seq[SimpleTypeD]): SimpleTypeD

  val unit: SimpleTypeD
  val zero: SimpleTypeD

  // TODO, not sure what is the correct name for this function.
  //   it is the method which is b.subtypep(a) for SOr  equiv a.supertypep(b)
  //                         and a.subtypep(b) for SAnd equiv b.supertypep(a)
  def annihilator(a: SimpleTypeD, b: SimpleTypeD): Option[Boolean]

  def sameCombination(td: SimpleTypeD): Boolean = false

  def dualCombination(td: SimpleTypeD): Boolean = false

  def combinator[A](a:Seq[A],b:Seq[A]):Seq[A]
  def dualCombinator[A](a:Seq[A],b:Seq[A]):Seq[A]
  def comboFilter[A](seq:Seq[A], f:A=>Boolean):Seq[A]

  def conversion1(): SimpleTypeD = {
    if (tds.isEmpty) {
      // (and) -> STop,  unit=STop,   zero=SEmpty
      // (or) -> SEmpty, unit=SEmpty,   zero=STop
      unit
      // (and A) -> A
      // (or A) -> A
    } else if (tds.tail.isEmpty)
      tds.head
    else
      this
  }

  def conversion2(): SimpleTypeD = {
    // (and A B SEmpty C D) -> SEmpty,  unit=STop,   zero=SEmpty
    // (or A B STop C D) -> STop,     unit=SEmpty,   zero=STop
    if (tds.contains(zero))
      zero
    else
      this
  }

  def conversion3(): SimpleTypeD = {

    // (and A (not A)) --> SEmpty,  unit=STop,   zero=SEmpty
    // (or A (not A)) --> STop,     unit=SEmpty, zero=STop
    if (tds.exists(td => tds.contains(SNot(td))))
      zero
    else
      this
  }

  def conversion4(): SimpleTypeD = {
    // SAnd(A,STop,B) ==> SAnd(A,B),  unit=STop,   zero=SEmpty
    // SOr(A,SEmpty,B) ==> SOr(A,B),  unit=SEmpty, zero=STop
    if (tds.contains(unit))
      create(tds.filterNot(_ == unit))
    else
      this
  }

  def conversion5(): SimpleTypeD = {
    // (and A B A C) -> (and A B C)
    // (or A B A C) -> (or A B C)
    create(adjuvant.Adjuvant.uniquify(tds))
  }

  def conversion6(): SimpleTypeD = {
    // (and A (and B C) D) --> (and A B C D)
    // (or A (or B C) D) --> (or A B C D)
    if (!tds.exists(sameCombination))
      this
    else {
      create(tds.flatMap {
        case td: SCombination if sameCombination(td) => td.tds
        case x => Seq(x)
      })
    }
  }

  def conversion7(nf: Option[NormalForm]): SimpleTypeD = {
    toNf(nf)
  }

  def conversion8(): SimpleTypeD = {
    // (or A (not B)) --> STop   if B is subtype of A,    zero=STop
    // (and A (not B)) --> SEmpty   if B is supertype of A,   zero=SEmpty
    tds.find { a =>
      tds.exists {
        case SNot(b) if annihilator(a, b).contains(true) => true
        case _ => false
      }
    } match {
      case None => this
      case Some(_) => zero
    }
  }

  def conversion9(): SimpleTypeD = {
    // (A+B+C)(A+!B+C)(X) -> (A+B+C)(A+C)(X)  (later -> (A+C)(X))
    // (A+B+!C)(A+!B+C)(A+!B+!C) -> (A+B+!C)(A+!B+C)(A+!C) ->
    // (A+B+!C)(A+!B+C)(A+!B+!C) -> does not reduce to (A+B+!C)(A+!B+C)(A)
    import adjuvant.Adjuvant.searchReplace
    val duals = tds.collect { case td: SCombination if dualCombination(td) => td }
    val outerArgs = tds.map {
      // A+!B+C -> A+C
      // A+B+C -> A+B+C
      // X -> X
      case td1: SCombination if dualCombination(td1) =>
        // A+!B+C -> A+C
        // A+B+C -> A+B+C
        val toRemove = td1.tds.collectFirst {
          case td@SNot(n) if duals.exists {
            case td2: SCombination if dualCombination(td2) => td2.tds == searchReplace(td1.tds, td, Seq(n))
          } => td
        } // Some(!B) or None
        toRemove match {
          case None => td1
          case Some(td) => createDual(td1.tds.filterNot(_ == td))
        }
      case td => td // X -> X
    }
    create(outerArgs)
  }

  def conversion10(): SimpleTypeD = {
    // (and A B C) --> (and A C) if  A is subtype of B
    // (or A B C) -->  (or B C) if  A is subtype of B
    tds.find(sub => tds.exists { sup =>
      ((sub != sup)
        && annihilator(sub, sup).contains(true))
    }) match {
      case None => this
      case Some(sub) =>
        // throw away all proper superclasses of sub, i.e., keep everything that is not a superclass
        // of sub and also keep sub itself.   keep false and dont-know
        val keep = tds.filter(sup => sup == sub || !annihilator(sub, sup).contains(true))
        create(keep)
    }
  }

  def conversion11(): SimpleTypeD = {
    // A + A! B -> A + B
    // A + AB --> A
    // A + AB + C --> A + C
    // A + A! BX + Y = (A + BX + Y)
    // A + ABX + Y = (A + Y)

    // TODO need to look at these relations if A < B or B < A,
    //  I'm not yet sure what will happen, but I think there are some simplifications to be made
    val ao = tds.find(a =>
                        tds.exists {
                          case td: SCombination if dualCombination(td) =>
                            td.tds.exists {
                              case SNot(b) if a == b => true
                              case b if a == b => true
                              case _ => false
                            }
                          case _ => false
                        })
    ao match {
      case None => this
      case Some(a) =>
        create(
          tds.flatMap {
            case td: SCombination if sameCombination(td) => Seq(td)
            case td: SCombination if td.tds.contains(a) => Seq()
            case td: SCombination if td.tds.contains(SNot(a)) => Seq(createDual(td.tds.filterNot(_ == SNot(a))))
            case b => Seq(b)
          })
    }
  }

  def conversion12(): SimpleTypeD = {
    // AXBC + !X = ABC + !X
    tds.find {
      case SNot(x) => tds.exists {
        case td: SCombination if dualCombination(td) => td.tds.contains(x)
        case _ => false
      }
      case _ => false
    } match {
      case Some(SNot(x)) => create(tds.map {
        case td: SCombination if dualCombination(td) => createDual(td.tds.filter(_ != x))
        case a => a
      })
      case _ => this
    }
  }

  def conversion13():SimpleTypeD = {
    // multiple !member
    // SOr( x,!{-1,1},!{1,2,3,4})
    //   --> SOr(x,!{1})   // intersection of non-member
    // SAnd( x,!{-1,1},!{1,2,3,4})
    //   --> SOr(x,!{-1,1,2,3,4})   // union of non-member

    val notMembers = tds.collect{
      case nc@SNot(_:SMemberImpl)  => nc
    }
    if (notMembers.size <= 1)
      this
    else {
      val newNotMember = SNot(createMemberFromPairs(notMembers.map{
        case SNot(td:SMemberImpl) => td.xs
        case _ => throw new Exception("scalac is not smart enough to know this will never happen")
      }.reduce(dualCombinator[(SimpleTypeD,Any)])))

      create(uniquify(tds.map{
        case SNot(_:SMemberImpl) => newNotMember
        case td => td
      }))
    }
  }
  def conversion14():SimpleTypeD = {
    // multiple member
    // (or (member 1 2 3) (member 2 3 4 5)) --> (member 1 2 3 4 5)
    // (and (member 1 2 3) (member 2 3 4 5)) --> (member 2 3)

    val members = tds.collect{
      case m:SMemberImpl => m
    }
    if (members.size <= 1)
      this
    else {
      val newMember = createMemberFromPairs(members.map(_.xs).reduce(combinator[(SimpleTypeD,Any)]))

      create(uniquify(tds.map{
        case _:SMemberImpl => newMember
        case td => td
      }))
    }
  }

  def conversion15():SimpleTypeD = {
    // SAnd(X, member, not-member)
    // SOr(X, member,  not-member)
    def diffPairs(xs:Vector[(SimpleTypeD,Any)],ys:Vector[(SimpleTypeD,Any)]):Vector[(SimpleTypeD,Any)] = {
      xs.filter(x => !ys.contains(x))
    }
    val member = tds.collectFirst {
      case m: SMemberImpl => m
    }
    val notMember = tds.collectFirst {
      case SNot(m: SMemberImpl) => m
    }
    // Here we have a case for SAnd and SOr,
    //   ideally it should be treated in an OO way, but I'm
    //   not sure how to do it.
    (this,member,notMember) match {
      case (_:SAnd,Some(m),Some(n)) =>
        //    SAnd({1,2,3,a},SNot({1,2,3,b})
        // --> SAnd({a}                    )
        create(tds.flatMap {
          case SNot(n2: SMemberImpl) if eql(n2.xs, n.xs) => Seq()
          case m2:SMemberImpl if m2 == m=>
            Seq(createMemberFromPairs(diffPairs(m.xs,n.xs)))
          case td => Seq(td)
        })
      case (_:SOr,Some(m),Some(n)) =>
        //    SOr({1,2,3,a},SNot({1,2,3,b})
        // --> SOr(        ,SNot({b})
        create(tds.flatMap {
          case m2: SMemberImpl if eql(m2,m) => Seq()
          case SNot(n2: SMemberImpl) if n2 == n =>
            Seq(SNot(createMemberFromPairs(diffPairs(n.xs,m.xs))))
          case td => Seq(td)
        })
      case (_,_,_) => this
    }
  }

  def conversion16():SimpleTypeD = {
    // Now (after conversions 13, 14, and 15, there is at most one SMember(...) and
    //   at most one SNot(SMember(...))

    // (and Double (not (member 1.0 2.0 "a" "b"))) --> (and Double (not (member 1.0 2.0)))

    val fewer: Seq[SimpleTypeD] = tds.flatMap {
      case _: SMemberImpl => Seq()
      case SNot(_: SMemberImpl) => Seq()
      case td => Seq(td)
    }
    // stricter in the case of SOr, but laxer in the case of SAnd
    val stricter:SimpleTypeD = create(fewer)
    def f(pair:(SimpleTypeD,Any)):Boolean = {
      stricter.typep(pair._2)
    }
    val newargs = tds.map {
      case m: SMemberImpl => createMemberFromPairs(comboFilter(m.xs, f))
      case SNot(m: SMemberImpl) => SNot(createMemberFromPairs(comboFilter(m.xs, f)))
      case td => td
    }
    create(newargs)
  }

  def conversion17():SimpleTypeD = {

    // A !B + A B --> A
    // (A + !B)(A + B) -> A
    def f(td:SCombination):SimpleTypeD = {
      if (!dualCombination(td))
        td
      else {
        //# e.g., SOr(SAnd(a,b,c...),...,SAnd(a,b,!c...))
        //# -->   SOr(SAnd(a,b),...SAnd(a,b,!c))
        val others = tds.collect {
          case c: SCombination if c != td && dualCombination(c) => c
        }

        def except_for(c: SimpleTypeD, tds1: Seq[SimpleTypeD], tds2: Seq[SimpleTypeD]): Boolean = {
          if (tds1.size != tds2.size)
            false
          else {
            val shorter = tds1.filter(x => x != c)
            c match {
              case SNot(s) => shorter == tds2.filter(x=> x != s)
              case _ =>
                val nc = SNot(c)
                shorter == tds2.filter(x => x != nc)
            }
          }
        }

        // is there an element, c, of the arglist of td and an x in others
        // such that x.tds has the same elements as td.tds
        // except that c is in td.tds and !c is in x.tds
        // or          !c is in td.tds and c is in x.tds
        // if so remove c or !c from td.tds
        td.tds.find(c =>
                      others.exists(x => except_for(c, td.tds, x.tds))
                    ) match {
          case None => td
          case Some(negated) => createDual(td.tds.filter(x => x != negated))
        }
      }
    }
    create(tds.map {
      case c: SCombination => f(c)
      case td => td
    })
  }

  def conversionD1():SimpleTypeD

  def conversionD3():SimpleTypeD

  def conversion98():SimpleTypeD = {
    create(tds.sortWith(cmpTypeDesignators))
  }

  def conversion99(nf:Option[NormalForm]):SimpleTypeD = {
    create(tds.map(_.canonicalize(nf)))
  }

  def conversion177(): SimpleTypeD

  // SCombination(tds: SimpleTypeD*)
  override def canonicalizeOnce(nf: Option[NormalForm] = None): SimpleTypeD = {
    findSimplifier(tag="SCombo", this, verbose=false, List[(String,() => SimpleTypeD)](
      "1" -> conversion1,
      "2" -> conversion2,
      "3" -> conversion3,
      "4" -> conversion4,
      "5" -> conversion5,
      "6" -> conversion6,
      "7" -> (() => { conversion7(nf) }),
      "8" -> conversion8,
      "9" -> conversion9,
      "10" -> conversion10,
      "11" -> conversion11,
      "12" -> conversion12,
      "13" -> conversion13,
      "14" -> conversion14,
      "15" -> conversion15,
      "16" -> conversion16,
      "17" -> conversion17,
      "D1" -> conversionD1,
      "D3" -> conversionD3,
      "177" -> conversion177,
      "98" -> conversion98,
      "99" -> (() => { conversion99(nf) })
      ))
  }

  // SCombination(tds: SimpleTypeD*)
  override def cmpToSameClassObj(td: SimpleTypeD): Boolean = {
    // this method is only called if td and this have exactly the same class
    if (this == td)
      false // sortWith requires returning false if A not < B, including the case that A == B.
    else td match {
      case sc: SCombination => compareSequence(this.tds, sc.tds)
      case _ => super.cmpToSameClassObj(td) // throws an exception
    }
  }

  def computeNf():SimpleTypeD = {
    // it turns out SAnd.compute_dnf and SOr.compute_cnf contain
    // the exact same code.  So I've factored that code here.
    //
    // convert SOr( x1, x2, SAnd(y1,y2,y3), x3, x4)
    //    --> td = SAnd(y1,y2,y3)
    // --> SAnd(SOr(x1,x2,  y1,  x3,x4),
    //          SOr(x1,x2,  y2,  x3,x4),
    //          SOr(x1,x2,  y3,  x3,x4),
    //     )
    // convert SAnd( x1, x2, SOr(y1,y2,y3), x3, x4)
    //    --> td = SOr(y1,y2,y3)
    // --> SOr(SAnd(x1,x2,  y1,  x3,x4),
    //          SAnd(x1,x2,  y2,  x3,x4),
    //          SAnd(x1,x2,  y3,  x3,x4),
    //     )
    tds.collectFirst{
      case c:SCombination if dualCombination(c) => c
    } match {
      case None => this
      case Some(td) => createDual(td.tds.map(y => create(tds.map(x => if (x == td) y else x))))
    }
  }
}