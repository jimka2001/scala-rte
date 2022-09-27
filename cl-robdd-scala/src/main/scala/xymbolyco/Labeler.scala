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

package xymbolyco

abstract class Labeler[Σ,L] {
  def member(s:Σ,lab:L):Boolean
  def combineLabels(l1:L,l2:L):L
  def equivLabels(a:L,b:L):Boolean = {a == b}
  lazy val universe:L = throw new NotImplementedError(s"missing universe in $this")
  def intersectLabels(l1:L,l2:L):L = throw new NotImplementedError(s"missing intersectLabels in $this")
  def subtractLabels(l1:L,ls:Seq[L]):L = throw new NotImplementedError(s"missing subtractLabels in $this")
  def inhabited(l1:L):Option[Boolean] = throw new NotImplementedError(s"missing inhabited in $this")
  def graphicalText():Seq[String] = Seq()
  def toDot(l1:L):String = l1.toString
  def toLatex(l1:L):String = "unknown"
  def universal(label: L): Boolean = {
    inhabited(subtractLabels(universe, Seq(label))) == Some(false)
  }
}

import genusbdd.GenusBdd

case class GenusBddLabeler() extends Labeler[Any,GenusBdd]() {
  def member(a:Any,gb:GenusBdd):Boolean = {
    gb.typep(a)
  }
  def combineLabels(a:GenusBdd,b:GenusBdd):GenusBdd = {
    a
  }
  override def intersectLabels(l1:GenusBdd,l2:GenusBdd):GenusBdd = ???
  override def subtractLabels(l1:GenusBdd,ls:Seq[GenusBdd]):GenusBdd = ???
  override def inhabited(l1:GenusBdd):Option[Boolean] = ???
}

import genus.SimpleTypeD
case class GenusLabeler() extends Labeler[Any,SimpleTypeD]() {
  import genus._
  def member(a:Any,rt:SimpleTypeD):Boolean = rt.typep(a)
  def combineLabels(a:SimpleTypeD,b:SimpleTypeD):SimpleTypeD = {
    SOr(a,b).canonicalize()
  }
  override lazy val universe:SimpleTypeD = STop
  override def intersectLabels(l1:SimpleTypeD,l2:SimpleTypeD):SimpleTypeD = {
    SAnd(l1,l2).canonicalize()
  }
  override def subtractLabels(l1:SimpleTypeD,ls:Seq[SimpleTypeD]):SimpleTypeD = {
    SAnd(l1,SNot(SOr(ls : _*))).canonicalize()
  }
  override def inhabited(l1:SimpleTypeD):Option[Boolean] = l1.inhabited

  override def graphicalText():Seq[String] = {
    if (SAtomic.getWorldView() == ClosedWorldView)
      Seq("world-view=closed")
    else
      Seq("world-view=open")
  }

  override def toDot(lab:SimpleTypeD):String = {
    lab.toDot()
  }

  override def toLatex(lab:SimpleTypeD):String= {
    lab.toLatex()
  }
}