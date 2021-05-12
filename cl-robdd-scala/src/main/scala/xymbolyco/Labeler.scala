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
}

import genusbdd.GenusBdd

case class GenusBddLabeler() extends Labeler[Any,GenusBdd]() {
  def member(a:Any,gb:GenusBdd):Boolean = {
    gb.typep(a)
  }
  def combineLabels(a:GenusBdd,b:GenusBdd):GenusBdd = {
    a
  }
}

import genus.{SimpleTypeD,SOr}
case class GenusLabeler() extends Labeler[Any,SimpleTypeD]() {
  def member(a:Any,rt:SimpleTypeD):Boolean = rt.typep(a)
  def combineLabels(a:SimpleTypeD,b:SimpleTypeD):SimpleTypeD = {
    SOr(a,b).canonicalize()
  }
}