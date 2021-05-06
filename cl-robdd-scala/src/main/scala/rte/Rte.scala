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

abstract class Rte {
  def toLaTeX:String
  override def toString:String = toLaTeX
}

case class Td(td:SimpleTypeD) extends Rte {
  override def toLaTeX:String = td.toString
}

object RteImplicits {

  import scala.language.implicitConversions

  implicit def tdToTd(raw: SimpleTypeD): Td = {
    Td(raw)
  }
  implicit def classToTd(raw: Class[_]): Td = {
    Td(genus.SAtomic(raw))
  }
}

final case class Cat(operands:Seq[Rte]) extends Rte {
  override def toLaTeX:String = "(" ++  operands.map(_.toLaTeX).mkString("\\cdot")  ++ ")"
}

object Cat {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new Cat(operands)
}

case class EmptyWord() extends Rte {
  override def toLaTeX:String = "\\varepsilon"
}

case class EmptySet() extends Rte {
  override def toLaTeX:String = "\\emptyset"
}

case class Or(operands:Seq[Rte]) extends Rte {
  override def toLaTeX:String = "(" ++  operands.map(_.toLaTeX).mkString("\\vee")  ++ ")"
}

object Or {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new Or(operands)
}

case class And(operands:Seq[Rte]) extends Rte{
  override def toLaTeX:String = "(" ++  operands.map(_.toLaTeX).mkString("\\wedge")  ++ ")"
}

object And {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new And(operands)
}

case class Not(operand:Rte) extends Rte {
  override def toLaTeX:String = "\\overline{" ++  operand.toLaTeX ++ "}"
}

case class Star(operand:Rte) extends Rte {
  override def toLaTeX:String = "(" ++  operand.toLaTeX ++  ")" ++ "^{*}"
}

object sanityTest {
  def main(argv: Array[String]):Unit = {
    import genus._
    println(Or(And(Td(SAtomic(classOf[Integer])),
                   Not(Td(SAtomic(classOf[Long])))),
               Not(Td(SEql(42)))))

    import RteImplicits._
    println(Or(And(SAtomic(classOf[Integer]),
                   Not(SAtomic(classOf[Long]))),
               Not(SEql(43))))

    println(Or(And(classOf[Integer],
                   Not(SAtomic(classOf[Long]))),
               Not(SEql(44))))
  }
}