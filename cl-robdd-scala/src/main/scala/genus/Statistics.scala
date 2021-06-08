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

import NormalForm._
import Types._

object Statistics {
  import RandomType.randomType
  def measureSubtypeComputability(n:Int,depth:Int,inh:Boolean):Map[String,Double] = {
    assert(n > 0, s"measureSubtypeComputability does not support n=$n")
    val m = (0 until n).foldLeft(Map[String, Int]()) { case (m, _) =>
      val rt1 = if (inh)
        randomType(depth, _.inhabited.contains(true))
      else
        randomType(depth)
      val rt2 = if (inh)
        randomType(depth, td => td.inhabited.contains(true) && td.typeEquivalent(rt1).contains(false))
      else
        randomType(depth)
      val can1 = rt1.canonicalize(Some(Dnf))
      val can2 = rt2.canonicalize(Some(Dnf))
      val s1 = rt1.subtypep(rt2)
      val s2 = can1.subtypep(can2)
      Map("inhabited" -> (m.getOrElse("inhabited",0) + (if (rt1.inhabited.contains(true) ) 1 else 0)),
          "inhabited DNF" -> (m.getOrElse("inhabited DNF",0) + (if (can1.inhabited.contains(true) ) 1 else 0)),
          "equal" -> (m.getOrElse("equal",0) + (if (can1.typeEquivalent(can2).contains(true)) 1 else 0)),
          "subtype True" -> (m.getOrElse("subtype True",0) + (if (s1.contains(true)) 1 else 0)),
          "subtype False" -> (m.getOrElse("subtype False",0) + (if (s1.contains(false)) 1 else 0)),
          "subtype Some" -> (m.getOrElse("subtype Some",0) + (if (s1.nonEmpty) 1 else 0)),
          "subtype None" -> (m.getOrElse("subtype None",0) + (if (s1.isEmpty) 1 else 0)),
          "subtype DNF True" -> (m.getOrElse("subtype DNF True",0) + (if (s2.contains(true)) 1 else 0)),
          "subtype DNF False" -> (m.getOrElse("subtype DNF False",0) + (if (s2.contains(false)) 1 else 0)),
          "subtype DNF Some" -> (m.getOrElse("subtype DNF Some",0) + (if (s2.nonEmpty) 1 else 0)),
          "subtype DNF None" -> (m.getOrElse("subtype DNF None",0) + (if (s2.isEmpty) 1 else 0)),
          // how many were not computed a original but became computable after DNF
          "gained" -> (m.getOrElse("gained",0) + (if (s1.isEmpty && s2.nonEmpty) 1 else 0)),
          // how many lost computability after DNF
          "lost" -> (m.getOrElse("lost",0) + (if (s1.nonEmpty && s2.isEmpty) 1 else 0)))
    }
    m.map{case (k,v) => k -> 100 * v.toDouble / n}
  }


  def main(args: Array[String]): Unit = {
    val p = measureSubtypeComputability(10000, 3, inh=true)
    p.foreach { case (k, v) => println(s"$k = $v") }
  }
}
