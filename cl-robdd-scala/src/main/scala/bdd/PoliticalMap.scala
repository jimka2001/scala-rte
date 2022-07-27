// Copyright (c) 2019,20 EPITA Research and Development Laboratory
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

package bdd

import adjuvant.Adjuvant.openGraphicalFile

import scala.annotation.tailrec


class PoliticalMap[V] {
  def uniMapToBiMap(all:Set[V], uniGraph:Map[V,Set[V]]):Map[V,Set[V]] = {
    val empty: Set[V] = Set()
    val map0: Map[V, Set[V]] = Map()
    all.foldLeft(map0) { (m1, state1: V) =>
      uniGraph.get(state1) match {
        case None => m1
        case Some(states) => states.foldLeft(m1) { (m2: Map[V, Set[V]], state2: V) =>
          // add state1 -> state2 and state2 -> state1 into m2 and return new map
          val states1 = m2.getOrElse(state1, empty)
          val states2 = m2.getOrElse(state2, empty)

          (m2 + (state1 -> (states1 + state2))) + (state2 -> (states2 + state1))
        }
      }
    }
  }

  def biMapToUniMap(biGraph:Map[V,Set[V]]):Map[V,Set[V]] = {
    val nil:Map[V,Set[V]] = Map()
    val (newMap,_) =  biGraph.foldLeft((nil, Set[V]())) {
      case ((m: Map[V, Set[V]], done: Set[V]), (st, states: Set[V])) =>
        (m + (st -> (states diff done)), done + st)
    }
    newMap
  }

  def checkUniMap(all:Set[V],uniGraph:Map[V,Set[V]]):Unit = {
    for {st <- all}
      assert(uniGraph.contains(st) ||
        uniGraph.count { case (_, v) =>
          v.contains(st)
        } >= 1, s"fail 1, problem with nothing connects to $st")

    for{ (k,_) <- uniGraph}
      assert(      all.contains(k), s"fail 2 expecting allStates contains $k")
    for{ (k,v) <- uniGraph}
      assert( v.subsetOf(all), s"fail 3, $k connects to non-existing ${v diff all}")

    assert(uniGraph.forall { case (k, v) =>
      // if x -> y then not y -> x
      v.forall { s =>
        !uniGraph.getOrElse(s, Set()).contains(k)
      }
    }, "fail4")
  }

  def checkBiMap(all:Set[V],biGraph:Map[V,Set[V]]):Unit = {
    // assert that stateBiGraph does not contain any isolates vertices
    assert(all.forall { state => biGraph.contains(state) })
  }

  val dotProgram:String = locally{
    import java.nio.file.{Paths, Files}
    Seq("/usr/local/bin/neato","/opt/local/bin/neato").find(p => Files.exists(Paths.get(p))) match {
      case None => "neato"
      case Some(path) => path
    }
  }
}

object GenericGraph extends PoliticalMap {
  def orderStates[V](states:List[V], biGraph:Map[V,Set[V]]):List[V] = {

    @tailrec
    def recur(states: List[V]): List[V] = {
      val nextState: Option[(V, Set[V])] = biGraph.find {
        case (state, neighbors) => !states.contains(state) && states.exists { st => neighbors.contains(st) }
      }
      nextState match {
        case None => states
        case Some((state, _)) => recur(states ++ List(state))
      }
    }

    recur(states)
  }

  def biGraphToDot[V](biGraph:Map[V,Set[V]],
                   locations:Map[V,(Double,Double)],
                   baseName:String)(symbols: V=>V ={ x:V=>x},
                                    colors: V=>String = { _:V=>"no-color"},
                                    view:Boolean,
                                    verbose:Boolean): Int = {
    val dotPathName = s"/tmp/$baseName.dot"
    val pngPathName = s"/tmp/$baseName.png"
    val stream = new java.io.FileOutputStream(new java.io.File(dotPathName))
    if(verbose)
      println(s"[writing to $dotPathName")
    def write(str: String): Unit = {
      for {c <- str} {
        stream.write(c.toByte)
      }
    }

    def writeln(str: String): Unit = {
      write(str)
      write("\n")
    }

    val nodes: Map[V, Int] = biGraph.keys.zipWithIndex.toMap
    writeln("graph G {")
    writeln("node [fontsize=32]")
    for {(st, index) <- nodes} {
      val abbreviation = symbols(st)
      write(s"""$index [label="$abbreviation"""")
      if (locations.contains(st)) {
        val (lat, lon) = locations(st)
        write(s""" pos="$lon,$lat!"""")
      }
      if (colors(st) != "no-color") {
        write(s""" style=filled fillcolor=${colors(st)}""")
      }
      writeln(s"]")
    }
    for {(st1, states) <- biGraph
         i1 = nodes(st1)
         st2 <- states
         i2 = nodes(st2)
         if i1 < i2 // only write one connection between two nodes
         }
      writeln(s" $i1 -- $i2")

    writeln("}")
    stream.close()
    if(verbose)
      println(s"finished $dotPathName ]")
    // convert the .dot file to a .png file
    locally {
      import sys.process._
      if(verbose)
        println(s"[writing to $pngPathName")
      val cmd = Seq(dotProgram, "-Tpng", dotPathName, "-o", pngPathName)
      val status = cmd.!
      if (view)
        openGraphicalFile(pngPathName)
      if(verbose)
        println(s"finished $pngPathName ]")
      status
    }
  }
}
