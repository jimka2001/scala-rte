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


class PoliticalMap {
  def uniMapToBiMap(all:Set[String], uniGraph:Map[String,Set[String]]):Map[String,Set[String]] = {
    val empty: Set[String] = Set()
    val map0: Map[String, Set[String]] = Map()
    all.foldLeft(map0) { (m1, state1: String) =>
      uniGraph.get(state1) match {
        case None => m1
        case Some(states) => states.foldLeft(m1) { (m2: Map[String, Set[String]], state2: String) =>
          // add state1 -> state2 and state2 -> state1 into m2 and return new map
          val states1 = m2.getOrElse(state1, empty)
          val states2 = m2.getOrElse(state2, empty)

          (m2 + (state1 -> (states1 + state2))) + (state2 -> (states2 + state1))
        }
      }
    }
  }

  def biMapToUniMap(biGraph:Map[String,Set[String]]):Map[String,Set[String]] = {
    val nil:Map[String,Set[String]] = Map()
    val (newMap,_) =  biGraph.foldLeft((nil, Set[String]())) { case ((m: Map[String, Set[String]], done: Set[String]), (st: String, states: Set[String])) =>
      println(s"$st -> ${states diff done}")
      (m + (st -> (states diff done)), (done + st))
    }
    newMap
  }

  def checkUniMap(all:Set[String],uniGraph:Map[String,Set[String]]):Unit = {
    for {st <- all}
      assert((uniGraph.get(st).nonEmpty ||
        uniGraph.count { case (_, v) =>
          v.contains(st)
        } >= 1), s"fail 1, problem with nothing connects to $st")

    for{ (k,v) <- uniGraph}
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

  def checkBiMap(all:Set[String],biGraph:Map[String,Set[String]]):Unit = {
    // assert that stateBiGraph does not contain any isolates vertices
    assert(all.forall { state => biGraph.get(state).nonEmpty })
  }

  def orderStates(states:List[String], biGraph:Map[String,Set[String]]):List[String] = {

    def recur(states: List[String]): List[String] = {
      val nextState: Option[(String, Set[String])] = biGraph.find { case (state, neighbors) => !states.contains(state) && states.exists { st => neighbors.contains(st) } }
      nextState match {
        case None => states
        case Some((state, _)) => recur(states ++ List(state))
      }
    }

    recur(states)
  }

  val dotProgram:String = locally{
    import java.nio.file.{Paths, Files}
    Seq("/usr/local/bin/neato","/opt/local/bin/neato").find(p => Files.exists(Paths.get(p))) match {
      case None => "neato"
      case Some(path) => path
    }
  }
  def biGraphToDot(biGraph:Map[String,Set[String]],locations:Map[String,(Double,Double)],baseName:String)(symbols:(String=>String)={x=>x},
                                                                                                          colors:(String=>String)={x=>"no-color"}) = {
    val dotPathName = s"/tmp/${baseName}.dot"
    val pngPathName = s"/tmp/${baseName}.png"
    val stream = new java.io.FileOutputStream(new java.io.File(dotPathName))

    def write(str: String): Unit = {
      for {c <- str} {
        stream.write(c.toByte)
      }
    }

    def writeln(str: String): Unit = {
      write(str)
      write("\n")
    }

    val nodes: Map[String, Int] = biGraph.keys.zipWithIndex.toMap
    writeln("graph G {")
    writeln("node [fontsize=32]")
    for {(st, index) <- nodes} {
      val abbreviation = symbols(st)
      write(s"""$index [label="$abbreviation"""")
      if (locations.get(st).nonEmpty) {
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

    // convert the .dot file to a .png file
    locally {
      import sys.process._
      val cmd = Seq(dotProgram, "-Tpng", dotPathName, "-o", pngPathName)
      cmd.!
    }
  }
}

object GenericGraph extends PoliticalMap
