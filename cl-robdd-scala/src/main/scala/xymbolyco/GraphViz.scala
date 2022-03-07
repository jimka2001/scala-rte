// Copyright (c) 2019,21 EPITA Research and Development Laboratory
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

import adjuvant.Adjuvant.openGraphicalFile

object GraphViz {

  import java.io.{File, OutputStream}

  def dfaView[Sigma,L,E](dfa: Dfa[Sigma,L,E], title:String="", abbrev:Boolean=false,
                        label:Option[String]=None): String = {
    val png = dfaToPng(dfa, title, abbrev=abbrev,label=label)
    openGraphicalFile(png)
  }

  def dfaToPng[Sigma,L,E](dfa:Dfa[Sigma,L,E], title:String, abbrev:Boolean,
                          label:Option[String]=None): String = {
    val prefix = if (title == "")
      "dfa"
    else
      title
    val png = File.createTempFile(prefix+"-", ".png")
    val pngPath = png.getAbsolutePath
    val dot = File.createTempFile(prefix+"-", ".dot")
    val dotPath = dot.getAbsolutePath
    val alt = File.createTempFile(prefix+"-", ".plain")
    val altPath = alt.getAbsolutePath
    val longTitle:String = label match {
      case None => title
      case Some(lab) => s"$title\\l-- $lab"
    }
    dfaToPng(dfa, dotPath, longTitle, abbrev=abbrev)
    locally {
      import sys.process._
      Seq("dot", "-Tplain", dotPath, "-o", altPath).! // write file containing coordinates
      val cmd = Seq("dot", "-Tpng", dotPath, "-o", pngPath)
      //println(s"cmd = $cmd")
      cmd.!
    }
    //png.deleteOnExit()
    //dot.deleteOnExit()
    pngPath
  }

  def dfaToPng[Sigma,L,E](dfa:Dfa[Sigma,L,E], pathname: String, title:String, abbrev:Boolean): String = {
    val stream = new java.io.FileOutputStream(new java.io.File(pathname))
    dfaToDot(dfa, stream, title, abbrev = abbrev)
    stream.close()
    pathname
  }

  def dfaToDot[Sigma,L,E](dfa:Dfa[Sigma,L,E], stream: OutputStream, title:String, abbrev:Boolean): Unit = {
    val qarr=dfa.Q.toArray
    val labels:Set[L] = for { q <- dfa.Q
                               (_,transitions) <- q.transitions.groupBy(_.destination)
                               labels = transitions.map(_.label)
                               label = labels.reduce(dfa.labeler.combineLabels)
                              } yield label
    val labelMap:Map[L,String] = labels.toSeq.zipWithIndex.map{ case (lab,i:Int) =>
      // TODO if abbreviateTransitions is true, need to create a label in the .dot
      //   file indicating the mapping from abbrev to actual label
      if (abbrev)
        lab -> s"t$i"
      else
        lab -> lab.toString
    }.toMap
    def write(str: String): Unit = {
      for {c <- str} {
        stream.write(c.toByte)
      }
    }
    def arrow(from:Int,to:Int,label:String):Unit = {
      write(s"$from -> $to")
      if(label != "")
        write(s""" [label="$label"]""")
      write("\n")
    }
    def drawState(q:State[Sigma,L,E]):Unit = {
      write(s"""${qarr.indexOf(q)} [label="${q.id}"]\n""")
      for {
        (destination, transitions) <- q.transitions.groupBy(_.destination)
        labels = transitions.map(_.label)
        label = labels.reduce(dfa.labeler.combineLabels)
      } arrow(qarr.indexOf(q), qarr.indexOf(destination), labelMap(label))
    }
    write("digraph G {\n")
    // header
    write("  fontname=courier;\n")
    write("  rankdir=LR; graph[labeljust=l,nojustify=true]\n")
    write("  node [fontname=Arial, fontsize=25];\n")
    write("  edge [fontsize=20];\n")

    // initial state Q0
    write("// Initial state\n")
    write("""I0 [label="", style=invis, width=0]""")
    write("\n")
    write("I0 -> " + qarr.indexOf(dfa.q0).toString + ";\n")

    // final states F
    write(s"// ${dfa.F.size} final states\n")
    for{q <- dfa.F}{
      val i = qarr.indexOf(q)
      write(s"""F$i [label="", style=invis, width=0]""")
      write("\n")
      write(i.toString + s" -> F$i ")
      write(s"""[label="${dfa.exitValue(q).toString}"]""")
      write(";\n")
    }
    write(s"// all ${dfa.Q.size} states\n")
    dfa.Q.foreach{drawState}

    lazy val transitionLabelText:String = (for{(lab,i) <- labels.toSeq.zipWithIndex
                                               } yield s"\\lt$i= $lab").mkString("","","\\l")

    if(abbrev || title != ""){
      write( """  labelloc="t";""")
      write("label=\"")
      write(title)
      write(transitionLabelText)
      write("\"\n")
    }

    write("}\n")
  }
}
