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

  def multiLineString(str:String,maxLine:Int=60):String = {
    if (str.size <= maxLine)
      str
    else {
     // start at column maxLine and count backwards for a delimiter
      val delimeters = Seq(',',')')
      val tab = "    "
      (maxLine to 1 by -1).find(i => delimeters.contains(str(i))) match {
        case Some(k) =>
          val suffix = multiLineString(str.drop(k+1),maxLine)
          if (suffix == "")
            str
          else
            str.take(k + 1) + "\\l" + tab + suffix
        case None =>
          str
      }
    }
  }

  def dfaView[Sigma,L,E](dfa: Dfa[Sigma,L,E],
                         title:String="",
                         abbrev:Boolean=false,
                         label:Option[String]=None,
                         showSink:Boolean=true,
                         dotFileCB:String=>Unit=(_=>()),
                         givenLabels:Seq[L]=Seq(),
                         printLatex:Boolean=false): String = {
    val extendedLabel = (label,dfa.labeler.graphicalText()) match {
      case (_, Seq()) => label
      case (None, strings) => Some(strings.mkString("\\l"))
      case (Some(str),strings) => Some(str + "\\l" + strings.mkString("\\l"))
    }
    val png = dfaToPng(dfa, title, abbrev=abbrev, label=extendedLabel,
                       showSink=showSink, dotFileCB=dotFileCB,
                       givenLabels=givenLabels,
                       printLatex=printLatex)
    openGraphicalFile(png)
  }

  def dfaToPng[Sigma,L,E](dfa:Dfa[Sigma,L,E],
                          title:String,
                          abbrev:Boolean=true,
                          label:Option[String]=None,
                          showSink:Boolean=true,
                          dotFileCB:String=>Unit= _=>(),
                          givenLabels:Seq[L]=Seq(),
                          printLatex:Boolean=false): String = {
    def toPng(pathname: String,
              title: String): String = {
      val stream = new java.io.FileOutputStream(new java.io.File(pathname))
      dfaToDot(dfa, stream, title, abbrev = abbrev,
               showSink = showSink, givenLabels=givenLabels,
               printLatex=printLatex)
      stream.close()
      dotFileCB(pathname)
      pathname
    }
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
    toPng(dotPath, longTitle)
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

  def dfaToDot[Sigma,L,E](dfa:Dfa[Sigma,L,E],
                          stream: OutputStream,
                          title:String,
                          abbrev:Boolean,
                          showSink:Boolean,
                          givenLabels:Seq[L],
                          printLatex:Boolean=false): Unit = {
    val qarr = dfa.Q.toArray
    val sinkStateIds = dfa.findSinkStateIds().filter(id => id != dfa.q0.id)
    val usedLabels: Set[L] = for {q <- dfa.Q
                                  if showSink || !sinkStateIds.contains(q.id)
                                  (dst, transitions) <- q.transitions.groupBy(_.destination)
                                  if showSink || !sinkStateIds.contains(dst.id)
                                  labels = transitions.map(_.label)
                                  label = labels.reduce(dfa.labeler.combineLabels)
                                  } yield label
    // try to put SEmpty and STop at t0 and t1,
    //   but respect givenLabels (which might be empty).
    //   All the labels in givenLabels go at the beginning of the
    //   list of orderedLabels.
    val orderedLabels: Seq[L] = locally {
      val newLabels = usedLabels.toSeq diff givenLabels
      val empty = newLabels.filter { lab => dfa.labeler.inhabited(lab).contains(false) }
      val univ = (newLabels diff empty).filter { lab => dfa.labeler.universal(lab) }
      val others = (newLabels diff empty diff univ)
      givenLabels ++ empty ++ univ ++ others
    }

    val labelMap: Map[L, (Int, String)] = orderedLabels.zipWithIndex.map {
      case (lab, i: Int) =>
        if (abbrev)
          lab -> (i, s"t$i")
        else
          lab -> (i, dfa.labeler.toDot(lab))
    }.toMap

    def write(str: String): Unit = {
      for {c <- str} {
        stream.write(c.toByte)
      }
    }

    def arrow(from: Int, to: Int, label: String): Unit = {
      write(s"$from -> $to")
      if (label != "")
        write(s""" [label="$label"]""")
      write("\n")
    }

    def drawState(q: State[Sigma, L, E]): Unit = {
      if (dfa.F.contains(q))
        write(s"""${qarr.indexOf(q)} [shape=doublecircle, label="${q.id}"]\n""")
      else
        write(s"""${qarr.indexOf(q)} [label="${q.id}"]\n""")
      for {
        (destination, transitions) <- q.transitions.groupBy(_.destination)
        if showSink || !sinkStateIds.contains(destination.id)
        labels = transitions.map(_.label)
        label = labels.reduce(dfa.labeler.combineLabels)
      } arrow(qarr.indexOf(q), qarr.indexOf(destination), labelMap(label)._2)
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
    for {q <- dfa.F} {
      val i = qarr.indexOf(q)
      write(s"""F$i [label="", style=invis, width=0]""")
      write("\n")
      write(i.toString + s" -> F$i ")
      write(s"""[label="${dfa.exitValue(q).toString}"]""")
      write(";\n")
    }
    write(s"// all ${dfa.Q.size} states\n")
    dfa.Q.foreach { q =>
      if (showSink || !sinkStateIds.contains(q.id))
        drawState(q)
    }

    lazy val transitionLabelText: String = (for {(lab, (i, t)) <- labelMap.toSeq.sortBy(_._2._1)
                                                 if usedLabels.contains(lab)
                                                 _ = if (printLatex) println(s"t$i => " + dfa.labeler.toLaTeX(lab))
                                                 multiLab = multiLineString(dfa.labeler.toDot(lab))
                                                 } yield s"\\l$i= $multiLab").mkString("", "", "\\l")

    if (abbrev || title != "") {
      write("""  labelloc="t";""")
      write("\n  label=\"")
      write(title)
      if (abbrev)
        write(transitionLabelText)
      write("\"\n")
    }

    write("}\n")
  }
}
