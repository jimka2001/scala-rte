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

package dfa

object GraphViz {

  import java.io.{File, OutputStream}

  def dfaView[Sigma,L,E](dfa: Dfa[Sigma,L,E], title:String=""): String = {
    import sys.process._
    val png = dfaToPng(dfa,title)
    val cmd = Seq("open", png)
    cmd.!
    png
  }

  def dfaToPng[Sigma,L,E](dfa:Dfa[Sigma,L,E], title:String): String = {
    val png = File.createTempFile("dfa-", ".png")
    val pngPath = png.getAbsolutePath
    val dot = File.createTempFile("dfa-", ".dot")
    val dotPath = dot.getAbsolutePath
    val alt = File.createTempFile("dfa-", ".plain")
    val altPath = alt.getAbsolutePath
    dfaToPng(dfa,dotPath, title)

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

  def dfaToPng[Sigma,L,E](dfa:Dfa[Sigma,L,E],pathname: String, title:String): String = {
    val stream = new java.io.FileOutputStream(new java.io.File(pathname))
    dfaToDot(dfa,stream, title)
    stream.close()
    pathname
  }

  def dfaToDot[Sigma,L,E](dfa:Dfa[Sigma,L,E],stream: OutputStream, title:String): Unit = {
    val qarr=dfa.Q.toArray

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
      for{
        tr <- q.transitions
      } arrow(qarr.indexOf(tr.source),qarr.indexOf(tr.destination),tr.label.toString)
    }
    write("digraph G {\n")
    // header
    write("rankdir=LR; graph[labeljust=l,nojustify=true]\n")

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

    if (title != "" ) {
      write( """  labelloc="t";""")
      write("\n")
      write(s"""  label="$title";""")
      write("\n")
    }
    write("}\n")
  }
}
