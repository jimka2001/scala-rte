// Copyright (c) 2020 EPITA Research and Development Laboratory
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

package lbdd

object GraphViz {

  import java.io.{File, OutputStream}

  def bddView(bdd: LBdd, drawFalseLeaf: Boolean, title:String=""): String = {
    import sys.process._
    val png = bddToPng(bdd,drawFalseLeaf,title)
    val cmd = Seq("open", png)
    cmd.!
    png
  }

  val dotProgram:String = locally{
    import java.nio.file.{Files, Paths}
    Seq("/usr/local/bin/dot").find(p => Files.exists(Paths.get(p))) match {
      case scala.None => "dot"
      case Some(path) => path
    }
  }

  def bddToPng(bdd: LBdd,drawFalseLeaf: Boolean, title:String): String = {
    val png = File.createTempFile("bdd", ".png")
    val pngPath = png.getAbsolutePath
    val dot = File.createTempFile("bdd", ".dot")
    val dotPath = dot.getAbsolutePath
    bddToPng(bdd,dotPath, drawFalseLeaf, title)

    locally {
      import sys.process._
      val cmd = Seq(dotProgram, "-Tpng", dotPath, "-o", pngPath)
      println(s"cmd = $cmd")
      cmd.!
    }
    //png.deleteOnExit()
    //dot.deleteOnExit()
    pngPath
  }

  def bddToPng(bdd: LBdd,pathname: String, drawFalseLeaf: Boolean, title:String): String = {
    val stream = new java.io.FileOutputStream(new java.io.File(pathname))
    bddToDot(bdd,stream, drawFalseLeaf, title)
    stream.close()
    pathname
  }

  def bddToDot(bdd: LBdd,stream: OutputStream, drawFalseLeaf: Boolean, title:String): Unit = {
    val penWidth = 2

    def write(str: String): Unit = {
      for {c <- str} {
        stream.write(c.toByte)
      }
    }

    def dotTrue(n: Int): Unit = {
      write(s""" $n [shape="box",label="T",fontname="sans-serif"]\n""")
    }

    def dotFalse(n: Int, draw: Boolean): Unit = {
      if (draw) write(s""" $n [shape="box",label="&perp;"]\n""")
    }

    write("digraph G {\n")
    val (_, names) = bdd.fold((1, Map[LBdd, Int]())) { case ((n: Int, names: Map[LBdd, Int]), bdd: LBdd) =>
      bdd match {
        case LBddTrue => dotTrue(n)
        case LBddFalse => dotFalse(n, drawFalseLeaf)
        case bdd: LBddNode => write(s""" $n [shape="ellipse",label="x${bdd.label}",penWidth=$penWidth]\n""")
      }
      (n + 1, names + (bdd -> n))
    }

    def drawConnection(parent: LBdd, child: LBdd, style: String, color: String,
                       arrowHead: Option[String], arrowTail: Option[String], dir: Option[String]): Unit = {
      if ((child != LBddFalse) || drawFalseLeaf) {

        write(s" ${names.getOrElse(parent, 0)} -> ${names.getOrElse(child, 0)} [")
        write(s"style=$style,color=$color,penwidth=$penWidth")
        for {a <- arrowHead} {
          write(s",arrowhead=$a")
        }
        for {a <- arrowTail} {
          write(s",arrowtail=$a")
        }
        for {d <- dir} {
          write(s",dir=$d")
        }
        write(s"]\n")
      }
    }

    for {(bdd, n) <- names} yield {
      bdd match {
        case bdd: LBddNode =>
          drawConnection(bdd, bdd.positive, style = """"solid"""", color = """"green"""", arrowHead = scala.None, arrowTail = scala.None, dir = scala.None)
          if (bdd.middle.nonEmpty)
            drawConnection(bdd, bdd.middle.get(), style = """"dashed"""", color = """"blue"""", arrowHead = scala.None, arrowTail = scala.None, dir = scala.None)
          drawConnection(bdd, bdd.negative, style = """"solid"""", color = """"red"""", arrowHead = scala.Some("normal"), arrowTail = scala.Some("odot"), dir = scala.Some("both"))
        case _ => ()
      }
    }

    if (bdd == LBddFalse) {
      // if drawFalseLeaf is false, we need to avoid drawing nothing, in this
      // case we draw the bot node even though drawFalseLeaf indicates otherwise.
      dotFalse(1, !drawFalseLeaf)
    }
    if (title != "" ) {
      write( """  labelloc="t";""")
      write("\n")
      write(s"""  label="$title";""")
      write("\n")
    }
    write("}\n")
  }

  // TYPE CLASS
  implicit class GraphVizOps(val givenBdd: LBdd) extends AnyVal {
    // this implicit class allows the syntax
    //   bdd.bddView(...),
    //   bdd.bddToPng(...), and
    //   bdd.bddToDot(...)
    def bddView(drawFalseLeaf: Boolean, title:String): Unit = {
      GraphViz.bddView(givenBdd, drawFalseLeaf,title)
    }
    def bddToPng(drawFalseLeaf: Boolean,title:String): String = {
      GraphViz.bddToPng(givenBdd,drawFalseLeaf,title)
    }
    def bddToDot(stream: OutputStream, drawFalseLeaf: Boolean, title:String): Unit = {
      GraphViz.bddToDot(givenBdd,stream,drawFalseLeaf,title)
    }
  }

  def main(argv: Array[String]): Unit = {
    val drawFalse = true
    val bdd3 = LBdd(3)
    val bdd2 = LBdd(2)
    LBdd(1).bddToDot(System.out, drawFalseLeaf=drawFalse,"")
    //LBdd(1, bdd2, bdd3).bddView(drawFalseLeaf=drawFalse,"")
    Or(1, 2, -3, And(-1, 4), And(2, Not(Or(1, 3)))).bddView(drawFalseLeaf=drawFalse,"")
    Or(1, 2).bddView(true, "Or(1, 2)")
    Or(1, 3).bddView(true, "Or(1, 3)")

    //Not(Or(Or(1, And(-2, -3)),
    //       AndNot(2, 3))).bddView(drawFalseLeaf=drawFalse,title="")
    //Not(Or(-2,
    //       3,
    //       Or(1, 2, And(-2, -3, 4)),
    //       AndNot(2, 3))).bddView(drawFalseLeaf=drawFalse,title="")
    //LBddTrue.bddView(drawFalse,title="")
  }
}
