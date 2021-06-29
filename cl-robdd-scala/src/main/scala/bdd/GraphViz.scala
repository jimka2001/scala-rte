// Copyright (c) 2019 EPITA Research and Development Laboratory
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

object GraphViz {

  import java.io.{File, OutputStream}

  def bddView(bdd: Bdd, drawFalseLeaf: Boolean, title:String="", labelToString:Int=>String): String = {
    val png = bddToPng(bdd,drawFalseLeaf,title,labelToString)
    openGraphicalFile(png)
  }

  val dotProgram:String = locally{
    import java.nio.file.{Paths, Files}
    Seq("/usr/local/bin/dot","/opt/local/bin/dot").find(p => Files.exists(Paths.get(p))) match {
      case None => "dot"
      case Some(path) => path
    }
  }

  def bddToPng(bdd:Bdd,drawFalseLeaf: Boolean, title:String, labelToString:Int=>String): String = {
    val png = File.createTempFile("bdd", ".png")
    val pngPath = png.getAbsolutePath
    val dot = File.createTempFile("bdd", ".dot")
    val dotPath = dot.getAbsolutePath
    bddToPng(bdd,dotPath, drawFalseLeaf, title,labelToString)

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

  def bddToPng(bdd:Bdd,pathname: String, drawFalseLeaf: Boolean, title:String, labelToString:Int=>String): String = {
    val stream = new java.io.FileOutputStream(new java.io.File(pathname))
    bddToDot(bdd,stream, drawFalseLeaf, title, labelToString)
    stream.close()
    pathname
  }

  def bddToDot(bdd:Bdd,stream: OutputStream, drawFalseLeaf: Boolean, title:String, labelToString:Int=>String): Unit = {
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
    val (_, names) = bdd.fold((1, Map[Bdd, Int]())) { case ((n: Int, names: Map[Bdd, Int]), bdd: Bdd) =>
      bdd match {
        case BddTrue => dotTrue(n)
        case BddFalse => dotFalse(n, drawFalseLeaf)
        case bdd: BddNode =>
          write(s""" $n [shape="ellipse",label="""")
          write(labelToString(bdd.label))
          write(s"""",penWidth=$penWidth]\n""")
      }
      (n + 1, names + (bdd -> n))
    }

    def drawConnection(parent: Bdd, child: Bdd, style: String, color: String,
                       arrowHead: Option[String], arrowTail: Option[String], dir: Option[String]): Unit = {
      if ((child != BddFalse) || drawFalseLeaf) {
        write(s" ${names(parent)} -> ${names(child)} [")
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

    for {(bdd, _) <- names} yield {
      bdd match {
        case bdd: BddNode =>
          drawConnection(bdd, bdd.positive, style = """"solid"""", color = """"green"""",
                         arrowHead = None, arrowTail = None, dir = None)
          drawConnection(bdd, bdd.negative, style = """"dashed"""", color = """"red"""",
                         arrowHead = Some("normal"), arrowTail = Some("odot"), dir = Some("both"))
        case _ => ()
      }
    }

    if (bdd == BddFalse) {
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
  implicit class GraphVizOps(val givenBdd: Bdd) extends AnyVal {
    def labelToString(label:Int):String = label.toString
    // this implicit class allows the syntax
    //   bdd.bddView(...),
    //   bdd.bddToPng(...), and
    //   bdd.bddToDot(...)
    def bddView(drawFalseLeaf: Boolean,
                title:String,
                labelToString:Int=>String=labelToString): Unit = {
      GraphViz.bddView(givenBdd, drawFalseLeaf,title,labelToString)
    }
    def bddToPng(drawFalseLeaf: Boolean,
                 title:String,
                 labelToString:Int=>String=labelToString): String = {
      GraphViz.bddToPng(givenBdd,drawFalseLeaf,title,labelToString)
    }
    def bddToDot(stream: OutputStream,
                 drawFalseLeaf: Boolean,
                 title:String,
                 labelToString:Int=>String=labelToString): Unit = {
      GraphViz.bddToDot(givenBdd,stream,drawFalseLeaf,title,labelToString)
    }
  }

  def main(argv: Array[String]): Unit = {
    val drawFalse = false
    Bdd.withNewBddHash {
      val bdd3 = Bdd(3)
      val bdd2 = Bdd(2)
      Bdd(1, bdd2, bdd3).bddToDot(System.out, drawFalseLeaf=drawFalse,"")
      Bdd(1, bdd2, bdd3).bddView(drawFalseLeaf=drawFalse,"")
      Or(1, 2, -3, And(-1, 4), And(2, Not(Or(1, 3)))).bddView(drawFalseLeaf=drawFalse,"")

      Not(Or(Xor(1, And(-2, -3)),
             AndNot(2, 3))).bddView(drawFalseLeaf=drawFalse,title="")
      Not(Or(-2,
             3,
             Xor(1, 2, And(-2, -3, 4)),
             AndNot(2, 3))).bddView(drawFalseLeaf=drawFalse,title="")

      Or(1, 2).bddToDot(System.out, drawFalseLeaf = true, title="")

    }
    BddTrue.bddView(drawFalse,title="")
  }
}
