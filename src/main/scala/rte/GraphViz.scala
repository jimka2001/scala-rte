package rte

import adjuvant.Adjuvant.openGraphicalFile


object GraphViz {
  import java.io.{File, OutputStream}
  val fDummy = (_:String) => ()

  def rteView(rte: Rte,
              title: String = "",
              dotFileCB: String => Unit = fDummy) = {

    val png = rteToPng(rte, title=title, dotFileCB = dotFileCB)
    openGraphicalFile(png)
  }

  def rteToPng(rte: Rte,
               title: String,
               label:Option[String]=None,
               dotFileCB: String => Unit = fDummy):String = {
    def toPng(pathname: String,
              title: String): String = {
      val stream = new java.io.FileOutputStream(new java.io.File(pathname))
      rteToDot(rte, stream, title)
      stream.close()
      dotFileCB(pathname)
      pathname
    }

    val prefix = if (title == "")
      "rte"
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

  def rteToDot(rte:Rte,
               stream: OutputStream,
               title: String):Unit = {
    def write(str: String): Unit = {
      for {c <- str} {
        stream.write(c.toByte)
      }
    }

    write("digraph G {\n")
    // header
    write("  fontname=courier;\n")
    write("  rankdir=TB; graph[labeljust=l,nojustify=true]\n")
    write("  node [fontname=Arial, fontsize=25];\n")
    write("  edge [fontsize=20];\n")


    val linear = rte.linearize().zipWithIndex
    val nodemap = linear.to(Map)

    // find nodes which are not pointed to by anything, ie, parents which
    // are never children

    val missingnodes = linear.filter{case (p,pidx) => linear.exists{case (c,cidx) => c==p}}
    println(s"missing = $missingnodes")
    val missingidxs = linear.filter{case (p,pidx) => linear.exists{case (c,cidx) => cidx==pidx}}
    println(s"missing = $missingidxs")


    val tds = linear.collect{
      case (Singleton(td), _) => td
    }.zipWithIndex.to(Map)

    def nodeLabel(rte:Rte):String = {
      rte match {
        case Star(_) => "*"
        case EmptySeq => "()"
        case Sigma => "&#931;"
        case Singleton(td) =>
          val idx = tds(td)
          // TODO need to add this type to the dot label
          println(f"type[$idx] = $td")
          s"t$idx"
        case _ => rte.getClass.getSimpleName
      }
    }
    for { (r,idx) <- linear
          label = nodeLabel(r)
          } write(s"""R${idx} [label="${label}"]\n""")
    for { (parent,idx) <- linear
          child <- parent.children()
          child_index = nodemap(child)
          } write(f"R${idx} -> R${child_index}\n")


    if ( title != "") {
      write("""  labelloc="t";""")
      write("\n  label=\"")
      write(title)
      write("\"\n")
    }

    write("}\n")
  }
}