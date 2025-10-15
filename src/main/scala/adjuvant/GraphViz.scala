package adjuvant


import java.io.{File, OutputStream}

object GraphViz {
  def multiLineString(str:String,sep:String="\\l",maxLine:Int=60):String = {
    if (str.size <= maxLine)
      str
    else {
      // start at column maxLine and count backwards for a delimiter
      val delimeters = Seq(',',')')
      val tab = "    "
      (maxLine to 1 by -1).find(i => delimeters.contains(str(i))) match {
        case Some(k) =>
          val suffix = multiLineString(str.drop(k+1),sep,maxLine)
          if (suffix == "")
            str
          else
            str.take(k + 1) + sep + tab + suffix
        case None =>
          str
      }
    }
  }

  // run dot and return the png path
  def runDot[A](title:String,
                prefix:String,
                // toPng is a function which will write to a named dot file
                toPng:(String,String,String)=>A):(A,String) = {
    val png = File.createTempFile(prefix+"-"+title+"-", ".png")
    val pngPath = png.getAbsolutePath
    val dot = File.createTempFile(prefix+"-"+title+"-", ".dot")
    val dotPath = dot.getAbsolutePath
    val alt = File.createTempFile(prefix+"-"+title+"-", ".plain")
    val altPath = alt.getAbsolutePath
    val latex = File.createTempFile(prefix+"-"+title+"-", ".tex")
    val latexPath = latex.getAbsolutePath

    val a:A = toPng(dotPath, latexPath, title)
    locally {
      import sys.process._
      Seq("dot", "-Tplain", dotPath, "-o", altPath).! // write file containing coordinates

      val cmd = Seq("dot", "-Tpng", dotPath, "-o", pngPath)
      //println(s"cmd = $cmd")
      cmd.!
    }
    //png.deleteOnExit()
    //dot.deleteOnExit()
    (a, pngPath)
  }

  def treeToPng(edges:Seq[(Int,Int)],
                labels:Map[Int,String],
                baseName:String,
                dotFileCB:String=>Unit = (_:String) => (),
                title: String = "",
                view:Boolean = true):Unit = {
    import adjuvant.Adjuvant.openGraphicalFile

    def writeToDot(dotPathName:String, _latexName:String, title:String):Unit = {
      val dotStream = new java.io.FileOutputStream(new File(dotPathName))
      treeToDot(edges, labels, dotStream, title=title, drawUnlabeled=false)
      dotStream.close()
      dotFileCB(dotPathName)
    }
    val (_, pngPath) = runDot(title, baseName, writeToDot)
    if (view)
      openGraphicalFile(pngPath)
  }

  def treeToDot(edges:Seq[(Int,Int)],
                labels:Map[Int,String],
                dotStream: OutputStream,
                drawUnlabeled:Boolean = false,
                title: String = ""):Unit = {

    val vertices:Set[Int] = edges.foldLeft(Set[Int]()){
      case (acc:Set[Int], (src:Int, dst:Int)) => acc + src + dst}

    def write(str: String): Unit = {
      for {c <- str} {
        dotStream.write(c.toByte)
      }
    }

    write("digraph G {\n")
    // header
    write("  fontname=courier;\n")
    write("  rankdir=TB; graph[labeljust=l,nojustify=true]\n")
    write("  node [fontname=Arial, fontsize=25];\n")
    write("  edge [fontsize=20];\n")

    for { v <- vertices.to(List).sorted
          } {
      if (drawUnlabeled)
        write(s"N$v")
      else if (labels.contains(v)) {
        labels.get(v) match {
          case None => if (drawUnlabeled) write(s""" [label=""]""")
          case Some(lab) => write(s"""N$v [label="${lab}"]""")
        }
      }

      write("\n")
    }

    for { (src,dst) <- edges
      if labels.contains(src)
      if labels.contains(dst)
          } write(s"N$src -> N$dst\n")


    if ( title != "") {
      write("""  labelloc="t";""")
      write("\n  label=")
      write(s""""${title}"""")
      write("\n")
    }

    write("}\n")
  }

  def main(argv:Array[String]):Unit = {
    // simple test
    treeToPng(edges=Seq((0, 1), (0, 2),
      (2, 3), (2, 4)),
      labels=Map(0 -> "0",
        2 -> "2"),
      baseName="tree",
      title="sample-tree",
      view=true)

  }

}
