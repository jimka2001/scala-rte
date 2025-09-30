package adjuvant

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
                toPng:(String,String,String)=>A):(A,String) = {
    import java.io.{File, OutputStream}
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

}
