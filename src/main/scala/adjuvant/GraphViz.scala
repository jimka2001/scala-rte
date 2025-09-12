package adjuvant

object GraphViz {
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

  // run dot and return the png path
  def runDot(title:String,
             label:Option[String],
             toPng:(String,String)=>Unit):String = {
    import java.io.{File, OutputStream}
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

}
