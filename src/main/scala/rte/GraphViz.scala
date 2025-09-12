package rte

import adjuvant.Adjuvant.openGraphicalFile
import genus.{SEmpty, STop, SimpleTypeD}
import adjuvant.GraphViz.{multiLineString, runDot}

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

    runDot(title, label, toPng)
  }

  val defaultTypeVector = Vector(SEmpty, STop)
  def rteToDot(rte:Rte,
               stream: OutputStream,
               title: String,
               abbrev:Boolean = true,
               givenTypes: Vector[SimpleTypeD] = defaultTypeVector):Vector[SimpleTypeD] = {
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

    // collect all type designators used in as operand of Singleton in this Rte.
    // some of these might already be in givenTypes
    val usedTypes = linear.collect{
      case (Singleton(td), _)  => td
    }.distinct

    // collect all the type designators not already in givenTypes
    val mergedTypes = givenTypes ++ (usedTypes.filter{td => ! givenTypes.contains(td)})

    val tds = mergedTypes.zipWithIndex.to(Map)
    def nodeLabel(rte:Rte):String = {
      rte match {
        case Star(_) => "*"
        case EmptySeq => "()"
        case Sigma => "&#931;" // Σ
        case Singleton(td) =>
          val idx = tds(td)
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

    val typesLabels = for{(td,idx) <- mergedTypes.zipWithIndex
                         if usedTypes.contains(td)
                         lab = multiLineString(td.toDot()).replaceAll("\"","\\\"")
                         }  yield s"t${idx} = $lab"
    val typesLabel = typesLabels.mkString("", "\\l", "")

    if ( title != "") {
      write("""  labelloc="t";""")
      write("\n  label=\"")
      write(title)
      if(abbrev)
        write(s"\\l${typesLabel}\\l")
      write("\"\n")
    }

    write("}\n")

    mergedTypes
  }
}