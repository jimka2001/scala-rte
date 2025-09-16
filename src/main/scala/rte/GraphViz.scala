package rte

import adjuvant.Adjuvant.openGraphicalFile
import genus.{SEmpty, STop, SimpleTypeD}
import adjuvant.GraphViz.{multiLineString, runDot}

object GraphViz {
  import java.io.{File, OutputStream}
  val fDummy = (_:String) => ()
  val defaultTypeVector = Vector(SEmpty, STop)

  def rteView(rte: Rte,
              title: String = "",
              givenTypes: Vector[SimpleTypeD] = defaultTypeVector,
              dotFileCB: String => Unit = fDummy,
              habitation:Boolean = true): (Vector[SimpleTypeD], String) = {

    val (labels,png) = rteToPng(rte, title=title,
                                givenTypes=givenTypes,
                                dotFileCB = dotFileCB,
                                habitation=habitation)
    openGraphicalFile(png)
    (labels,png)
  }

  def rteToPng(rte: Rte,
               title: String,
               label:Option[String]=None,
               givenTypes:Vector[SimpleTypeD] = defaultTypeVector,
               dotFileCB: String => Unit = fDummy,
               habitation: Boolean = true):(Vector[SimpleTypeD], String) = {
    def toPng(pathname: String,
              title: String): Vector[SimpleTypeD] = {
      val stream = new java.io.FileOutputStream(new java.io.File(pathname))
      val mergedTypes = rteToDot(rte, stream, title, givenTypes=givenTypes, habitation=habitation)
      stream.close()
      dotFileCB(pathname)
      mergedTypes
    }

    runDot(title, label, toPng)
  }



  // returns a vector of type designators (each of tyep SimpleTypeD).
  //    this return value is useful in case the caller wants to make
  //    another call to rteToDot making sure the same t0, t1, t2 assignment
  //    is made to each type.  This helps to compare two different
  //    images, so the same type is notated the same way.
  // `habitation` controls whether the AST is colorized into 4 cases
  //    empty/vacuous = red
  //    universe = blue (equiv to Sigma *)
  //    inhabited = green
  //    dont-know = orange
  def rteToDot(rte:Rte,
               stream: OutputStream,
               title: String,
               abbrev:Boolean = true,
               givenTypes: Vector[SimpleTypeD] = defaultTypeVector,
               habitation: Boolean = true):Vector[SimpleTypeD] = {

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
          } {
      write(s"""R${idx} [label="${label}"""")
      if (habitation) {
        write(", style=filled, fillcolor=")
        if (Not(r).toDfa(true).inhabited() == Some(false))
          write("skyblue, shape=egg, orientation=180")
        else
          r.toDfa(true).inhabited() match {
            // maybe
            case None => write("orange, shape=Mcircle")
            // inhabited
            case Some(true) => write("lightgreen, shape=rect")
            // empty
            case Some(false) => write("lightcoral, shape=egg")
          }
      }
      write(s"""]\n""")
    }
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