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

package dimacs

import dimacs.QmVec.{CNF, ClauseAsList, canonicalizeClause}

object dimacsParse {

  import scala.io.Source

  //  Read a DIMACS CNF file, as described by https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
  //  The CNF file format is an ASCII file format.
  //
  //  Each clause encountered in the DIMACS file is passed to a call to
  //  the given CONSUME function.  The terms of the clause are in reverse
  //  order as listed explicitly in the file.
  //  E.g., if the clause in the file is listed as '1 2 -3 0', then consume is
  //  called with (-3 2 1) as argument.  The default consume function reverses
  //  these back into the order specified in the DIMACS file, and accumulates
  //  the list of clauses in the same order as in the file.
  //
  //  The file may begin with comment lines. The first character of each
  //  comment line must be a lower case letter "c". Comment lines typically
  //  occur in one section at the beginning of the file, but are allowed to
  //  appear throughout the file.
  //
  //  The comment lines are followed by the "problem" line. This begins
  //  with a lower case "p" followed by a space, followed by the problem
  //  type, which for CNF files is "cnf", followed by the number of
  //  variables followed by the number of clauses.
  //
  //  The remainder of the file contains lines defining the clauses, one by
  //  one.
  //
  //  A clause is defined by listing the index of each positive literal,
  //  and the negative index of each negative literal. Indices are 1-based,
  //  and for obvious reasons the index 0 is not allowed.
  //
  //  The definition of a clause may extend beyond a single line of text.
  //
  //  The definition of a clause is terminated by a final value of "0".
  //
  //  The file terminates after the last clause is defined.
  //
  //  Some odd facts include:
  //
  //  The definition of the next clause normally begins on a new line, but
  //  may follow, on the same line, the "0" that marks the end of the
  //  previous clause.
  //
  //  In some examples of CNF files, the definition of the last clause is
  //  not terminated by a final '0';
  //
  //  In some examples of CNF files, the rule that the variables are
  //  numbered from 1 to N is not followed. The file might declare that
  //  there are 10 variables, for instance, but allow them to be numbered 2
  //  through 11.

  def parseFileByName(fileName: String): DimacsFile = {
    parseBufferedContent(Source.fromFile(fileName).buffered)
  }

  def consumeFileByName(fileName: String, consumeClause: ClauseAsList => Unit, consumeProblem: Problem => Unit): Unit = {
    consumeBufferedContent(Source.fromFile(fileName).buffered, consumeClause, consumeProblem)
  }

  def parseStringContent(content: String): DimacsFile = {
    parseBufferedContent(content.toIterator.buffered)
  }

  private val digits = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  private val numeric = digits + '-'
  private val endOfLine = Set('\r', '\n')
  private val whiteSpace = Set(' ', '\t') ++ endOfLine

  def dropWhile(it: BufferedIterator[Char], p: Char => Boolean): BufferedIterator[Char] = {
    while (it.hasNext && p(it.head)) {
      it.next()
    }
    it
  }

  def takeWhile(it: BufferedIterator[Char], p: Char => Boolean): List[Char] = {
    def loop(buf: List[Char]): List[Char] = {
      if (it.hasNext && p(it.head))
        loop(it.next() :: buf)
      else
        buf.reverse
    }

    loop(Nil)
  }

  def parseProblem(it: BufferedIterator[Char]): Problem = {
    assert(it.next() == 'p')
    assert(it.next() == ' ') // space is required after p in problem line
    dropWhile(it, whiteSpace.contains)
    val problemType: String = takeWhile(it, !whiteSpace.contains(_)).mkString
    dropWhile(it, whiteSpace.contains)
    assert(digits.contains(it.head))
    val numVars: Int = takeWhile(it, digits.contains).mkString.toInt
    dropWhile(it, whiteSpace.contains)
    assert(digits.contains(it.head))
    val numClauses: Int = takeWhile(it, digits.contains).mkString.toInt
    Problem(problemType, numVars, numClauses)
  }

  def skipThroughEOL(it: BufferedIterator[Char]): Unit = {
    dropWhile(it, !endOfLine.contains(_))

  }

  def skipComment(it: BufferedIterator[Char]): Unit = {
    assert(it.next() == 'c')
    skipThroughEOL(it)
    dropWhile(it, whiteSpace.contains)
    Unit
  }

  def consumeBufferedContent(it: BufferedIterator[Char], consumeClause: ClauseAsList => Unit, consumeProblem: Problem => Unit): Unit = {
    def recur(partialClause: ClauseAsList): Unit = {

      if (it.hasNext)
        it.head match {
          case 'p' => {
            consumeProblem(parseProblem(it))
            recur(partialClause)
          }
          case 'c' =>
            skipComment(it)
            recur(partialClause)
          case 'a' |
               'e' |
               's' =>
            skipThroughEOL(it)
            recur(partialClause)
          case c if numeric.contains(c) =>
            val num = takeWhile(it, numeric.contains)
            val number = num.mkString.toInt
            if (number == 0) {
              consumeClause(partialClause)
              recur(Nil)
            }
            else
              recur(number :: partialClause)
          case c if whiteSpace.contains(c) =>
            dropWhile(it, whiteSpace.contains)
            recur(partialClause)
          case c =>
            sys.error(s"no case for character [$c]")
        }
      else if (partialClause.nonEmpty)
        consumeClause(partialClause)
      else
        Unit
    }

    recur(Nil)
  }

  def parseBufferedContent(it: BufferedIterator[Char]): DimacsFile = {
    var clauses: CNF = Nil
    var problem: Problem = Problem.nil

    def consumeClause(clause: ClauseAsList): Unit = {
      clauses = clause :: clauses
    }

    def consumeProblem(p: Problem): Unit = {
      problem = p
    }

    consumeBufferedContent(it, consumeClause, consumeProblem)

    DimacsFile(problem, clauses)
  }

  import java.io.File

  def getListOfFiles(dir: File, extensions: List[String]): List[File] = {
    // copied from https://alvinalexander.com/scala/how-to-list-files-in-directory-filter-names-scala
    dir.listFiles.filter(_.isFile).toList.filter { file =>
      extensions.exists(file.getName.endsWith(_))
    }
  }

  def dimacsConvertDirectory(suffix: String, inputDirectory: String, getOutputName: String => String, fileNames: List[String]): Unit = {
    fileNames.foreach { fName =>
      dimacsConvertFile(fName,
                        fName => s"$inputDirectory/$fName",
                        getOutputName)
    }
  }

  def dimacsConvertDirectory(suffix: String, inputDirectory: String, getOutputName: String => String): Unit = {
    import java.nio.file.{Files, Paths}
    require(Files.exists(Paths.get(inputDirectory)), s"path '$inputDirectory' does not exist")
    val fileNames = getListOfFiles(new File(inputDirectory), List(suffix))
    dimacsConvertDirectory(suffix, inputDirectory, getOutputName, fileNames.map(_.getName))
    //    fileNames.foreach { fName =>
    //      dimacsConvertFile(fName.getName,
    //                        fName => s"$inputDirectory/$fName",
    //                        getOutputName)
    //    }
  }

  def dimacsConvertFile(inFileName: String, outFileName: String): Unit = {
    import java.nio.file.{Files, Paths}

    assert(Files.exists(Paths.get(inFileName)), s"file does not exist: [$inFileName]")

    var diagnostics: List[String] = Nil
    val vec = new QmVec

    def logDiagnostic(comment: String): Unit = {
      println(comment)
      diagnostics = comment :: diagnostics
    }

    var numVars1: Int = 0
    var numClauses1: Int = 0

    val t0 = System.nanoTime()
    consumeFileByName(inFileName,
                      clause => vec.addClause1(canonicalizeClause(clause)),
                      problem => {
                        numVars1 = problem.numVars;
                        numClauses1 = problem.numClauses
                      })

    logDiagnostic(s"   old numVars=$numVars1 numClauses=$numClauses1")

    val t1 = System.nanoTime()
    logDiagnostic(s"   parse & compile time ${(t1 - t0) / 1.0e9}")

    vec.qmReduce()
    val t2 = System.nanoTime()
    logDiagnostic(s"   reduce time= ${(t2 - t1) / 1.0e9}")
    val (numVars2, numClauses2) = vec.writeDimacs(outFileName, Some(diagnostics.reverse))
    logDiagnostic(s"   new numVars=$numVars2 numClauses=$numClauses2")
    logDiagnostic(s"   reduced by numVars=" + (1.0 - numVars2.toFloat / numVars1) +
                    " numClauses=" + (1.0 - numClauses2.toFloat / numClauses1))
    val t3 = System.nanoTime()
    println(s"   write time= ${(t3 - t2) / 1.0e9}")

  }

  def dimacsConvertFile(fName: String, getInputName: String => String, getOutputName: String => String): Unit = {
    println(s"parsing $fName")
    val inFileName: String = getInputName(fName)
    val outFileName: String = getOutputName(fName)
    dimacsConvertFile(inFileName, outFileName)
  }

  def main(argv: Array[String]): Unit = {
    import java.nio.file._
    val inDir = if (argv.isEmpty)
      "/Users/jnewton/Downloads/SAT-benchmarks/NoLimits" // default in case no command line argument is given
    else
      argv(0)
    val outDir = if (argv.length < 2)
      "/tmp/NoLimits"
    else
      argv(1)

    Files.createDirectories(Paths.get(outDir)) // create any necessary directories

    if (argv.length <= 2)
      dimacsConvertDirectory("cnf", inDir, { f: String => s"$outDir/$f" })
    else
      (2 until argv.length).foreach { i =>
        dimacsConvertDirectory("cnf", inDir, { f: String => s"$outDir/$f" }, List(argv(i)))
      }
  }
}
