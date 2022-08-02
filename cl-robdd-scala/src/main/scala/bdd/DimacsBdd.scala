// Copyright (c) 2019,2022 EPITA Research and Development Laboratory
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


object DimacsBdd {
  import dimacs.QmVec.ClauseAsList

  // This imports the TreeReducible instances.
  import treereduce.TreeReducible._
  // This imports the obj.treeMapReduce() syntax.
  import treereduce.TreeReduce._

  def clauseToBdd(clause: ClauseAsList, ident: BddTerm, op: BinaryOperation): Bdd = {
    clause.treeMapReduce(ident: Bdd)(Bdd(_), (acc: Bdd, b: Bdd) => op(acc, b))
  }

  def cnfClauseToBdd(cnfClause: ClauseAsList): Bdd = clauseToBdd(cnfClause, BddFalse, Or)

  def dnfClauseToBdd(dnfClause: ClauseAsList): Bdd = clauseToBdd(dnfClause, BddTrue, And)

  // Interpret a list of Clauses as an And of Ors where each clause is a disjunction
  def cnfToBdd(cnf: IterableOnce[ClauseAsList]): Bdd = {
    cnf.treeMapReduce(BddTrue:Bdd)(cnfClauseToBdd, { (acc: Bdd, b: Bdd) => And(acc, b) })
  }

  def dnfToBdd(dnf: IterableOnce[ClauseAsList]): Bdd = {
    dnfToBdd2(dnf, ()=>())
  }
  // Interpret a list of Clauses as an Or of Ands where each clause is a conjunction
  def dnfToBdd2(dnf: IterableOnce[ClauseAsList], thunk: ()=>Unit): Bdd = {
    dnf.treeMapReduce(BddFalse:Bdd)(dnfClauseToBdd, { (acc: Bdd, b: Bdd) =>
      thunk()
      Or(acc, b)
    })
  }
}

object DimacsParallelReduce {
  import dimacs.QmVec.ClauseAsList

  // This imports the obj.treeMapReduce() syntax.
  import treereduce.TreeParallelReduce._

  def clauseToBdd(clause: ClauseAsList, ident: BddTerm, op: BinaryOperation): Bdd = {
    pairMapReduce(clause)(ident: Bdd,Bdd(_), (acc: Bdd, b: Bdd) => op(acc, b))
  }
  def dnfClauseToBdd(dnfClause: ClauseAsList): Bdd = clauseToBdd(dnfClause, BddTrue, And)

  // Interpret a list of Clauses as an Or of Ands where each clause is a conjunction
  def dnfToBdd(dnf: List[ClauseAsList]): Bdd = {
    pairMapReduce(dnf)(BddFalse:Bdd,dnfClauseToBdd, { (acc: Bdd, b: Bdd) => Or(acc, b) })
  }
}


object DimacsMixedReduce {
  import dimacs.QmVec.ClauseAsList

  // This imports the TreeReducible instances.
  import treereduce.TreeReducible._
  // This imports the obj.treeMapReduce() syntax.
  import treereduce.TreeReduce._

  def clauseToBdd(clause: ClauseAsList, ident: BddTerm, op: BinaryOperation): Bdd = {
    clause.treeMapReduce(ident: Bdd)(Bdd(_), (acc: Bdd, b: Bdd) => op(acc, b))
  }
  def dnfClauseToBdd(dnfClause: ClauseAsList): Bdd = clauseToBdd(dnfClause, BddTrue, And)

  // Interpret a list of Clauses as an Or of Ands where each clause is a conjunction
  def dnfToBdd(dnf: IterableOnce[ClauseAsList]): Bdd = {
    def x(acc:Bdd,next:ClauseAsList):Bdd = Or(acc,dnfClauseToBdd(next))
    dnf.iterator.foldLeft(BddFalse:Bdd)(x)
  }
}

object DimacsFold {
  import dimacs.QmVec.ClauseAsList

  def clauseToBdd(clause: ClauseAsList, ident: BddTerm, op: BinaryOperation): Bdd = {
    def x(b:Bdd,i:Int):Bdd = op(b,i)
    clause.foldLeft(ident: Bdd)( x)
  }
  def dnfClauseToBdd(dnfClause: ClauseAsList): Bdd = clauseToBdd(dnfClause, BddTrue, And)

  def dnfToBdd(dnf: IterableOnce[ClauseAsList]): Bdd = {
    dnfToBdd2(dnf,()=>())
  }
  // Interpret a list of Clauses as an Or of Ands where each clause is a conjunction
  def dnfToBdd2(dnf: IterableOnce[ClauseAsList],thunk:()=>Unit): Bdd = {
    def x(acc:Bdd,next:ClauseAsList):Bdd = {
      thunk()
      Or(acc,dnfClauseToBdd(next))
    }
    dnf.iterator.foldLeft(BddFalse:Bdd)(x)
  }
}
