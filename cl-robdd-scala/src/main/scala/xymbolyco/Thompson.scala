package xymbolyco

import adjuvant.Accumulators.{makeCounter, withSetCollector}
import adjuvant.Adjuvant.{fixedPoint, traceGraph}
import genus.Types.mdtd
import genus.{SAnd, SEmpty, SNot, SOr, STop, SimpleTypeD}
import rte.And.createAnd
import rte.Cat.createCat
import rte.{And, Cat, EmptySet, EmptyWord, Not, Or, Rte, Sigma, Singleton, Star}
import rte.Or.createOr

import scala.annotation.tailrec

object Thompson {
  private val count = makeCounter(1)
  type TRANSITION = (Int, Either[EmptyWord.type, SimpleTypeD], Int)
  type TRANSITIONS = (Int, Int, Seq[TRANSITION])

  @tailrec
  def makeNewState(allStates:Set[Int]):Int = {
    val q = count()
    if (allStates.contains(q))
      makeNewState(allStates)
    else
      q
  }

  // makeTransition is useful for debugging
  def makeTTransition(in:Int, td:SimpleTypeD, out:Int):TRANSITION = {
    (in,Right(td),out)
  }

  // makeTransition is useful for debugging
  def makeETransition(in:Int, out:Int): TRANSITION = {
    (in,Left(EmptyWord),out)
  }

  def constructEpsilonFreeTransitions(rte: Rte): (Int,Seq[Int],Seq[(Int,SimpleTypeD,Int)]) = {
    val (in:Int, out:Int, transitions:Seq[TRANSITION]) = constructTransitions(rte)
    removeEpsilonTransitions(in,out,transitions)
  }

  def constructDeterminizedTransitions(rte:Rte):(Int,Seq[Int],Seq[(Int,SimpleTypeD,Int)]) = {
    val (in2,outs2,clean) = constructEpsilonFreeTransitions(rte)
    val completed = complete(in2, clean)
    determinize(in2,outs2,completed)
  }

  // start with a given vertex of a graph (yet to be determined).
  // We discover all the vertices by calling the edges function on each vertex
  //   yet discovered, starting with the given initial vertex.
  //   Once all the vertices have been discovered, filter them with the isAccepting predicate
  //      to determine the final/accepting states.
  //  Return a pair (sequence-of-final-states,
  //                 sequence-of-transitions of the form (vertex,label,vertex)
  def traceTransitionGraph[V](q0:V,
                              edges: V=>Seq[(SimpleTypeD,V)],
                              isFinal:V=>Boolean):(Seq[V],Seq[(V,SimpleTypeD,V)]) = {
    val (qs,transitions) = traceGraph[V,SimpleTypeD](q0,edges)
    (qs.filter(isFinal),
      for{(x,pairs) <- qs.zip(transitions)
          (label, y) <- pairs
          } yield (x, label, qs(y)))
  }

  def constructTransitions(rte: Rte): TRANSITIONS = {
    lazy val in = count() // TODO this is wrong, need to assure new state
    lazy val out = count() // TODO this is wrong, need to assure new state
    rte match {
      case EmptyWord =>
        (in, out, Seq(makeETransition(in,out)))
      case EmptySet =>
        (in, out, Seq())
      case Sigma =>
        (in, out, Seq(makeTTransition(in,STop,out)))
      case Singleton(td) =>
        (in, out, Seq(makeTTransition(in, td, out)))
      case Star(operand) =>
        val (inInner, outInner, transitions) = constructTransitions(operand)
        (in, out, transitions ++
                  Seq(makeETransition(in, inInner),
                      makeETransition(outInner, out),
                      makeETransition(in, out),
                      makeETransition(outInner, inInner)))
      case Or(operands) => constructTransitionsOr(operands)
      case Cat(operands) => constructTransitionsCat(operands)
      case Not(operand) => constructTransitionsNot(operand)
      case And(operands) => constructTransitionsAnd(operands)
    }
  }

  @tailrec
  def constructTransitionsOr(rtes: Seq[Rte]): TRANSITIONS = {
    rtes match {
      case Seq() => constructTransitions(EmptySet)
      case Seq(rte) => constructTransitions(rte)
      case Seq(rte1, rte2) =>
        val in = count()
        val out = count()
        val (or1In, or1Out, transitions1) = constructTransitions(rte1)
        val (or2In, or2Out, transitions2) = constructTransitions(rte2)
        (in, out, transitions1 ++
                  transitions2 ++
                  Seq(makeETransition(in, or1In),
                      makeETransition(in, or2In),
                      makeETransition(or1Out, out),
                      makeETransition(or2Out, out)))
      case _ =>
        constructTransitionsOr(Seq(rtes.head,
                                   createOr(rtes.tail)))
    }
  }

  @tailrec
  def constructTransitionsCat(rtes: Seq[Rte]): TRANSITIONS = {
    rtes match {
      case Seq() => constructTransitions(EmptyWord)
      case Seq(rte) => constructTransitions(rte)
      case Seq(rte1, rte2) =>
        val (cat1In, cat1Out, transitions1) = constructTransitions(rte1)
        val (cat2In, cat2Out, transitions2) = constructTransitions(rte2)
        (cat1In, cat2Out, transitions1 ++
                          transitions2 ++
                          Seq(makeETransition(cat1Out, cat2In)))
      case _ =>
        constructTransitionsCat(Seq(rtes.head,
                                    createCat(rtes.tail)))
    }
  }

  // join the multiple outputs of a set of transitions (each labeled with a SimpleTypeD)
  //   into a single output, having epsilon transitions from the previous outputs
  //   to a single new output.   That output is the confluence (hence the name of the function)
  //   of the old output.   All the old outputs flow via epsilon transition into the new output.
  def confluxify(in:Int, outs:Seq[Int], transitions:Seq[(Int,SimpleTypeD,Int)]):TRANSITIONS = {
    val allStates = findAllStates(transitions) ++ outs + in
    val ini = makeNewState(allStates)
    val fin = makeNewState(allStates)

    val wrapped:Seq[TRANSITION] = transitions.map{case (x,td,y) => makeTTransition(x,td,y)}
    val prefix:Seq[TRANSITION] = makeETransition(ini,in) +: outs.map(f => makeETransition(f,fin))
    (ini,
      fin,
      prefix ++  wrapped)
  }

  def constructTransitionsNot(rte: Rte): TRANSITIONS = {
    val (in3,outs3, determinized) = constructDeterminizedTransitions(rte)
    val inverted = invertFinals(outs3, determinized)

    confluxify(in3,inverted,determinized)
  }

  def complete(in:Int, clean:Seq[(Int,SimpleTypeD,Int)]):Seq[(Int,SimpleTypeD,Int)] = {
    val allStates = findAllStates(clean)
    lazy val sink = makeNewState(allStates)
    val grouped = clean.groupBy(_._1)

    val completingTransitions = allStates.flatMap{ q =>
      grouped.get(q) match {
        // for states with no exiting transitions
        case None => Some((q,STop,sink))
        // for states withs ome exiting transitions, find the complement of their union
        case Some(transitions) =>
          val tds = transitions.map(_._2)
          val remaining = SNot(SOr.createOr(tds))
          // if either satisfiable is Some(True) or dont-know None,
          // then we have to create a transition to the sink state
          if (remaining.inhabited.contains(false))
            None
          else
            Some(q,remaining,sink)
      }
    }
    if (clean.isEmpty)
      Seq((in,STop,sink),
          (sink,STop,sink))
    else if (completingTransitions.isEmpty)
      clean
    else
      clean ++ completingTransitions ++ Seq((sink,STop,sink))
  }

  def invertFinals(outs:Seq[Int], completed:Seq[(Int,SimpleTypeD,Int)]):Seq[Int] = {
    findAllStates(completed).filter(x => ! outs.contains(x)).toSeq
  }

  @tailrec
  def constructTransitionsAnd(rtes: Seq[Rte]): TRANSITIONS = {
    rtes match {
      case Seq() => constructTransitions(Star(Sigma))
      case Seq(rte) => constructTransitions(rte)
      case Seq(rte1, rte2) =>
        val (and1in,and1outs,transitions1) = constructEpsilonFreeTransitions(rte1)
        val (and2in,and2outs,transitions2) = constructEpsilonFreeTransitions(rte2)
        val (sxpIn, sxpOuts, sxpTransitions) = sxp(and1in, and1outs, transitions1,
                                                   and2in, and2outs, transitions2,
                                                   (a:Boolean,b:Boolean) => a && b)
        val (renumIn, renumOuts, renumTransitions) = renumberTransitions(sxpIn,
                                                                         sxpOuts,
                                                                         sxpTransitions)
        confluxify(renumIn, renumOuts, renumTransitions)
      case _ =>
        constructTransitionsAnd(Seq(rtes.head,
                                    createAnd(rtes.tail)))
    }
  }

  def sxp(in1:Int, outs1:Seq[Int], transitions1:Seq[(Int,SimpleTypeD,Int)],
          in2:Int, outs2:Seq[Int], transitions2:Seq[(Int,SimpleTypeD,Int)],
          arbitrate:(Boolean,Boolean)=>Boolean
         ):((Int,Int), Seq[(Int,Int)], Seq[((Int,Int),SimpleTypeD,(Int,Int))]) = {
    import adjuvant.Accumulators.withCollector
    val grouped1 = transitions1.groupBy(_._1)
    val grouped2 = transitions2.groupBy(_._1)

    def stateTransitions(qq:(Int,Int)):Seq[(SimpleTypeD,(Int,Int))] = {
      val (q1,q2) = qq
      withCollector[(SimpleTypeD, (Int, Int))](
        collect => for {trs1 <- grouped1.get(q1)
                        trs2 <- grouped2.get(q2)
                        (_, td1, y1) <- trs1
                        (_, td2, y2) <- trs2
                        td = SAnd(td1, td2)
                        if !td.inhabited.contains(false)
                        } collect((td.canonicalize(), (y1, y2))))
    }

    val inX = (in1,in2)
    val (finalsX,transitionsX) =
      traceTransitionGraph[(Int,Int)](inX,
                                      stateTransitions,
      {case (x,y) => arbitrate(outs1.contains(x),outs2.contains(y))})

    (inX, finalsX, transitionsX)
  }

  def determinize(in:Int,
                  finals:Seq[Int],
                  transitions: Seq[(Int,SimpleTypeD,Int)]
                 ): (Int, Seq[Int], Seq[(Int,SimpleTypeD,Int)]) = {
    val grouped: Map[Int, Seq[(Int, SimpleTypeD, Int)]] = transitions.groupBy(_._1)

    def expandOneState(qs: Set[Int]): Seq[(SimpleTypeD,Set[Int])] = {
      val tds:Set[SimpleTypeD] = withSetCollector(collect =>
                                                    for {q: Int <- qs
                                                         trs <- grouped.get(q)
                                                         (_, td, _) <- trs
                                                         } collect(td))
      val tr2 = withSetCollector[(SimpleTypeD,Int)](collect =>
                         for { (td, factors, _) <- mdtd(tds)
                               q <- qs
                               trs <- grouped.get(q)
                               (_, td1, y ) <- trs
                               if factors.contains(td1)
                               } collect(td,y))
      for {(td, pairs) <- tr2.groupBy(_._1).toSeq
           nextStates = pairs.map(_._2)
           } yield (td,nextStates)
    }

    val inX = Set(in)
    val (expandedFinals,expandedTransitions) =
      traceTransitionGraph[Set[Int]](inX,
                                     expandOneState,
                                     qs => finals.exists(f => qs.contains(f)))
    renumberTransitions(inX,expandedFinals,expandedTransitions)
  }

  // We have transitions in some form other than (Int,SimpleTypeD,Int), and we
  //  need to convert to that form.  So we assign a new Int value to each (x,_,y)
  //  in the transitions, and return a translated copy of transitions, as well
  //  as updated value of in and outs.
  def renumberTransitions[T](in:T,
                             outs:Seq[T],
                             transitions:Seq[(T,SimpleTypeD,T)],
                            ):(Int,Seq[Int],Seq[(Int,SimpleTypeD,Int)]) = {
    val mapping:Map[T,Int] = (
      Seq(in) ++ outs ++ transitions.flatMap{ case (xx,_,yy) => List(xx,yy)})
      .distinct
      .map(qq => qq -> count())
      .toMap

    (mapping(in),
      outs.map(mapping),
      transitions.map{case (xx,td,yy) => (mapping(xx),td,mapping(yy))})
  }

  def findAllStates[T](transitions:Seq[(Int,T,Int)]):Set[Int]={
    (for {(x, _, y) <- transitions
          z <- Seq(x, y)} yield z).toSet
  }

  // type TRANSITION = (Int, Either[EmptyWord.type, SimpleTypeD], Int)
  def removeEpsilonTransitions(in: Int,
                               out: Int,
                               transitions: Seq[TRANSITION]
                              ): (Int, Seq[Int], Seq[(Int,SimpleTypeD,Int)]) = {
    val epsilonTransitions = transitions.collect { case (x, Left(EmptyWord), y) => (x, y) }.toSet
    val normalTransitions = transitions.collect { case (x, Right(tr), y) => (x,tr,y) }
    val allStates: Seq[Int] = findAllStates(transitions).toSeq
    def reachableFrom(q: Int): Set[Int] = {
      for {(x, y) <- epsilonTransitions
           if x == q} yield y
    }

    def extendClosure(m: Seq[Set[Int]]): Seq[Set[Int]] = {
      for {qs <- m
           } yield qs.flatMap(reachableFrom).union(qs)
    }

    val epsilonClosure = fixedPoint(allStates.map(q => Set(q)),
                                    extendClosure,
                                    (a: Seq[Set[Int]], b: Seq[Set[Int]]) => a == b)
    val transitions2 = for {(q, closure) <- allStates.zip(epsilonClosure)
                            c <- closure
                            (x, label, y) <- normalTransitions
                            if x == c
                            if x != q
                            } yield (q, label, y)
    // some states no longer exist after removing epsilon transitions
    val updatedTransitions = normalTransitions ++ transitions2
    val remainingStates = findAllStates(updatedTransitions)
    val finals = for {(q, closure) <- allStates.zip(epsilonClosure)
                      if remainingStates.contains(q) || q == in
                      if closure.contains(out)
                      } yield q
    (in, finals, updatedTransitions)
  }

  def constructThompsonDfa[E](rte:Rte, exitValue:E):Dfa[Any,SimpleTypeD,E] = {
    val (in3, outs3, determinized) = constructDeterminizedTransitions(rte)
    val fmap = outs3.map{i => i -> exitValue}.toMap

    new Dfa(Qids = findAllStates(determinized)+in3,
            q0id = in3,
            Fids = outs3.toSet,
            protoDelta = determinized.toSet,
            labeler = xymbolyco.GenusLabeler(),
            fMap = fmap)
  }
}