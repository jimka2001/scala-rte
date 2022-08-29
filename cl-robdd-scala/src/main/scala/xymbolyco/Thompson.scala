package xymbolyco


import adjuvant.Accumulators.{makeCounter, withSetCollector}
import adjuvant.Adjuvant.{fixedPoint, traceGraph, uniquify}
import genus.Types.mdtd
import genus.{SAnd, SNot, SOr, STop, SimpleTypeD}
import rte.And.createAnd
import rte.Cat.createCat
import rte.{And, Cat, EmptySet, EmptyWord, Not, Or, Rte, Sigma, Singleton, Star}
import rte.Or.createOr
import xymbolyco.GraphViz.dfaView

import scala.annotation.tailrec

object Thompson {
  private val count = makeCounter()

  def makeNewState[V,L](in:V, outs:Seq[V], transitions:Seq[(V,L,V)], count:()=>V):V = {

    @tailrec
    def recur(): V = {
      val q = count()
      if (in == q)
        recur()
      else if (outs.contains(q))
        recur()
      else if (transitions.exists { case (x, _, y) => x == q || y == q })
        recur()
      else
        q
      }
    recur()
  }

  def constructEpsilonFreeTransitions(rte: Rte): (Int,Seq[Int],Seq[(Int,SimpleTypeD,Int)]) = {
    val (in:Int, out:Int, transitions:Seq[(Int, Option[SimpleTypeD], Int)]) = constructTransitions(rte)
    removeEpsilonTransitions(in,out,transitions)
  }

  def constructDeterminizedTransitions(rte:Rte): (Int,Seq[Int],Seq[(Int,SimpleTypeD,Int)]) = {
    val (in2,outs2,clean) = constructEpsilonFreeTransitions(rte)
    val completed = complete(in2, outs2, clean)
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

  def constructVarArgsTransitions(operands:Seq[Rte],
                                  id:Rte,
                                  binOp:(Rte,Rte)=>Rte,
                                  varArgsOp:Seq[Rte]=>Rte,
                                  continuation:(Rte,Rte)=>(Int,Int,Seq[(Int,Option[SimpleTypeD],Int)])
                                 ): (Int,Int,Seq[(Int,Option[SimpleTypeD],Int)]) = {
    operands match {
      case Seq() => constructTransitions(id)
      case Seq(r) => constructTransitions(r)
      case Seq(rte1, rte2) => continuation(rte1, rte2)
      // If there are more than two arguments, we rewrite as follows
      //  Or(a,b,c,d,e,f,...) -> Or(a,Or(b,d,e,f,...))
      //  Now Or has two arguments.
      // TODO, here we are grouping 1 way.  This decision is arbitrary.
      //    We could very well group different ways  E.g., Or(Or(a,b),Or(c,d),Or(e,f),g)
      //    We need to investigate whether a smarter grouping has an effect on
      //    the size (or other characteristics) of the automaton being constructed.
      case _ if operands.size % 2 == 0 =>
        constructTransitions(varArgsOp(operands.grouped(2).map {
          case Seq(x, y) => binOp(x, y)
          case _ => sys.error("uneven size")
        }.toSeq))
      case _ => constructTransitions(binOp(operands.head,
                                           varArgsOp(operands.tail)))
    }
  }

  // Construct a sequence of transitions specifying an epsilon-nondeterministic-finite-automaton.
  // Also return the initial and final state.
  def constructTransitions(rte: Rte): (Int, Int, Seq[(Int, Option[SimpleTypeD], Int)]) = {
    lazy val in = count()
    lazy val out = count()
    rte match {
      case EmptyWord =>
        (in, out, Seq((in, None, out)))
      case EmptySet =>
        (in, out, Seq())
      case Sigma =>
        (in, out, Seq((in, Some(STop), out)))
      case Singleton(td) =>
        (in, out, Seq((in, Some(td), out)))
      case Star(rte) =>
        val (inInner, outInner, transitions) = constructTransitions(rte)
        (in, out, transitions ++
                  Seq((in, None, inInner),
                      (outInner, None, out),
                      (in, None, out),
                      (outInner, None, inInner)))
      // Handle Or(...)
      // Or might have 0 or more rts, we need to handle 4 cases.
      case Or(rtes) =>
        constructVarArgsTransitions(rtes,
                                    EmptySet,
                                    (rte1: Rte, rte2: Rte) => Or(rte1, rte2),
                                    createOr,
                                    (rte1: Rte, rte2: Rte) => {
                                      val (or1In, or1Out, transitions1) = constructTransitions(rte1)
                                      val (or2In, or2Out, transitions2) = constructTransitions(rte2)
                                      (in, out, transitions1 ++
                                                transitions2 ++
                                                Seq((in, None, or1In),
                                                    (in, None, or2In),
                                                    (or1Out, None, out),
                                                    (or2Out, None, out)))
                                    }
                                    )

      // Handle Cat(...)
      // Cat might have 0 or more rts, we need to handle 4 cases.
      case Cat(rtes) =>
        constructVarArgsTransitions(rtes,
                                    EmptyWord,
                                    (rte1: Rte, rte2: Rte) => Cat(rte1, rte2),
                                    createCat,
                                    (rte1: Rte, rte2: Rte) => {
                                      val (cat1In, cat1Out, transitions1) = constructTransitions(rte1)
                                      val (cat2In, cat2Out, transitions2) = constructTransitions(rte2)
                                      (cat1In, cat2Out, transitions1 ++
                                                        transitions2 ++
                                                        Seq((cat1Out, None, cat2In)))
                                    }
                                    )

      // Handle And(...)
      // And might have 0 or more rts, we need to handle 4 cases.
      case And(rtes) =>
        constructVarArgsTransitions(rtes,
                                    Star(Sigma),
                                    (rte1: Rte, rte2: Rte) => And(rte1, rte2),
                                    createAnd,
                                    (rte1: Rte, rte2: Rte) => {
                                      constructTransitionsAnd(rte1, rte2)
                                    }
                                    )

      // Handle Not(rte)
      case Not(rte) => constructTransitionsNot(rte)
    }
  }

  // join the multiple outputs of a set of transitions (each labeled with a SimpleTypeD)
  //   into a single output, having epsilon transitions from the previous outputs
  //   to a single new output.   That output is the confluence (hence the name of the function)
  //   of the old output.   All the old outputs flow via epsilon transition into the new output.
  def confluxify(in:Int,
                 outs:Seq[Int],
                 transitions:Seq[(Int,SimpleTypeD,Int)]
                ):(Int, Int, Seq[(Int, Option[SimpleTypeD], Int)]) = {
    val ini = makeNewState(in,outs,transitions,count)
    val fin = makeNewState(in,outs,transitions,count)

    val wrapped:Seq[(Int, Option[SimpleTypeD], Int)] = transitions.map{case (x,td,y) => (x,Some(td),y)}
    val prefix:Seq[(Int, Option[SimpleTypeD], Int)] = (ini, None, in) +: outs.map(f => (f, None, fin))
    (ini,
      fin,
      prefix ++  wrapped)
  }

  // Construct an epsilon-NFA representing the complement (NOT) of the given Rte.
  // This is done by constructing the given Rte in its determinized form,
  // swapping final with non-final states, and confluxifying the result,
  // thus creating an epsilon-NFA with a single input and single output.
  def constructTransitionsNot(rte: Rte): (Int, Int, Seq[(Int, Option[SimpleTypeD], Int)]) = {
    val (in,outs, determinized) = constructDeterminizedTransitions(rte)
    val inverted = invertFinals(outs, determinized)

    confluxify(in,inverted,determinized)
  }

  // Transform a sequence of transitions (either deterministic or otherwise)
  // into a set of completed transitions.  I.e., the union of the types labeling
  // any arbitrary transition is STop.  In the case when it cannot be determined
  // whether the transitions are already locally complete, an additional transition
  // is added with is the complement of the existing transitions.  This may be
  // empty but if it is empty, .inhabited returns None (dont-know).
  def complete(in:Int,
               outs:Seq[Int],
               clean:Seq[(Int,SimpleTypeD,Int)]
              ):Seq[(Int,SimpleTypeD,Int)] = {
    val allStates = findAllStates(clean)
    lazy val sink = makeNewState(in,outs,clean,count)
    val grouped = clean.groupBy(_._1)

    val completingTransitions = allStates.flatMap{ q =>
      grouped.get(q) match {
        // for states with no exiting transitions
        case None => Some((q,STop,sink))
        // for states with some exiting transitions, find the complement of their union
        case Some(transitions) =>
          val tds = transitions.map(_._2)
          val remaining = SNot(SOr.createOr(tds))
          // if either satisfiable is Some(True) or dont-know None,
          // then we have to create a transition to the sink state
          if (remaining.inhabited.contains(false))
            None
          else
            Some((q,remaining,sink))
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

  // Return the set of states which are not final states.
  def invertFinals(outs:Seq[Int], completed:Seq[(Int,SimpleTypeD,Int)]):Seq[Int] = {
    findAllStates(completed).filter(q => ! outs.contains(q)).toSeq
  }

  def constructTransitionsAnd(rte1:Rte,rte2:Rte): (Int, Int, Seq[(Int, Option[SimpleTypeD], Int)]) = {
    // The Thompson construction does not have a case for AND.
    // So what we do here is take the two arguments of AND
    // and perform a synchronized cross product on the two results.
    val (and1in,and1outs,transitions1) = constructEpsilonFreeTransitions(rte1)
    val (and2in,and2outs,transitions2) = constructEpsilonFreeTransitions(rte2)
    val (sxpIn, sxpOuts, sxpTransitions) = sxp(and1in, and1outs, transitions1,
                                               and2in, and2outs, transitions2,
                                               (a:Boolean,b:Boolean) => a && b)
    val (renumIn, renumOuts, renumTransitions) = renumberTransitions(sxpIn,
                                                                     sxpOuts,
                                                                     sxpTransitions,
                                                                     count)
    confluxify(renumIn, renumOuts, renumTransitions)
  }

  // Compute the synchronized cross product.
  // Given two descriptions of finite automata (either deterministic or otherwise)
  // use a graph-tracing algorithm to compute a new list of transitions
  // with all the reachable states in the cartesian product.  In the case that
  // a transition is provably unsatisfiable, the transition is omitted.
  // However if the transition is provably satisfiable or dont-know, then
  // the transition is included.
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

  // remove non-accessible transitions, and non-accessible final states
  def accessible(in:Int,
                 outs:Seq[Int],
                 transitions:Seq[(Int,SimpleTypeD,Int)]
                ):(Int,Seq[Int],Seq[(Int,SimpleTypeD,Int)]) = {
    val grouped = transitions.groupMap(_._1){case (_,td,y) => (td,y)}
    val (accessibleOuts,accessibleTransitions) = traceTransitionGraph[Int](in,
                                                                         q => grouped.getOrElse(q,Seq()),
                                                                         q => outs.contains(q))
    (in,accessibleOuts,accessibleTransitions)
  }

  def coaccessible(in:Int,
                   outs:Seq[Int],
                   transitions:Seq[(Int,SimpleTypeD,Int)]
                  ):(Int,Seq[Int],Seq[(Int,SimpleTypeD,Int)]) = {
    val proxy = makeNewState(in, outs, transitions,count)
    val augmentedTransitions = transitions ++ outs.map(q => (q,STop,proxy))
    val grouped = augmentedTransitions.groupMap(_._3){case (x,td,_) => (td,x)}
    // we now compute the coaccessible transitions, but each transition is reversed (z,td,x)
    val (coaccessibleOuts,reversedTransitions) = traceTransitionGraph[Int](proxy,
                                                                         q => grouped.getOrElse(q,Seq()),
                                                                         q => q == in)
    val coaccessibleTransitions = for{(y,td,x) <- reversedTransitions
                                      if y != proxy } yield (x,td,y)
    (in,outs,coaccessibleTransitions)
  }

  // remove transitions which are not accessible and are not coaccessible.
  // This doesn't change the input state, but some final states may be lost.
  def trim(in:Int,
           finals:Seq[Int],
           transitions:Seq[(Int,SimpleTypeD,Int)]
          ):(Int,Seq[Int],Seq[(Int,SimpleTypeD,Int)]) = {
    val (aIn, aFinals, aTransitions) = accessible(in,finals,transitions)
    coaccessible(aIn,aFinals,aTransitions)
  }

  // Given a description of a non-deterministic FA, with epsilon transitions
  // already removed, use a graph-tracing algorithm to compute the reachable
  // states in the determinized automaton.
  def determinize(in:Int,
                  finals:Seq[Int],
                  transitions: Seq[(Int,SimpleTypeD,Int)]
                 ): (Int, Seq[Int], Seq[(Int,SimpleTypeD,Int)]) = {
    val grouped: Map[Int, Seq[(Int, SimpleTypeD, Int)]] = transitions.groupBy(_._1)

    def expandOneState(qs: Set[Int]): Seq[(SimpleTypeD,Set[Int])] = {
      // Given a set of states, find all the SimpleTypeD which are labels of
      //   transitions leaving any of those states.
      val tds:Set[SimpleTypeD] = withSetCollector(collect =>
                                                    for {q: Int <- qs
                                                         trs <- grouped.get(q)
                                                         (_, td, _) <- trs
                                                         } collect(td))
      // The set tds of SimpleTypeD might contain some non-disjoint types.
      //   So we call compute the minimum disjoint type decomposition (mdtd)
      //   and create one transition per type per type from this decomposition.
      //   This is done in two steps.
      //   step 1. compute tr2 which is the list of (type,next-state) pairs.
      //   step 2. for { (td,pairs) ...} compute the seq of next states corresponding
      //      to each type.
      //      The pair returned from step 2 is exactly what is needed by traceGraph
      val tr2 = withSetCollector[(SimpleTypeD,Int)](collect =>
                         for { (td, (factors, _)) <- mdtd(tds)
                               q <- qs
                               trs <- grouped.get(q)
                               (_, td1, y ) <- trs
                               if factors.contains(td1)
                               } collect((td,y)))
      for {(td, pairs) <- tr2.groupBy(_._1).toSeq
           nextStates = pairs.map(_._2)
           } yield (td,nextStates)
    }

    val inX = Set(in)
    val (expandedFinals,expandedTransitions) =
      traceTransitionGraph[Set[Int]](inX,
                                     expandOneState,
                                     qs => finals.exists(f => qs.contains(f)))
    // we now have transitions which map Set[Int] to Set[Int]
    //   where each state (Set[Int]) represents 1 state.  We must
    //   now replace each Set[Int] with a corresponding Int.
    renumberTransitions(inX,expandedFinals,expandedTransitions,count)
  }

  // We have transitions in some form other than DFA_TRANSITION, and we
  //  need to convert to that form.  So we assign a new Int value to each (x,_,y)
  //  in the transitions, and return a translated copy of transitions, as well
  //  as updated value of in and outs.
  def renumberTransitions[T](in:T,
                             outs:Seq[T],
                             transitions:Seq[(T,SimpleTypeD,T)],
                             count: ()=>Int,
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

  // Given a sequence of transitions, compute and return the set
  // of states mentioned therein.
  def findAllStates[V,L](transitions:Seq[(V,L,V)]):Set[V]={
    (for {(x, _, y) <- transitions
          z <- Seq(x, y)} yield z).toSet
  }
  // given a Dfa, and a Map[Int,Int], return a new Dfa will all the states
  //   mapped by the mapping.
  // states not mentioned in the mapping are filtered out.
  def reNumber[L](in: Int,
               out: Seq[Int],
               transitions: Seq[(Int, L, Int)],
               rename: Map[Int,Int]): (Int, Seq[Int], Seq[(Int, L, Int)]) = {
    assert(rename.contains(in))
    val out2 = out.filter(q => rename.contains(q))
    val transitions2 = transitions.filter(tr => rename.contains(tr._1) && rename.contains(tr._3))
    (rename(in),
      out2.map(rename),
      transitions2.map(tr => (rename(tr._1),tr._2, rename(tr._3))))
  }

  def canonicalizeDfa[L](in: Int,
                         out: Seq[Int],
                         transitions: Seq[(Int, L, Int)],
                         cmp: (L, L) => Boolean): (Int, Seq[Int], Seq[(Int, L, Int)]) = {
    def generateCanonMap(toDo: List[Int],
                         done: Set[Int],
                         mapping: Map[Int, Int],
                         nextState: Int): Map[Int, Int] = {
      toDo match {
        case Nil => mapping
        case q0 :: qs if done.contains(q0) =>
          generateCanonMap(qs, done, mapping, nextState)
        case q0 :: _ if !mapping.contains(q0) =>
          generateCanonMap(toDo,
                           done,
                           mapping + (q0 -> nextState),
                           nextState + 1)
        case q0 :: qs =>
          val stateTransitions = transitions
            .filter(tr => (tr._1 == q0) && !done.contains(tr._3))
            .sortWith((tr1, tr2) => cmp(tr1._2, tr2._2))
            .toList
          val unvisited = uniquify(stateTransitions.map(_._3).reverse).reverse
          val unvisitedMap = unvisited.zip(nextState until nextState + unvisited.size).toMap
          generateCanonMap(qs,
                           done + q0,
                           mapping ++ unvisitedMap,
                           nextState + unvisited.size)
      }
    }

    reNumber(in, out, transitions,
             generateCanonMap(List(in), Set(), Map(), 0))
  }

  // compute and return the transitions of a non-deterministic finite automaton
  // having its epsilon transitions removed.  This is done by computing the forward
  // epsilon-closure of every state.  Then advancing transitions (x,td,y) forward
  // according to (x,td,z) where z in in the epsilon-closure of z.
  // Then removing all epsilon transitions.
  // We also trim away transitions (x,td,y) such that all remaining transitions
  // are both accessible and coaccessible.
  def removeEpsilonTransitions(in: Int,
                               out: Int,
                               transitions: Seq[(Int, Option[SimpleTypeD], Int)]
                              ): (Int, Seq[Int], Seq[(Int,SimpleTypeD,Int)]) = {
    val epsilonTransitions = transitions.collect { case (x, None, y) => (x, y) }.toSet
    val normalTransitions = transitions.collect { case (x, Some(tr), y) => (x,tr,y) }
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
    trim(in,finals,updatedTransitions)
  }

  // Given a sequence and an Rte,
  //    construct the Thompson automaton, remove epsilon-transitions,
  //    but don't determinize.
  //    return None, if the sequence does not match the expression
  //    else return Some(exitValue)
  def simulate[E](sequence:Seq[Any],
                  exitValue:E,
                  rte:Rte
                 ):Option[E] = {
    val (in, outs, transitions) = constructEpsilonFreeTransitions(rte)
    simulate(sequence,exitValue,
             in, outs, transitions)
  }

  // trace a computation through a non-deterministic finite state machine
  // given in terms of in, outs, and transitions.
  def simulate[E](sequence:Seq[Any],
                  exitValue:E,
                  in:Int,
                  outs:Seq[Int],
                  transitions:Seq[(Int,SimpleTypeD,Int)]
                 ):Option[E] = {
    //import cats._
    import cats.syntax.all._
    val groups = transitions.groupBy(_._1)
    // To perform this computation, we use foldM to walk the variable, v, through
    //  the input sequence.  At each iteration of the foldM, we take a set, qs,
    //  of states, Set[Int], and map each Int, q, to a set of next states
    //  my examining groups(q) and calling td.typep(v).   One of two things
    //  happens with this `foldM`.
    //  Either 1: it computes an empty set of next states, mid-way through the
    //  sequence, in which case foldM aborts and returns None, or 2: it reaches
    //  the end of the sequence and returns a (possibly empty) set of states
    //  reached, in which case it returns Some(that-set).
    sequence.foldM[Option,Set[Int]](Set(in)){
      (qs:Set[Int],v:Any) =>
        if (qs.isEmpty)
        // no next state, but not finished with the sequence, just abort the foldM
          None
        else
          Some(for {q <- qs
                     (_, td, y) <- groups.getOrElse(q,Seq())
                     if td.typep(v)
                     } yield y)
    } match {
      //  Finally, given this set of states reached, we examine them to
      //  determine whether at least one of them is a final state.  If not,
      //  we return None from simulate, else we return Some(exitValue).
      case None => None
      case Some(computation) =>
        if (computation.exists(f => outs.contains(f)))
          Some(exitValue)
        else
          None
    }
  }

  // Construct a deterministic finite automaton, instance of Dfa[Any,SimpleTypeD,E]
  // given an Rte, using the Thompson construction algorithm which has been
  // extended to handle Not and And.
  def constructThompsonDfa[E](rte:Rte, exitValue:E):Dfa[Any,SimpleTypeD,E] = {
    val (in0, outs0, determinized0) = constructDeterminizedTransitions(rte)
    val (in, outs, determinized) = renumberTransitions(in0, outs0, determinized0, makeCounter(0,1))
    val fmap = outs.map{i => i -> exitValue}.toMap

    new Dfa(Qids = findAllStates(determinized)+in,
            q0id = in,
            Fids = outs.toSet,
            protoDelta = determinized.toSet,
            labeler = xymbolyco.GenusLabeler(),
            fMap = fmap)
  }
}

object Profiling {

  def check(pattern:Rte,r:Int,depth:Int):Map[String,Int] = {
    val dfa_thompson = Minimize.trim(Thompson.constructThompsonDfa(pattern, 42))
    val min_thompson = Minimize.minimize(dfa_thompson)
    val dfa_brzozowski = Minimize.trim(pattern.toDfa(42))
    val min_brzozowski = Minimize.minimize(dfa_brzozowski)
    val data = Map(
      "thompson_size" -> dfa_thompson.Q.size,
      "thompson_min" -> min_thompson.Q.size,
      "brzozowski_size" -> dfa_brzozowski.Q.size,
      "brzozowski_min" -> min_brzozowski.Q.size)
    if (min_brzozowski.Q.size != min_thompson.Q.size) {
      dfaView(dfa_thompson, "thompson", abbrev = true, label = Some(s"$depth.$r " + pattern.toString))
      dfaView(min_thompson, "thompson-min", abbrev = true, label = Some(s"$depth.$r " + pattern.toString))
      dfaView(dfa_brzozowski, "brzozowski", abbrev = true, label = Some(s"$depth.$r " + pattern.toString))
      dfaView(min_brzozowski, "brzozowski-min", abbrev = true, label = Some(s"$depth.$r " + pattern.toString))

      dfaView(Rte.dfaXor(min_thompson, min_brzozowski),
              title = "xor",
              abbrev = true,
              label = Some(s"$depth.$r " + pattern.toString))
    }
    data
  }

  def main(argv:Array[String]) : Unit = { // ("brz vs thomp") {
    // here we generate some random Rte patterns, then construct
    //  both the Thompson and Brzozowski automata, trim and minimize
    //  them both, and look for cases where the resulting size
    //  is different in terms of state count.

    val num_random_tests = 100*3
    for {depth <- 5 until 6
         r <- 0 until num_random_tests
         pattern = Rte.randomRte(depth)
         data = check(pattern,r,depth)
         } {
      println(depth, data, pattern)
    }
  }
}
