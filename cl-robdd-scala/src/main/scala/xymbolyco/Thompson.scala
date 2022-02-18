package xymbolyco

import adjuvant.Accumulators.{makeCounter, withSetCollector}
import adjuvant.Adjuvant.fixedPoint
import genus.Types.mdtd
import genus.{SEmpty, SNot, SOr, STop, SimpleTypeD}
import rte.Cat.createCat
import rte.{And, Cat, EmptySet, EmptyWord, Not, Or, Rte, Sigma, Singleton, Star}
import rte.Or.createOr

import scala.annotation.tailrec

object Thompson {
  val count = makeCounter(1)
  val epsilon = Left(EmptyWord)
  type TRANSITION = (Int, Either[EmptyWord.type, SimpleTypeD], Int)
  type TRANSITIONS = (Int, Int, Seq[TRANSITION])

  @tailrec
  def makeNewState(allStates:Seq[Int]):Int = {
    val q = count()
    if (allStates.contains(q))
      makeNewState(allStates)
    else
      q
  }

  // makeTransition is useful for debugging
  def makeTransition(in:Int, td:SimpleTypeD, out:Int):TRANSITION = {
    (in,Right(td),out)
  }

  // makeTransition is useful for debugging
  def makeTransition(in:Int, epsilon:EmptyWord.type, out:Int): TRANSITION = {
    (in,Left(epsilon),out)
  }

  def constructTransitions(rte: Rte): TRANSITIONS = {
    lazy val in = count() // TODO this is wrong, need to assure new state
    lazy val out = count() // TODO this is wrong, need to assure new state
    rte match {
      case EmptyWord =>
        (in, out, Seq((in, epsilon, out)))
      case EmptySet =>
        (in, out, Seq((in, Right(SEmpty), out)))
      case Sigma =>
        (in, out, Seq((in, Right(STop), out)))
      case Singleton(td) =>
        (in, out, Seq((in, Right(td), out)))
      case Star(operand) =>
        val (inInner, outInner, transitions) = constructTransitions(operand)
        (in, out, transitions ++
                  Seq((in, epsilon, inInner),
                      (outInner, epsilon, out),
                      (in, epsilon, out),
                      (outInner, epsilon, inInner)))
      case Or(operands) => constructTransitionsOr(operands)
      case Cat(operands) => constructTransitionsCat(operands)
      case Not(operand) => constructTransitionsNot(operand)
      case And(operands) => constructTransitionsAnd(operands)
    }
  }

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
                  Seq((in, epsilon, or1In),
                      (in, epsilon, or2In),
                      (or1Out, epsilon, out),
                      (or2Out, epsilon, out)))
      case _ =>
        constructTransitionsOr(Seq(rtes.head,
                                   createOr(rtes.tail)))
    }
  }

  def constructTransitionsCat(rtes: Seq[Rte]): TRANSITIONS = {
    rtes match {
      case Seq() => constructTransitions(EmptyWord)
      case Seq(rte) => constructTransitions(rte)
      case Seq(rte1, rte2) =>
        val (cat1In, cat1Out, transitions1) = constructTransitions(rte1)
        val (cat2In, cat2Out, transitions2) = constructTransitions(rte2)
        (cat1In, cat2Out, transitions1 ++
                          transitions2 ++
                          Seq((cat1Out, epsilon, cat2In)))
      case _ =>
        constructTransitionsCat(Seq(rtes.head,
                                    createCat(rtes.tail)))
    }
  }

  def constructTransitionsNot(rte: Rte): TRANSITIONS = {
    val (notIn, notOut, transitions) = constructTransitions(rte)

    val (in,finals,clean) = removeEpsilonTransitions(notIn, notOut, transitions)
    val completed = complete(clean)
    val determinized = determinize(in,finals,clean)

    val inverted = invert(notIn, finals, completed)

    ???
  }

  def complete(clean:Seq[(Int,SimpleTypeD,Int)]):Seq[(Int,SimpleTypeD,Int)] = {
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
    if (completingTransitions.isEmpty)
      clean
    else
      clean ++ completingTransitions ++ Seq((sink,STop,sink))
  }

  def invert(outs:Seq[Int], completed:Seq[(Int,SimpleTypeD,Int)]):Seq[Int] = {
    findAllStates(completed).filter(x => ! outs.contains(x)).toSeq
  }

  def constructTransitionsAnd(operands: Seq[Rte]): TRANSITIONS = {
    ???
  }

  def determinize(in:Int,
                  finals:Seq[Int],
                  transitions: Seq[(Int,SimpleTypeD,Int)]
                 ): (Seq[Int], Seq[(Int,SimpleTypeD,Int)]) = {
    val grouped: Map[Int, Seq[(Int, SimpleTypeD, Int)]] = transitions.groupBy(_._1)

    def f(qs: Set[Int]): Seq[(Set[Int],SimpleTypeD,Set[Int])] = {
      val tds:Set[SimpleTypeD] = withSetCollector(collect =>
                                                    for {q: Int <- qs
                                                         trs <- grouped.get(q)
                                                         (x, td, y) <- trs
                                                         } collect(td))
      val tr2 = withSetCollector[(SimpleTypeD,Int)](collect =>
                         for { (td,factors,disjoints) <- mdtd(tds)
                               q <- qs
                               trs <- grouped.get(q)
                               (_, td1, y ) <- trs
                               if factors.contains(td1)
                               } collect(td,y))
      for {(td, pairs) <- tr2.groupBy(_._1).toSeq
           nextStates = pairs.map(_._2)
           } yield (qs,td,nextStates)
    }

    def expand(toDoList:List[Set[Int]],
               done:Set[Set[Int]],
               transitions:Seq[(Set[Int],SimpleTypeD,Set[Int])]
              ):Seq[(Set[Int],SimpleTypeD,Set[Int])] = {
      toDoList match {
        case List() => transitions
        case qs::toDo if done.contains(qs) =>
          expand(toDo,done,transitions)
        case qs::toDo =>
          val newTransitions = f(qs)
          expand(toDo,
                 done + qs,
                 transitions ++ newTransitions)
      }
    }

    def renumberStates(tr1:List[(Set[Int],SimpleTypeD,Set[Int])],
                       tr2:List[(Int,SimpleTypeD,Int)],
                       mapping:Map[Set[Int],Int]):Seq[(Int,SimpleTypeD,Int)] = {
      tr1 match {
        case List() => tr2
        case (qs1, td, qs2) :: trs =>
          val x = mapping.getOrElse(qs1, count())
          val y = if (qs1 == qs2) x else mapping.getOrElse(qs2, count())
          renumberStates(trs,
                         (x, td, y) :: tr2,
                         mapping ++ Map(qs1 -> x,
                                        qs2 -> y))
      }
    }
    (Seq[Int](),
      renumberStates(
        expand(List(Set(in)), Set(), Seq()).toList,
        List(),
        Map()))
  }

  def findAllStates[T](transitions:Seq[(Int,T,Int)]):Seq[Int]={
    (for {(x, _, y) <- transitions
          z <- Seq(x, y)} yield z).distinct
  }

  // type TRANSITION = (Int, Either[EmptyWord.type, SimpleTypeD], Int)
  def removeEpsilonTransitions(in: Int,
                               out: Int,
                               transitions: Seq[TRANSITION]
                              ): (Int, Seq[Int], Seq[(Int,SimpleTypeD,Int)]) = {
    val epsilonTransitions = transitions.collect { case (x, Left(EmptyWord), y) => (x, y) }.toSet
    val normalTransitions = transitions.collect { case (x, Right(tr), y) => (x,tr,y) }
    val allStates: Seq[Int] = findAllStates(transitions)
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
    val finals = for {(q, closure) <- allStates.zip(epsilonClosure)
                      if closure.contains(out)
                      } yield q
    (in, finals, normalTransitions ++ transitions2)
  }
}