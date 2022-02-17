package xymbolyco

import adjuvant.Accumulators.{makeCounter,withSetCollector}
import adjuvant.Adjuvant.fixedPoint
import genus.Types.mdtd
import genus.{SEmpty, STop, SimpleTypeD}
import rte.Cat.createCat
import rte.{And, Cat, EmptySet, EmptyWord, Not, Or, Rte, Sigma, Singleton, Star}
import rte.Or.createOr

class Thompson {
  val count = makeCounter(1)
  val epsilon = Left(EmptyWord)
  type TRANSITION = (Int, Either[EmptyWord.type, SimpleTypeD], Int)
  type TRANSITIONS = (Int, Int, Seq[TRANSITION])

  def constructTransitions(rte: Rte): TRANSITIONS = {
    lazy val in = count()
    lazy val out = count()
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
    val sink = count()

    val (finals,clean) = removeEpsilonTransitions(notIn, notOut, transitions)
    val determinized = determinize(notIn,finals,clean)
    val completed = complete(notIn, notOut, clean)
    val inverted = invert(notIn, notOut, completed)

    ???
  }

  def complete(in:Int, out:Int, clean:Seq[(Int,SimpleTypeD,Int)]):Unit = {
    ???
  }
  def invert(in:Int, out:Int, completed:Unit):Unit = {
    ???
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

  def removeEpsilonTransitions(in: Int,
                               out: Int,
                               transitions: Seq[TRANSITION]
                              ): (Seq[Int], Seq[(Int,SimpleTypeD,Int)]) = {
    val epsilonTransitions = transitions.collect { case (x, Left(EmptyWord), y) => (x, y) }.toSet
    val normalTransitions = transitions.collect { case (x, Right(tr), y) => (x,tr,y) }
    val allStates: Seq[Int] = (for {(x, _, y) <- transitions
                                    z <- Seq(x, y)} yield z).distinct

    def reachableFrom(q: Int): Set[Int] = {
      for {(x, y) <- epsilonTransitions
           if x == q} yield y
    }

    def extendClosure(m: Seq[Set[Int]]): Seq[Set[Int]] = {
      for {qs <- m
           } yield qs.flatMap(reachableFrom)
    }

    val epsilonClosure = fixedPoint(allStates.map(q => Set(q)),
                                    extendClosure,
                                    (a: Seq[Set[Int]], b: Seq[Set[Int]]) => a == b)

    val transitions2 = for {(q, closure) <- allStates.zip(epsilonClosure)
                            (x, label, y) <- normalTransitions
                            if y == q
                            c <- closure
                            if c != q
                            } yield (x, label, c)

    val finals = for {(q, closure) <- allStates.zip(epsilonClosure)
                      if closure.contains(out)
                      } yield q

    (finals, normalTransitions ++ transitions2)
  }
}