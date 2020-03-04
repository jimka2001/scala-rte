package dfa

import scala.annotation.tailrec

object Minimize {
  def fixedPoint[V](value: V, f: V => V, cmp: (V, V) => Boolean): V = {
    @tailrec
    def recur(value: V): V = {
      val newValue = f(value)
      if (cmp(value, newValue))
        value
      else
        recur(newValue)
    }

    recur(value)
  }

  def partitionBy[S,T](domain:Set[S], f:S=>T):Set[Set[S]] = {
    if (domain.size == 1)
      Set(domain)
    else
      domain.groupBy(f).values.toSet
  }

  def findHopcroftPartition[L, E](dfa: Dfa[L, E]): Set[Set[State[L, E]]] = {
    type STATE = State[L, E]
    type EQVCLASS = Set[STATE]
    type PARTITION = Set[EQVCLASS]

    val Pi0 = partitionBy(dfa.F, dfa.exitValue) + dfa.Q.diff(dfa.F)

    def partitionEqual(pi1    : PARTITION, pi2: PARTITION): Boolean =
      pi1 == pi2

    def refine(partition: PARTITION): PARTITION = {
      def phi(source: STATE, label: L): EQVCLASS = {
        partition.find(_.contains(source.delta(label))).get
      }

      def Phi(s: STATE): Set[(L, EQVCLASS)] = {
        val m = for {
          (eqvClass, trans) <- s.transitions.groupBy(tr => phi(s, tr.label))
          label = trans.map(_.label).reduce(dfa.combineLabels)
        } yield (label, eqvClass)
        m.toSet
      }
      def repartition(eqvClass:EQVCLASS):PARTITION = {
        partitionBy(eqvClass,Phi)
      }
      partition.flatMap(repartition)
    }

    fixedPoint(Pi0, refine, partitionEqual)
  }

  def mergeMap[K, V](m:Map[K,V], kv:(K,V), combine:(V,V)=>V):Map[K,V] = {
    kv match {
      case (key,value) =>
        if (m.contains(key))
          m + (key -> combine(m(key), value))
        else
          m + (key -> value)
    }
  }

  def minimize[L, E](dfa: Dfa[L, E]): Dfa[L, E] = {
    val PiMinimized = findHopcroftPartition(dfa)
    // associate each element of PiMinimized with a new id
    // attempt to correlate the old state.id with the new
    def minState(eqvClass:Set[State[L,E]]):Int = {
      eqvClass.map(_.id).reduce( (a:Int,b:Int) => a.min(b))
    }
    val ids:Map[Set[State[L,E]],Int] = PiMinimized.map{(eqvClass)=>(eqvClass -> minState(eqvClass))}.toMap
    def eqvClassOf(s:State[L,E]):Set[State[L,E]] = {
      PiMinimized.find(eqvClass => eqvClass.contains(s)).get
    }
    def newId(s:State[L,E]):Int = {
      ids(eqvClassOf(s))
    }
    //val ids: Array[Set[State[L,E]]] = PiMinimized.toArray
    val newIds = ids.values.toSet
    val newQ0:Int = newId(dfa.q0)
    val newFids = PiMinimized.filter(eqv => dfa.F.exists(q => eqv.contains(q))).map(ids)

    val newProtoDelta = for{
      q <- dfa.Q
      tr <- q.transitions
    } yield (newId(tr.source), tr.label, newId(tr.destination))

    val newFmap = for {
      (eqv,id) <- ids
      if newFids.contains(id)
      q = eqv.head
    } yield (id -> dfa.exitValue(q))

    // return a newly constructed Dfa extracted from the Hopcroft partition minimization
    new Dfa[L, E](newIds, newQ0, newFids, newProtoDelta.toSet, dfa.combineLabels, newFmap.toMap)
  }
}
