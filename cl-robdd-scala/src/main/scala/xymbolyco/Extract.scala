// Copyright (c) 2021 EPITA Research and Development Laboratory

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

package xymbolyco

import rte._
import genus._

object Extract {
  def dfaToRte[E](dfa:Dfa[Any,SimpleTypeD,E],default:E):Map[E,Rte] = {
    val rt_map = extractRte(dfa)
    if (rt_map.isEmpty)
      Map(default -> EmptySet)
    else
      rt_map
  }
  def extractRte[E](dfa_given:Dfa[Any,SimpleTypeD,E]):Map[E,Rte] = {
    import Minimize._
    type Triple = (Int, Rte, Int)
    //    1. minimize and trim the given dfa
    //    2. generate a list of transition triples [from label to]
    //    3. add transitions from extra-state-I to all initial states with :epsilon transition
    //    4. add transitions from all accepting states to extra-state-F (one per exit value) with :epsilon transition
    //    5. loop on each state
    //    6.    partition transitions into 4 groups [to-this-state loops-on-state from-state everything-else]
    //    7.    combine parallel transitions
    //    8.    n^2 iteration to-this-state x from-this-state
    //    9.    append new transitions in next iteration of loop 5.
    //    10. this reduces to one transition per exit value, returns the map of exit-value to label

    // step 1
    val dfa = trim(minimize(dfa_given))
    // step 2
    val old_transition_triples = for {q <- dfa.Q
                                      tr <- q.transitions
                                      } yield (tr.source.id, Singleton(tr.label), tr.destination.id)

    // step 3
    val new_initial_transitions = Seq((-1, EmptyWord, 0))
    val nindexToState = (for {(q, id) <- dfa.F.zipWithIndex} yield -2 - id -> q).toMap
    // step 4
    val new_final_transitions = for {(id, q) <- nindexToState
                                     } yield (q.id, EmptyWord, id)

    def combine_parallel_labels(operands: Seq[Rte]): Rte =
      Or.createOr(operands).canonicalize

    def extract_labels(triples: Seq[Triple]): Seq[Rte] =
      for {(_, rt, _) <- triples} yield rt

    def combine_parallel(triples: Seq[(Int, Rte, Int)]): Seq[(Int, Rte, Int)] = {
      (for {((from, to), triples) <- triples.groupBy(triple => (triple._1, triple._3))
            label = combine_parallel_labels(extract_labels(triples))
            } yield (from, label, to)).toSeq
    }

    def eliminate_state(transition_triples: Seq[(Int, Rte, Int)], qid: Int) = {
      type LLLL = (List[Triple], List[Triple], List[Triple], List[Triple])

      def f(acc: LLLL, triple: Triple): LLLL = {
        val (x_to_q, q_to_q, q_to_x, others) = acc
        val (src, _, dst) = triple
        if ((src == qid) && (dst == qid))
          (x_to_q, triple :: q_to_q, q_to_x, others)
        else if (src == qid)
          (x_to_q, q_to_q, triple :: q_to_x, others)
        else if (dst == qid)
          (triple :: x_to_q, q_to_q, q_to_x, others)
        else
          (x_to_q, q_to_q, q_to_x, triple :: others)
      }

      val z = List[Triple]()
      // step 6
      val (x_to_q, q_to_q, q_to_x, others) = transition_triples.foldLeft((z, z, z, z))(f)
      // step 7
      val self_loop_label = combine_parallel_labels(extract_labels(q_to_q))
      // step 8
      val new_triples = for {(src, pre_label, _) <- combine_parallel(x_to_q)
                             (_, post_label, dst) <- combine_parallel(q_to_x)
                             } yield (src,Cat(pre_label,Star(self_loop_label),post_label),dst)

      // return from eliminate_state
      others.reverse ++ new_triples
    }

    val starting_triples = new_initial_transitions ++ old_transition_triples ++ new_final_transitions
    // step 5 and step 9
    val new_transition_triples = dfa.Q.foldLeft(starting_triples){
      case (acc,q) => eliminate_state(acc,q.id)
    }
    val grouped = new_transition_triples.groupBy{case (i,_,f) =>
      assert(f < 0)
      assert(i == -1, s"i=$i  f=$f, new_transition_triples=$new_transition_triples")
      // compute exit_value of the final state whose negative-index is f
      dfa.fMap(nindexToState(f).id)
    }
    // return value of extractRte
    for{(exit_value,triples) <- grouped
        pretty = combine_parallel_labels(extract_labels(triples))
        }
    // step 10
    yield (exit_value, pretty)
  }
}
