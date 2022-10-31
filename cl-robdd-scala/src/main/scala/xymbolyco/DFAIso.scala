package xymbolyco
import genus.SimpleTypeD
import xymbolyco._


object DFAIso {

      def isIsomorphiclocal(dfa: Dfa[Any,SimpleTypeD,Int],dfa2: Dfa[Any,SimpleTypeD,Int]):Some[Boolean]=
      {
        Some(true)
      }
      def isIsomorphic(dfa: Dfa[Any, SimpleTypeD,Int], dfa2:  Dfa[Any,SimpleTypeD,Int]): Some[Boolean]=
        {
          var fvals = dfa.fMap.toList.map(a=> a._2).toSet
          if(dfa2.fMap.toList.map(a=>a._2).toSet!=fvals)
          {
            return Some(false)
          }

          var head = fvals.head
          var res = Some(true)
          while(!res.contains(false)&& fvals.nonEmpty)
          {// TODO change FIDS to only contain the finals that are of the exit value
            val temp = isIsomorphiclocal(Dfa(dfa.Qids,dfa.q0id,dfa.Fids,dfa.protoDelta,dfa.labeler,dfa.fMap.filter(_._2 ==head)),
                                    Dfa(dfa2.Qids,dfa2.q0id,dfa2.Fids,dfa2.protoDelta,dfa2.labeler,dfa2.fMap.filter(_._2==head)))
            if(res.contains(Some(true)))
            {
              res = temp
            }
            else if(!temp.contains(Some(true)))
            {
              res = temp
            }
            fvals = fvals.tail
            head = fvals.head
          }
          res
        }
}
/*



 */