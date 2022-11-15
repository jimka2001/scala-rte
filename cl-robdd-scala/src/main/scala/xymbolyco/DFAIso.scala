// Copyright (©) 2021 EPITA Research and Development Laboratory
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
// NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package xymbolyco
import genus.SimpleTypeD
import xymbolyco._


object  DFAIso {

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