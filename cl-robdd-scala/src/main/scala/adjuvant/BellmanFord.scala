// Copyright (©) 2022 EPITA Research and Development Laboratory
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

package adjuvant

object BellmanFord {
  def shortestPath[V](vertices:Seq[V],
                      source:V,
                      edges:Seq[((V,V),Double)]):(Map[V,Double],Map[V,V]) = {
    import scala.Double.PositiveInfinity
    def recur(k:Int,d:Map[V,Double],p:Map[V,V]):(Map[V,Double],Map[V,V]) = {
      def dist(v:V) = d.getOrElse(v,PositiveInfinity)
      if (k == 0)
        (d,p)
      else {
        val (d2:Map[V,Double],p2:Map[V,V]) = edges.foldLeft((d,p)){
          case ((d1,p1),((u,v),w)) =>
            val newDist = dist(u) + w
            if (newDist < dist(v))
              (d1 + (v -> newDist),
                p1 + (v -> u))
            else
              (d1,p1)
        }
        recur(k-1,d2,p2)
      }
    }
    recur(vertices.size - 1,
          vertices.map{v => v -> (if (v == source) 0.0 else PositiveInfinity)}.toMap,
          Map(source -> source))
  }

  def reconstructPath[V](pred:Map[V,V], v:V):List[V] = {
    def recur(path:List[V]):List[V] = {
      path match {
        case Nil => throw new Error(s"empty path not supported")
        case v::_ if v == pred(v) => path.reverse
        case v::_ => pred(v):: path
      }
    }
    recur(List(v))
  }


  def Dijkstra[V](vertices: Seq[V],
                  source: V,
                  edges: Seq[((V, V), Double)]): (Map[V, Double], Map[V, V]) = {

    var current = source
    var distancemap: Map[V, Double] = vertices.zip(Array.fill(vertices.size)(Double.PositiveInfinity)).toMap
    var bestpredecessor: Map[V, V] = Map(source -> source)
    var visitedmap: Map[V, Boolean] = vertices.zip(Array.fill(vertices.size)(false)).toMap
    distancemap += (source -> 0)
    while (!visitedmap(current)) {
      //updating all distances, if we have found a new shorter path, then the distances and predecessors will be updated
      for (i <- edges) {
        if (i._1._1 == current) {
          if (distancemap(i._1._2) > i._2 + distancemap(current)) {
            distancemap += (i._1._2 -> (i._2 + distancemap(current)))
            bestpredecessor += (current -> i._1._2)
          }
        }
      }
      //finding the next vertex ( the one with the smallest distance that we haven't visited yet)
      var next = source
      var nextdist = Double.PositiveInfinity
      for (i <- distancemap) {
        if (i._2 < nextdist) {
          nextdist = i._2
          next = i._1
        }
      }
      //marking the current as visited, then continuing on the next, if we have visited all vertexes,
      // or at least all those that were accessible then the loop will stop
      visitedmap += (current -> true)
      current = next
    }
    (distancemap, bestpredecessor)
  }
}
