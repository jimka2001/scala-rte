// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package adjuvant

import adjuvant.Adjuvant.sizedSet

import scala.collection.mutable

object BellmanFord {
  def shortestPath[V](vertices: Seq[V],
                      source: V,
                      edges: Seq[((V, V), Double)]): (Map[V, Double], Map[V, V]) = {
    import scala.Double.PositiveInfinity
    def recur(k: Int, d: Map[V, Double], p: Map[V, V]): (Map[V, Double], Map[V, V]) = {
      def dist(v: V) = d.getOrElse(v, PositiveInfinity)

      if (k == 0)
        (d, p)
      else {
        val (d2: Map[V, Double], p2: Map[V, V]) = edges.foldLeft((d, p)) {
          case ((d1, p1), ((u, v), w)) =>
            val newDist = dist(u) + w
            if (newDist < dist(v))
              (d1 + (v -> newDist),
                p1 + (v -> u))
            else
              (d1, p1)
        }
        recur(k - 1, d2, p2)
      }
    }

    recur(vertices.size - 1,
          vertices.map { v => v -> (if (v == source) 0.0 else PositiveInfinity) }.toMap,
          Map(source -> source))
  }

  // returns a path ordered from the source to v.
  def reconstructPath[V](pred: Map[V, V], v: V): List[V] = {
    def recur(path: List[V]): List[V] = {
      path match {
        case Nil => throw new Error(s"empty path not supported")
        case v :: _ if v == pred(v) => path
        case v :: _ => recur(pred(v) :: path)
      }
    }

    recur(List(v))
  }

  def generateVerticesSet(finals: Int, num: Int): Set[Int] = {
    val r = util.Random
    sizedSet[Int](finals, () => r.nextInt(num))
  }

  def dijkstra[V](vertices: Seq[V],
                  source: V,
                  edges: Seq[((V, V), Double)]): (Map[V, Double], Map[V, V]) = {
    implicit val myOrdering: Ordering[(V, Double)] = Ordering.by { case (_, d) => d }
    //create a map of vertex to edges to reduce the cost when treating every accessible vertex
    val edgess: Map[V, Seq[((V, V), Double)]] = edges.groupBy(x => x._1._1).withDefaultValue(Seq())
    //create priority queue, distance map and best predecessor map,
    // adding a source-> infinity item to mark the end of the queue
    val pq1 = new mutable.PriorityQueue[(V, Double)]().reverse.addOne((source -> Double.PositiveInfinity))
    pq1.enqueue(source -> 0)
    var distancemap: Map[V, Double] = vertices.zip(Array.fill(vertices.size)(Double.PositiveInfinity)).toMap
    var bestpredecessor: Map[V, V] = Map(source -> source)
    //loop on all the vertices that will get treated, meeting with a (x-> infinity) key will mean that the end of the
    // queue has been reached
    while (pq1.head._2 != Double.PositiveInfinity) {
      val current = pq1.dequeue()
      //not treat a vertex if we have already treated it with a better path
      if (current._2 < distancemap(current._1)) {
        distancemap += current
        for (i <- edgess(current._1)) {
          //change values for each edge if the new value is better than the old one
          if (i._1._1 == current._1) {
            if (distancemap(i._1._2) > i._2 + distancemap(current._1)) {
              pq1.enqueue((i._1._2, i._2 + distancemap(current._1)))
              bestpredecessor += (i._1._2 -> current._1)
            }
          }
        }
      }
    }
    (distancemap, bestpredecessor)
  }
}
