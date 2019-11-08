package au.edu.imb.ppii.fgraph

import collection.mutable.{HashSet, HashMap}

/**
 * Graph utils
 * Version: 1.00
 * Author : Stefan Maetschke
 */

/**
 * Adaptation of code from:
 * http://en.literateprograms.org/Dijkstra%27s_algorithm_%28Scala%29#chunk%20use:dijkstra
 */
object Utils {

  /** Modified Dijkstra's algorithm to find the shortest path between a start node and
   a given set of end nodes. Returns the set of nodes on the shortest path without the start node */
  def shortestPathSingle(graph:FGraph, start: FNode, ends: Set[FNode]): Set[FNode] = {
    var (end, pred, endReached) = dijkstraSingle(graph, start, ends)
    var path = Set[FNode]()
    if(endReached)
      while(end != start) {
        path += end
        end = pred(end)
      }
    path
  }

  def shortestPathAll(graph:FGraph, start: FNode, ends: Set[FNode]): Set[FNode] = {
    var (end, preds, endReached) = dijkstraAll(graph, start, ends)
    var path = Set[FNode](start)
    def backtrack(node:FNode):Unit = {
      if(!path.contains(node)) {
        path += node
        preds(node).foreach(backtrack)
      }
    }
    if(endReached)
      backtrack(end)

    path
  }

  /** Modified Dijkstra's algorithm to find ALL shortest paths
   * between a start node and a given set of end nodes. */
  protected def dijkstraAll(graph:FGraph, start: FNode, ends: Set[FNode]) = {
    val dist = new HashMap[FNode, Double]      // distances
    val Q = new HashSet[FNode]                   // priority queue
    val settled = new HashSet[FNode]             // settled vertices
    val preds = new HashMap[FNode,List[FNode]]   // predecessors

    def nearest(Q: HashSet[FNode]):FNode =
      Q.reduceLeft((u, v) => if(dist(u) <= dist(v)) u else v)

    dist += start -> 0.0
    Q += start
    var u:FNode = null
    var endReached = false

    while(!Q.isEmpty && !endReached) {
      u = nearest(Q)
      Q -= u
      settled += u
      endReached = ends.contains(u)
      if(!endReached)
        for(e <- graph.edges(u); val v = graph.opposite(u, e) if !settled.contains(v)) {
          val newDist = dist(u) + e.weight
          if(!dist.isDefinedAt(v) || newDist < dist(v)) {
            dist += v -> newDist
            preds += v -> List(u)
            Q += v
          }
          else if(newDist == dist(v))
            preds += v -> (u::preds(v))
        }
    }
    (u, preds, endReached)
  }


  /** Modified Dijkstra's algorithm to find a SINGLE shortest path
   * between a start node and a given set of end nodes. */
  protected def dijkstraSingle(graph:FGraph, start: FNode, ends: Set[FNode]) = {
    val dist = new HashMap[FNode, Double]       // distances
    var Q = new HashSet[FNode]            // priority queue
    val settled = new HashSet[FNode]            // settled vertices
    val pred = new HashMap[FNode,FNode]         // predecessors

    def nearest(Q: HashSet[FNode]):FNode =
      Q.reduceLeft((u, v) => if(dist(u) <= dist(v)) u else v)

    dist += start -> 0.0
    Q += start
    var u:FNode = null
    var endReached = false
  
    while(!Q.isEmpty && !endReached) {
      u = nearest(Q)
      Q -= u 
      settled += u
      endReached = ends.contains(u)
      if(!endReached)
        for(e <- graph.edges(u); val v = graph.opposite(u, e) if !settled.contains(v)) {
          val vNewDist = dist(u) + e.weight
          if(!dist.isDefinedAt(v) || vNewDist < dist(v)) {
            dist += v -> vNewDist
            pred += v -> u 
            Q += v
          }
        }
    }
    (u, pred, endReached)
  }
}


/** Usage example */
object UtilsExample extends App {
  val g = new FGraph("Example")
  g.add("A","B")
  g.add("B","C")
  g.add("B","D")
  g.add("C","E")
  g.add("D","E")
  g.add("X")
  val path = Utils.shortestPathAll(g,g("A"),Set(g("E")))
  println(path)

}