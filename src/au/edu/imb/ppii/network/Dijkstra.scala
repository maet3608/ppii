package au.edu.imb.ppii.network

/**
 * Dijkstra's algorithm to find the shortest path between two nodes within a network.
 * Version: 1.00
 * Author : Stefan Maetschke
 */

import scala.collection.mutable.{HashSet, HashMap}


/**
 * Adaptation of code from:
 * http://en.literateprograms.org/Dijkstra%27s_algorithm_%28Scala%29#chunk%20use:dijkstra
 */
object Dijkstra {
  
  def shortestPath(start: Protein, end: Protein): List[Pair[Protein, Double]] = {
    val (dist, pred, endReached) = dijkstra(start, end)
 
    // build path from end based on predecessors
    var path: List[Pair[Protein, Double]] = Nil 
    if (endReached) {
      var v = end 
      while(v != start) {
        path = Pair(v, dist(v)) :: path 
        v = pred( v)
      }
    }
    path
  }
  
  def dijkstra(source: Protein, end: Protein) = {  
    val dist = new HashMap[Protein, Double]()     // distances
    val Q = new HashSet[Protein]                  // priority queue
    val Settled = new HashSet[Protein]            // settled vertices
    val pred = new HashMap[Protein, Protein]      // predecessors
   
    def minimumDistVertex(Q: HashSet[Protein]): Protein = {         
      val iterator = Q.iterator 
      val w = iterator.next           // first element, because Q is not empty
      iterator.foldLeft(w) {(u, v) => if(dist(u) <= dist(v)) u else v}
    }
    
    dist += ((source,0.0))
    Q += source 
    var endReached = false 
  
    while(!Q.isEmpty && !endReached) {
      val u = minimumDistVertex(Q)
      Q -= u 
      Settled += u 
 
      if(end != null) endReached = (u == end)
        // update neighbours distances and add updated ones to Q
        if (!endReached)
          for(v <-  u.neighbors if !Settled.contains(v)) {
            val vNewDist = dist(u) + 1.0
            if ( !dist.isDefinedAt(v) || vNewDist < dist(v)) {
              dist += v -> vNewDist 
              pred += v -> u 
              Q += v 
            }
          }
    }
    (dist, pred, endReached)
  }
}
