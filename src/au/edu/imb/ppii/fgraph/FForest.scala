package au.edu.imb.ppii.fgraph

/**
 * A forest of feature graphs.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 13/10/2010
 */

class FForest extends  {
  var graphs = List[FGraph]()

  /** Number of nodes within the entire forest */
  def size = graphs.map(_ size).sum

}
