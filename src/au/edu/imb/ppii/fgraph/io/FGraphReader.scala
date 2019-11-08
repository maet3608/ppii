package au.edu.imb.ppii.fgraph.io

import au.edu.imb.ppii.fgraph.{FGraph}

/**
 * Abstract base class for readers of feature graphs
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 22/09/2010
 */

abstract class FGraphReader {

  /** Override: reads a feature graph from the given file */
  def read(filepath:String):FGraph


  /** sets all node indices to be in [0..n] with n = graph size.
   * Required when creating a feature vector from a node set */
  protected def setNodeIndices(graph:FGraph):Unit = {
    graph.zipWithIndex.foreach{case (node,i) => node.index = i}
  }
}