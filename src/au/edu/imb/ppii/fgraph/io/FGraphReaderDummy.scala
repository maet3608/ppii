package au.edu.imb.ppii.fgraph.io

import au.edu.imb.ppii.fgraph.{FGraph}

/**
 * Creates a feature graph from a list of given labels. This not an actual
 * reader. The labels are provided during class construction and are not
 * read from a file.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 10/10/2010
 */

class FGraphReaderDummy(labels:Iterable[String]) extends FGraphReader {

  /** Filepath is not used! */
  def read(filepath:String = null) = {
    val graph = new FGraph(filepath)
    labels.foreach(graph.add(_))
    setNodeIndices(graph)
    graph
  }

}

