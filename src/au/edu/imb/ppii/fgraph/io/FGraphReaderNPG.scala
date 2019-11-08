package au.edu.imb.ppii.fgraph.io

import java.io.File
import io.Source
import au.edu.imb.ppii.fgraph.{FNodeFilter, AcceptAllFilter, FNode, FGraph}

/**
 * Creates a feature graph from a list of interacting pairs of n-grams.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 10/10/2010
 */

class FGraphReaderNPG(nodeFilter:FNodeFilter = AcceptAllFilter) extends FGraphReader {

  def read(filepath:String) = {
    val graph = new FGraph(filepath)
    val lines = Source.fromFile(new File(filepath)).getLines()

    for(line <- lines) {
      val elems = line.split("\t")
      graph.add(elems(0),elems(1))
    }

    if(nodeFilter != AcceptAllFilter)
      graph.toSeq.filter(!nodeFilter(_)).foreach(n => graph.remove(n.label))
    setNodeIndices(graph)
    graph
  }

}

/** Usage example */
object FGraphReaderNPGExample extends App {
  val graph = (new FGraphReaderNPG().read("f:/PPII/Data/PDB/Interactions/ngrams50.txt"))
  for(node <- graph.toSeq.sortBy(graph.neighbors(_).size))
     printf("%s -> %s\n", node.label, graph.neighbors(node).mkString(","))
  println("Nodes :"+graph.size)
}