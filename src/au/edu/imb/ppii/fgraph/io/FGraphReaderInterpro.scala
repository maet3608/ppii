package au.edu.imb.ppii.fgraph.io

import java.io.File
import io.Source
import au.edu.imb.ppii.fgraph.{FNodeFilter, AcceptAllFilter, FNode, FGraph}

/**
 * Creates a feature graph from a tree of InterPro domains.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 10/10/2010
 */

class FGraphReaderInterpro(nodeFilter:FNodeFilter = AcceptAllFilter) extends FGraphReader {
  private val extract = """^(-*)(IPR[0-9]+)::(.+)::""".r

  def read(filepath:String) = {
    val graph = new FGraph(filepath)
    val lines = Source.fromFile(new File(filepath)).getLines()
    //graph.add("root",0)

    for(line <- lines) {
      val extract(dashes,id,comment) = line
      val level = dashes.length/2
      graph.add(id,level)
    }

    graph.toSeq.filter(!nodeFilter(_)).foreach(n => graph.remove(n.label))

    setNodeIndices(graph)

    //graph.roots = Set(graph("root"))
    graph
  }

}

/** Usage example */
object FGraphReaderInterproExample extends App {
  val graph = (new FGraphReaderInterpro().read("f:/PPII/Data/Domain/InterPro/ParentChildTreeFile.txt"))
  for(node <- graph.toSeq.sortBy(_.index))
     printf("%s(%d) -> %s\n", node.label, node.index, graph.children(node).mkString(","))

}