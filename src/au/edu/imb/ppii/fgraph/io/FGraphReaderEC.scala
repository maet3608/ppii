package au.edu.imb.ppii.fgraph.io

import java.io.File
import io.Source
import au.edu.imb.ppii.fgraph.{FNodeFilter, AcceptAllFilter, FNode, FGraph}

/**
 * Creates a feature graph from a list of EC numbers.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 10/10/2010
 */

class FGraphReaderEC(nodeFilter:FNodeFilter = AcceptAllFilter) extends FGraphReader {
  
  def read(filepath:String) = {
    val graph = new FGraph(filepath)
    val lines = Source.fromFile(new File(filepath)).getLines()

    def add(ec:String, level:Int):String = {
      if(!graph.contains(ec)) {
        graph.add(ec, level)
        if(level >= 0)
          graph.add(ec, add(parent(ec,level),level-1))
      }
      ec
    }

    def parent(ec:String, level:Int) = {
      val ecs = ec.split('.')
      ecs(level) = "-"
      ecs.mkString(".")
    }

    lines.map(_.trim).filter(nodeFilter(_)).foreach(add(_,3))
    setNodeIndices(graph)

    graph.roots = Set(graph("-.-.-.-"))
    graph
  }

}

/** Usage example */
object FGraphReaderECExample extends App {
  val graph = (new FGraphReaderEC().read("f:/PPII/Data/ENZYME/ecnumbers.txt"))
  for(node <- graph.toSeq.sortBy(_.label))
     printf("%s -> %s\n", node.label, graph.parents(node).mkString(","))
  println("Nodes :"+graph.size)
}