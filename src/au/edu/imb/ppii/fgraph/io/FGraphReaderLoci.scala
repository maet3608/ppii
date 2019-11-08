package au.edu.imb.ppii.fgraph.io

import java.io.File
import io.Source
import au.edu.imb.ppii.network.annotation.AnnotatorLoci
import au.edu.imb.ppii.fgraph.{FNode, FGraph}


/**
 * Creates a feature graph from a list of protein loci.
 * The graph is not hierarchical! Node levels are therefore not set
 * and roots is an empty list.
 * Each chromosome is represented as a node chain where each node
 * is a section of the chromosome.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 10/10/2010
 */

class FGraphReaderLoci(isCircular:Boolean=false) extends FGraphReader {
  def read(filepath:String) = {
    val graph = new FGraph(filepath)

    val lociGroups = Source.fromFile(new File(filepath)).getLines().
      map(AnnotatorLoci.locus(_)._2).toSeq.groupBy(l => l.split("_")(0))

    for((chr,loci) <- lociGroups) {
      val uniqueLoci = (Set()++loci).toSeq.sortBy(l => l)
      if(uniqueLoci.length > 1) {
        uniqueLoci.sliding(2,1).foreach(pair => graph.add(pair(0),pair(1)))
        if(isCircular) graph.add(uniqueLoci.head,uniqueLoci.last)
      }
      else
        graph.add(uniqueLoci.head)
    }
    setNodeIndices(graph)    
    graph
  }

}

/** Usage example */
object FGraphReaderLociExample extends App {
  val graph = (new FGraphReaderLoci().read("data/ecoli.loci"))
  for(node <- graph.toSeq.sortBy(_.index))
     printf("%s(%d) -> %s\n", node.label, node.index, graph.neighbors(node).mkString(","))

}