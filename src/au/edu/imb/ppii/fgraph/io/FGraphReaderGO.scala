package au.edu.imb.ppii.fgraph.io

import au.edu.imb.ppii.fgraph.FGraph
import au.edu.imb.ppii.go.Ontology._
import au.edu.imb.ppii.go.{Ontology, Term, TermFilter, OboParser}

/**
 * Reads feature graphs from a GO ontology in OBO format.
 * Line endings are expected to be in Windows format (\n\r)!
 * http://www.geneontology.org/GO.format.obo-1_2.shtml
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 22/09/2010
 */

class FGraphReaderGO(termFilter:TermFilter) extends FGraphReader {
  def read(filepath:String) = {
    val graph = new FGraph(filepath)
    val ontology = new Ontology(OboParser(filepath),termFilter)

    ontology.foreach(term => term.parents.foreach(parent => graph.add(term.id,parent.id)))
    ontology.foreach(term => graph(term.id).level = term.level)
    graph.roots = ontology.roots.map(term => graph(term.id)).toSet
    setNodeIndices(graph)
    graph
  }

}

/** Usage example */
object FGraphReaderGOExample extends App {
  val graph = (new FGraphReaderGO(cellularComponent)).read("data/go.obo")
  for(node <- graph.toSeq.sortBy(_.label))
     printf("%s(%d) -> %s\n", node.label, node.level, graph.parents(node).mkString(","))
  println("roots: "+graph.roots.mkString(","))
}