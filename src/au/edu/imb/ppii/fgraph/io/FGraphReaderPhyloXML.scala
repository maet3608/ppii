package au.edu.imb.ppii.fgraph.io

import scala.xml._
import au.edu.imb.ppii.fgraph._

/**
 * Reads feature graphs in PyhloXML format.
 * http://en.wikipedia.org/wiki/PhyloXML
 * http://www.phyloxml.org/
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 22/09/2010
 */

class FGraphReaderPhyloXML(nodeFilter:FNodeFilter = AcceptAllFilter) extends FGraphReader {
  
  def read(filepath:String) = {
    val xml = XML.loadFile(filepath)
    val graph = new FGraph(filepath)

    // recursively traverse tree and build graph
    def traverse(clade:Node, level:Int=0):FNode = {
      val id = (clade \ "taxonomy" \ "id").text
      val name = (clade \ "name").text
      require(id!="", "Taxonomic id for species %s is missing!\n%s\n" format (name,filepath))
      val parent = graph.add(id)
      parent.level = level
      for(subclade <- clade \ "clade") {
        val child = traverse(subclade, level+1)
        graph.add(parent.label, child.label)
      }
      parent
    }

    val isRooted = (xml \ "phylogeny" \ "@rooted").text == "true"
    val clades = xml \ "phylogeny" \ "clade"
    require(isRooted, "Tree is not rooted! "+filepath)
    require(clades.length==1, "Tree has more than one top level clade! "+filepath)

    graph.roots = Set(traverse(clades.head))
    filterNodes(graph)
    setNodeIndices(graph)
    graph
  }

  /** removes nodes according to node filter but ensures that the tree remains connected */
  private def filterNodes(graph:FGraph) {
    if(nodeFilter != AcceptAllFilter)
      graph.remove(AcceptNodesFilter(graph.ancestors(graph.filter(n => nodeFilter(n))), ""))
  }

}



/** Usage example */
object FGraphReaderPhyloXML extends App {
  // pretty printing
  def pretty(graph:FGraph, node:FNode, level:Int=0) {
    printf("%s(%d) %s\n", "-"*level,node.level,node.label)
    graph.children(node).foreach(pretty(graph,_,level+1))
  }

  val filepath = "f:/PPII/Data/PhyloProfiles/taxomonicTreeNCBI.xml"
  val graph = (new FGraphReaderPhyloXML()).read(filepath)

  pretty(graph, graph.roots.head)

  println("root is:    "+graph.roots.mkString("'",",","'"))
  println("terms:      "+graph.size)
  val levels = graph.map(_.level)
  println("max depth:  "+levels.max)
  println("mean depth: "+levels.sum/levels.size)
  val kidLens = graph.map(graph.children(_).size)
  println("max kids:   "+kidLens.max)
  println("mean kids:  "+kidLens.sum/kidLens.size)
  val dadLens = graph.map(graph.parents(_).size)
  println("max dads:   "+dadLens.max)
  println("mean dads:  "+dadLens.sum/dadLens.size)

}