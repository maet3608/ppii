package au.edu.imb.ppii.fgraph

import au.edu.imb.ml.sampleset.{ASample, SparseSample}

/**
 * Generator for feature vectors.
 * Version: 1.00
 * Author : Stefan Maetschke
 */

object FGenerator {

  /** converts a node set into a sparse sample */
  def toSample(graph:FGraph, nodes:Set[FNode], classID:Int) = {
    val l = graph.size
    SparseSample(l,toVector(nodes,0,l),classID)
  }

  /**converts a node set over several feature graphs into a sparse sample.
    requires that node indices are incrementing over the graphs! */
  def toSample(graphs:List[FGraph],nodes:Set[FNode], classID:Int) = {
    val l:Int = graphs.map(_.size).sum
    val values = nodes.toSeq.sortBy(_.index).map(n => (n.index, 1.0))
    SparseSample(l,values,classID)
  }

  /** converts node set into sparse feature vector.
    start = start index, length = max. length of vector */
  private def toVector(nodes:Set[FNode], start:Int, length:Int) = {
    def check(idx:Int) =
      require(idx < length && idx > -1, "Invalid index %d [0,%d]" format (idx,length))
    def index(node:FNode) =
      { check(node.index); node.index }
    nodes.toSeq.sortBy(_.index).map(n => (start+index(n),1.0))
  }

  /** returns the graph nodes corresponding to the given labels */
  private def nodes(graph:FGraph, labels:Set[String]) =
    labels.flatMap(graph.get(_))

  /** same as nodes() but returns root node if no other nodes are found */
  private def rnodes(graph:FGraph, labels:Set[String]) = labels.flatMap(graph.get(_)) match {
      case nodes if nodes.isEmpty => graph.roots  // no labels => use roots
      case nodes => nodes
    }

  /** helper to print out a set of nodes in sorted order */
  private def nodestr(nodes:Iterable[FNode]) = nodes.toSeq.sortBy(_.level).mkString(",")

  /** all nodes up to the lowest common ancestor */
  def ulca(graph:FGraph, labels1:Set[String], labels2:Set[String]):Set[FNode] = {
    val ancestors1 = graph.ancestors(rnodes(graph,labels1))
    val ancestors2 = graph.ancestors(rnodes(graph,labels2))
    val commonAncestors = ancestors1 & ancestors2
    if(commonAncestors.isEmpty) return al(graph,labels1,labels2)
    val maxLevel = commonAncestors.map(_.level).max
    (ancestors1 | ancestors2).filter(_.level >= maxLevel)
  }

  /** union of all ancestors without the lowest common ancestors */
  def wlca(graph:FGraph, labels1:Set[String], labels2:Set[String]) = {
    val ancestors1 = graph.ancestors(rnodes(graph,labels1))
    val ancestors2 = graph.ancestors(rnodes(graph,labels2))
    val commonAncestors = ancestors1 & ancestors2
   (ancestors1 | ancestors2) -- commonAncestors
  }

  /** lowest common ancestors only */
  def olca(graph:FGraph, labels1:Set[String], labels2:Set[String]):Set[FNode] = {
    val ancestors1 = graph.ancestors(rnodes(graph,labels1))
    val ancestors2 = graph.ancestors(rnodes(graph,labels2))
    val commonAncestors = ancestors1 & ancestors2
    if(commonAncestors.isEmpty) return al(graph,labels1,labels2)
    val maxLevel = commonAncestors.map(_.level).max
   (ancestors1 | ancestors2).filter(_.level == maxLevel)
  }

  /** lowest common ancestors and labeled nodes */
  def lca(graph:FGraph, labels1:Set[String], labels2:Set[String]) = {
    val nodes1 = rnodes(graph,labels1)
    val nodes2 = rnodes(graph,labels2)
    olca(graph,labels1,labels2) | nodes1 | nodes2
  }

  /** union of both labels sets */
  def al(graph:FGraph, labels1:Set[String], labels2:Set[String]) = {
    nodes(graph,labels1) | nodes(graph,labels2)
  }

  /** only labels common to both label sets */
  def ac(graph:FGraph, labels1:Set[String], labels2:Set[String]) = {
    rnodes(graph,labels1) & rnodes(graph,labels2)
  }

  /** union of both labels sets and their ancestors */
  def aa(graph:FGraph, labels1:Set[String], labels2:Set[String]) = {
    graph.ancestors(rnodes(graph,labels1)) | graph.ancestors(rnodes(graph,labels2))
  }

  /** intersection of both labels sets and their ancestors */
  def aca(graph:FGraph, labels1:Set[String], labels2:Set[String]) = {
    graph.ancestors(rnodes(graph,labels1)) & graph.ancestors(rnodes(graph,labels2))
  }

    
  /** Shortest path, only a single path between two labels  */
  def sps(graph:FGraph, labels1:Set[String], labels2:Set[String]) = {
    val all = rnodes(graph,labels1) ++ rnodes(graph,labels2)
    var path = all
    for(node <- all)
      path = path ++ Utils.shortestPathSingle(graph, node, path - node)
    path
  }

  /** Shortest path, all paths between two labels  */
  def spa(graph:FGraph, labels1:Set[String], labels2:Set[String]):Set[FNode] = {
    val all = rnodes(graph,labels1) ++ rnodes(graph,labels2)
    var path = all
    for(node <- all)
      path = path ++ Utils.shortestPathAll(graph, node, path - node)
    path
  }
  
}