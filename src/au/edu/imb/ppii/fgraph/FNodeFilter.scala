package au.edu.imb.ppii.fgraph

/**
 * Classes to implement acceptance filters for nodes within a graph.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 09/10/2010
 */

/** Abstract base class for all node filters */
abstract class FNodeFilter(val name:String) {

  /** true = accept node, false = reject node */
  def apply(node:FNode):Boolean

  /** filtering by label instead of node. Not very efficient */
  def apply(label:String):Boolean = apply(new FNode(0,label))

}


/** Node filter that filters accepts all nodes */
object AcceptAllFilter extends FNodeFilter("AcceptAll") {
   def apply(node:FNode) = true
}


/** Node filter that filters all nodes that are not within the given node set */
class AcceptNodesFilter(nodeSet:Set[FNode], name:String) extends FNodeFilter(name) {
  def apply(node:FNode) = nodeSet.contains(node)
}

object AcceptNodesFilter {
  def apply(nodeSet:Set[FNode], name:String = "AcceptNodes")
    = new AcceptNodesFilter(nodeSet,name)
}


/** Node filter that filters all nodes that are not within the given label set */
class AcceptLabelsFilter(labelSet:Set[String], name:String) extends FNodeFilter(name) {
  def apply(node:FNode) = labelSet.contains(node.label)
}

object AcceptLabelsFilter {
  def apply(labelSet:Set[String], name:String = "AcceptLabels")
    = new AcceptLabelsFilter(labelSet,name)
}