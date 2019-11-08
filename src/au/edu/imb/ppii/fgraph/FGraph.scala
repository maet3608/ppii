package au.edu.imb.ppii.fgraph

import edu.uci.ics.jung.algorithms.layout._
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.UndirectedSparseGraph
import edu.uci.ics.jung.visualization.VisualizationViewer
import io.FGraphReaderNPG
import javax.swing.JFrame
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import scala.collection.JavaConversions._

/**
 * FGraph = Feature graph. Represents the relations between symbolic feature labels
 * as an undirected graph. The graph can be rooted.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 21/09/2010
 */



/** Feature node with its unique index and label */
class FNode(var index:Int, val label:String) {
  var level = -1  // hierarchy level in rooted graph. Might not be set!

  /** Two nodes are equal if their references or their labels are the same */
  override def equals(other:Any) = other match {
    case that:FNode => (this eq that) || this.label == that.label
    case _ => false
  }

  /** Hashcode based on label */
  override def hashCode = label.hashCode

  /** String representation that returns label */
  override def toString = label
}

/** Node factory */
object FNode {
  // Instance counter
  var counter = -1
  /** constructor */
  def apply(label:String) = {counter += 1; new FNode(counter, label)}
}


/** Edge between features */
class FEdge(val weight:Double)

/** Edge factory */
object FEdge {
  /** constructor */
  def apply(weight:Double = 1.0) = new FEdge(weight)
}


/**
 * Wrapper around a jung.Graph that contains a map of its nodes.
 */
class FGraph(val name:String) extends Iterable[FNode] {
  type LabelSet = Set[String]
  type NodeSet = Set[FNode]

  private var nodeMap = Map[String,FNode]()
  private val graph:Graph[FNode,FEdge] = new UndirectedSparseGraph[FNode,FEdge]

  var roots = Set[FNode]() // root nodes of the graph - provided it is rooted


  /** Getter for a node with the given label. Throws exception if it doesn't exist */
  def apply(label:String) = nodeMap(label)
  /** Getter for a node with the given label as Option */
  def get(label:String) = nodeMap.get(label)
  /** tests if graph contains the given label */
  def contains(label:String):Boolean = nodeMap.contains(label)
  /** tests if graph contains the given node */
  def contains(node:FNode):Boolean = contains(node.label)

  /** creates a node with the given label and adds it to the graph
   * provided it does not exist already */
  def add(label:String):FNode = nodeMap.get(label) match {
    case None => val node = FNode(label); graph.addVertex(node); nodeMap += label -> node; node
    case Some(node) => node
  }

  /** creates a node with the given label and level and adds it to the graph
   * provided it does not exist already */
  def add(label:String, level:Int):FNode = {
    val node = add(label)
    node.level = level
    node
  } 

  /** adds a weighted edge to the graph provided it doesn't exist already */
  def add(weight:Double, label1:String, label2:String):FGraph = {
    graph.addEdge(FEdge(weight), add(label1), add(label2)); this }

  /** adds an edge to the graph - provided it doesn't exist already. edge weight is 1 */
  def add(label1:String, label2:String):FGraph =
    add(1.0, label1, label2)

  /** removes the node with the given label from the graph */
  def remove(label:String):Unit = {
    graph.removeVertex(this(label))
    nodeMap -= label
  }

  /** removes the given node (and its links) from the graph */
  def remove(node:FNode):Unit =
    remove(node.label)

  /** removes nodes according to the given node filter */
  def remove(nodeFilter:FNodeFilter):Unit =
    this.toList.foreach(node => if(!nodeFilter(node)) remove(node))

  /** creates a viewer for the graph */
  def viewer = {
    val vv = new VisualizationViewer(new KKLayout(graph))
    //val vv = new VisualizationViewer(new CircleLayout(graph))
    vv.getRenderContext().setVertexLabelTransformer(new ToStringLabeller())
    vv
  }

  /** all edges within the graph */
  def edges:Traversable[FEdge] = graph.getEdges
  /** all edges for the given node */
  def edges(node:FNode):Traversable[FEdge] = graph.getIncidentEdges(node)
  /** all edges as tuples of nodes */
  def edgeNodes:Traversable[(FNode,FNode)] =
    for(e <- graph.getEdges; val p = graph.getEndpoints(e)) yield (p.getFirst, p.getSecond)
  /** the node opposite the given node on this edge */
  def opposite(node:FNode, edge:FEdge):FNode = graph.getOpposite(node, edge)
  /** all direct neighbours of the given node */
  def neighbors(node:FNode):Traversable[FNode] = graph.getNeighbors(node)
  /** parents of a node. Requires rooted graph with level info for nodes */
  def parents(node:FNode):Set[FNode] = neighbors(node).filter(_.level < node.level).toSet
  /** children of a node. Requires rooted graph with level info for nodes */
  def children(node:FNode):Set[FNode] = neighbors(node).filter(_.level > node.level).toSet
  /** set of ancestors for the given label including the label node */
  def ancestors(label:String):Set[FNode] = ancestors(nodeMap(label))
  /** set of ancestors of the given node including the given node */
  def ancestors(node:FNode):Set[FNode] = Set(node)++parents(node).flatMap(ancestors)
  /** union of ancestors to all given nodes including the nodes */
  def ancestors(nodes:Iterable[FNode]):Set[FNode] =  nodes.map(ancestors).reduceLeft(_ | _)
  /** true: if there is an edge between the two nodes */
  def isLinked(node1:FNode, node2:FNode) = neighbors(node1).find(_  == node2).isDefined

  /** Number of nodes within the graph */
  override def size = nodeMap.size

  /** Iterator over all nodes */
  def iterator = nodeMap.valuesIterator

  override def toString = nodeMap.valuesIterator.toSeq.sortBy(_.label).map(n =>
     "%s -> %s\n" format(n.label, neighbors(n).mkString(","))).mkString
}


/**
 * Graph factory
 */
object FGraph {
  def apply(name:String) = new FGraph(name)
}



/**
 * Usage example.
 */
object FGraphExample  {

  def createGraph = {
    FGraph("Example")
    .add("A","B")
    .add("B","C")
    .add("C","A")
    .add("A","A")
  }

  def loadGraph = {
    val filepath = "f:/PPII/Data/PDB/Interactions/ngrams_test.txt"
    new FGraphReaderNPG().read(filepath)
  }

  def main(args:Array[String]) = {
    val frame = new JFrame()
    val graph = createGraph
    frame.getContentPane().add(graph.viewer)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)
  }

}