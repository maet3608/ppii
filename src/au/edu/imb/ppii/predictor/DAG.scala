package au.edu.imb.ppii.predictor

import au.edu.imb.ppii.go._
import au.edu.imb.ppii.utils.Annotations

/**
 * Abstract base class fro directed acyclic graphs (DAG).
 * Derived class has to construct the DAG and define the roots method.
 */
abstract class DAG[N<:Node] extends Iterable[N]  {
  protected var nodeMap = Map[String,N]()

  /** Recursively determines and sets hierarchy levels of terms */
  protected def setLevel[N<:Node](node:N, level:Int):Unit = {
    node.level = node.level max level
    node.children.foreach(setLevel(_, level+1))
  }  

  /** root nodes of the DAG */
  def roots:List[N]
  
  /** Returns the set of ancestors of the given node including the given node */  
  def ancestors[N<:Node](node:N):Set[N] = 
     Set()+node++node.parents.flatMap(n => ancestors(n.asInstanceOf[N]))

  /** Returns the union of ancestors common to all given nodes including the nodes */  
  def ancestors[N<:Node](nodes:Iterable[N]):Set[N] =
    nodes.map(n => ancestors(n)).reduceLeft(_ | _)
   
  /** Returns all lowest common ancestors of the two given node lists.
    * For each node list the union of ancestors is calculated and then the
    * lowest common ancestors for the two ancestor sets are identified.
    */
  def LCAs[N<:Node](nodes1:Iterable[N], nodes2:Iterable[N]):Set[N] = {
    val common = ancestors(nodes1) & ancestors(nodes2)
    val maxLevel = common.map(_.level).max
    common.filter(_.level == maxLevel)
  }   
    
  /** Tests whether the DAG contains a node with the given GO ID */
  def contains(id:String) = nodeMap.contains(id)

  /** Returns the node with the given GO id if it exists */
  def get(id:String) = nodeMap.get(id)

  /** Returns the node with the given GO id. Throws an exception if
   * node with the given id does not exist */
  def apply(id:String) = nodeMap(id)
  
  /** Iterator over all nodes */
  def iterator:Iterator[N] = nodeMap.valuesIterator

  /** Number of nodes in the dag */
  def length = nodeMap.size
}


/**
 * A DAG derived from the GO Ontology DAG.
 * Usually the GoDAG and the Ontology will be identical with respect to
 * topology but this does not need to be the case. The DAG can represent
 * a modified Ontology, e.g. by extracting a subset of nodes.
 * @param ontology Gene Ontology the DAG is derived from.
 */
class GoDAG(val ontology:Ontology) extends DAG[Node] {
  def roots = ontology.roots.flatMap(t => nodeMap.get(t.id))

  ontology.foreach(term => nodeMap += term.id -> Node(term))
  ontology.foreach(term => term.parents.foreach(parent => this(term.id)->this(parent.id)))

  roots.foreach(setLevel(_,0))
}


/**
 * Just a usage example.
 */
object DAGExample {
  def main(args: Array[String]) { 
    println("started...")
    //val ontology = new Ontology(OboParser("test/go_test2.obo"))
    val ontology = new Ontology(OboParser("data/go.obo"), Ontology.cellularComponent)
    val dag = new GoDAG(ontology)

    println("roots:      "+dag.roots.mkString(", "))
    println("nodes:      "+dag.size)
    println("max depth:  "+dag.map(_.level).max)
    println("max kids:   "+dag.map(_.children.length).max)
    println("max dads:   "+dag.map(_.parents.length).max)

    for(node <- dag.toSeq.sortBy(_.id))
       printf("%s(%d) -> %s\n", node.id, node.level, node.parents.mkString(","))

    println("finished")
  }
}


