package au.edu.imb.ppii.predictor

import scala.math.exp
import au.edu.imb.ppii.go._
import scala.collection.mutable.ArrayBuffer
import au.edu.imb.ppii.predictor.Node._
import au.edu.imb.ppii.network.Interaction

/** Weighted link between nodes */
case class Link(val child:Node, val parent:Node, var weight:Double)


/** Node that wraps and enriches a GO term and has an activation function f */
case class Node(val term:Term) {
  var level  = -1
  var index  = createIndex
  val id     = term.id
  val name   = term.name
  private var _parentLinks = ArrayBuffer[Link]()
  private var _childLinks  = ArrayBuffer[Link]()

  var bweight = 1.0   // bias weight
  var aweight = 1.0   // activation weight

  /** Returns the node activation for an interaction */
  def activation(interaction:Interaction) = {
    var act = 0.0
    if(interaction.protein1.goids.contains(term.id)) act += 1.0
    if(interaction.protein2.goids.contains(term.id)) act += 1.0
    act
  }

  /** Returns the node output for an interaction */
  def output(interaction:Interaction):Double = {
    1/(1+exp( -bweight - aweight*activation(interaction) -
        childLinks.map(l => l.weight*l.child.output(interaction)).sum ))
  }

  /** Adds a link */
  def -> (parent:Node) = {
    val link = Link(this, parent, 1.0)
    this._parentLinks += link
    parent._childLinks += link
    link
  }

  /** Two nodes are equal if their GO terms are the same */
  override def equals(other:Any) = other match {
    case that:Node => (this eq that) || (that.term == this.term)
    case _ => false
  }
  
  /** Hash code based on GO term */
  override def hashCode = term.hashCode
  
  /** Returns a sequence of parent nodes */
  def parents:Seq[Node] = _parentLinks.map(_.parent)
  /** Returns a sequence of child nodes */
  def children:Seq[Node] = _childLinks.map(_.child)
  /** Returns the sequence of links to child nodes */
  def childLinks:Seq[Link] = _childLinks
}

/**
 * Only used to create unique node indices
 */
object Node {
  // Instance counter
  private var counter = -1

  /** Running number to map a node instance to an index */
  def createIndex = { counter += 1; counter}
}
