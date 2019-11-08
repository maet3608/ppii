package au.edu.imb.ppii.go

import au.edu.imb.ppii.utils.Annotations
import au.edu.imb.ppii.go.Ontology._
import au.edu.imb.ppii.go.Term._

/**
 * Describes a term within the ontology. Which is essentially a node with
 * some annotations, and lists of links to its parent and child nodes.
 */
class Term(val annotations:Annotations) {
  var level:Int = -1                                 // hierarchy level
  val index:Int = createIndex                        // unique index
  private var _parentRelations = List[Relation]()    // edges to parents
  private var _childRelations  = List[Relation]()    // edges from children
      
  /** Returns the first value of the Go term annotation for the given tag */
  def apply(tag:String) = annotations(tag)
  /** Returns first annotation for given tag or an empty string if tag does not exist */
  def get(tag:String) = try{ annotations(tag) } catch { case _ => "" }
  /** Returns all values for the given tag. Fails if tag does not exist. */
  def values(tag:String) = annotations.values(tag)
  /** Returns all values for the given tag. Returns empty list if tag does not exist. */
  def valuesOrElse(tag:String) = annotations.valuesOrElse(tag)
  /** Convenience function that tests if the term annotation contains the given tag */
  def contains(tag:String) = annotations.contains(tag)
  /** Convenience function that returns the GO id of the term */
  def id = annotations(GO_ID)
  /** Convenience function that returns the name of the term or an empty string */
  def name = get(GO_NAME)
  
  /** Returns an Iterator over all parent relations */
  def parentRelations:Iterable[Relation] = _parentRelations
  /** Returns an Iterator over all child relations */
  def childRelations:Iterable[Relation] = _childRelations
  /** Returns a sequence of parent terms */
  def parents:Seq[Term] = _parentRelations.map(_.parent)
  /** Returns a sequence of child terms */
  def children:Seq[Term] = _childRelations.map(_.child)
  
  /** Adds a relation from this to parent and updates relation list in child nodes*/
  private def addRelation(parent:Term, is_a:Boolean) = {
    val relation = Relation(this, parent, is_a)
    this._parentRelations  ::= relation
    parent._childRelations ::= relation
    relation
  }  
  
  /** Creates a is_a edge between this and the given parent term */
  def addIsA(parent:Term) = addRelation(parent, true)
  /** Creates a part_of edge between this and the given parent term */
  def addPartOf(parent:Term) = addRelation(parent, false)
  
  /** Tests if the given term is in a isA relationship to this term */
  def isA(parent:Term) = parentRelations.exists(r => r.parent == parent && r.isA)
  
  /** Tests if the given term is in a partOf relationship to this term */
  def partOf(parent:Term) = parentRelations.exists(r => r.parent == parent && !r.isA)
  
  /** Two terms are equal if their GO ids are the same */
  override def equals(other:Any) = other match {
    case that:Term => (this eq that) || (that.id == this.id)
    case _ => false
  }
  
  /** Hashcode based on GO ID */
  override def hashCode = id.hashCode
  
  /** Returns string with term id, level and ids of parent nodes */
  override def toString = annotations("id")
}

/**
 * Only used to create unique term indices
 */
object Term {
  // Instance counter
  private var counter = -1

  /** Running number to map a term instance to an index */
  def createIndex = { counter += 1; counter}
}