package au.edu.imb.ppii.go

/**
 * Describes a direct edge from child to parent within the Go DAG.
 * The attribute is_a determines if the edge represents a is_a or a part_of
 * relationship.
 */
case class Relation(val child:Term, val parent:Term, val isA:Boolean) {
  
  /** Two relations are equal when their children and parents are equal */
  override def equals(other:Any) = other match {
    case that:Relation => (this eq that) || 
      (that.child == this.child && this.parent == that.parent)
    case _ => false
  }
  
  /** Hash code based on the hash codes of child and parent */
  override def hashCode = 13*child.hashCode+parent.hashCode
  
  /** Returns string with child and parent id and edge weight */
  override def toString = "%s --> %s" format (child.id,parent.id)
}
