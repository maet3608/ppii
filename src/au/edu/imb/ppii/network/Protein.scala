package au.edu.imb.ppii.network
/**
 * Describes proteins/nodes within a PPI network.
 * Version: 1.01
 * Author : Stefan Maetschke
 */

import au.edu.imb.ppii.utils.Annotations
import au.edu.imb.ppii.network.Protein._
import java.lang.Double

/**
 * Protein/node within a PPI network. It is a set of annotation and a
 * list of edges/interactions.
 */
class Protein(val annotations:Annotations) {
  val index  = createIndex()
  private var _interactions = List[Interaction]();

  /** Creates a protein with the given id */
  def this(uid:String) = this(new Annotations(List((P_ID,uid))))
      
  /** Returns a sequence of interactions for this protein */
  def interactions:Seq[Interaction] = _interactions
  
  /** Establishes an interaction. Does not check if it already exists. */
  def interact(that:Protein,weight:Double=1.0) = {
    val interaction = new Interaction(this, that, weight)
    this._interactions ::= interaction
    that._interactions ::= interaction
    interaction
  }
  
  /** Tests if there is an interaction of that protein with this protein */
  def isInteracting(that:Protein) = _interactions.exists(_.contains(that))
  
  /** Iterator over all direct neighbor proteins */
  def neighbors = for(interaction <-_interactions) yield
    if(interaction.protein1==this) interaction.protein2 else interaction.protein1
  
  /** Returns the first value of the annotation for the given tag */
  def apply(tag:String) = annotations(tag)
  /** Returns all values of the annotation for the given tag */
  def values(tag:String) = annotations.values(tag)
  /** Returns all values of the annotation for the given tag or an empty list */
  def valuesOrElse(tag:String) = annotations.valuesOrElse(tag)
  /** Convenience function that tests if the protein annotation contains the given tag */
  def contains(tag:String) = annotations.contains(tag)
  /** Convenience function that returns a unique protein id */
  def id = annotations(P_ID)
  /** Convenience function that returns the associated GO IDs */
  def goids = valuesOrElse(P_GOIDS)
  
  /** Two proteins are equal if their references or their ids are the same */
  override def equals(other:Any) = other match {
    case that:Protein => (this eq that) || this.id == that.id
    case _ => false
  }
  
  /** Hashcode based on protein ID */
  override def hashCode = id.hashCode
  
  /** returns string with protein id */
  override def toString = "Protein %s" format id
}

/**
 * Only used to provide unique node indices and annotation constants
 */
object Protein {
  val P_ID         = "P_ID"           // should be uniprot id = accession number
  val P_NAME       = "P_NAME"         // Protein name
  val P_GOIDS      = "P_GOIDS"        // GO identifiers assigned to a protein
  val P_KEYWORDS   = "P_KEYWORDS"     // uniprot keywords
  val P_DBREFS     = "P_DBREFS"       // database references
  val P_INTERPRO   = "P_INTERPRO"     // Interpro domain identifiers
  val P_KEGG       = "P_KEGG"         // KEGG identifiers
  val P_EC         = "P_EC"           // EC identifiers
  val P_MEMBRANE   = "P_MEMBRANE"     // flags a membrane protein
  val P_SEQUENCE   = "P_SEQUENCE"     // Protein sequence
  val P_NGRAMS     = "P_NGRAMS"       // Protein sequence split into n-grams
  val P_PROFILE    = "P_PROFILE"      // Phylogenetic profile
  val P_LOCUS      = "P_LOCUS"        // Genetic locus


  // Instance counter
  private var counter = -1

  /** Running number to protein instance to an index */
  def createIndex() = { counter += 1; counter}
}
