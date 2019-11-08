package au.edu.imb.ppii.network

import scala.collection.mutable.Map
import java.lang.Double

/**
 * Describes a PPI network. For efficiency reasons the class maintains an
 * additional map, which maps protein ids to protein nodes within the network.
 * The network object itself serves as an iterable over its nodes.
 * Version: 1.00
 * Author : Stefan Maetschke
 */
class Network(val name:String) extends Iterable[Protein] {
  /** mutable map of ids to proteins */
  private val proteinMap = Map[String,Protein]()
  /** List of all interactions */
  private var _interactions = List[Interaction]()

  /** Adds a protein to the network if it does not exist already.
   It returns the added or the existing protein */
  def +=(protein:Protein) = proteinMap.getOrElseUpdate(protein.id, protein)
   
  /** Adds an interaction to the network. If one or both proteins are not
   part of the network they will be added. Does not check if the interaction already
   exists */
  def +=(protein1:Protein, protein2:Protein, weight:Double=0.0):Interaction = {
    val interaction = (this+=protein1) interact (this+=protein2, weight)
    _interactions ::= interaction
    interaction
  }    

  /** Returns a sequence of all interactions */
  def interactions:Seq[Interaction] = _interactions
  
  /** Getter for a protein with the given ID*/
  def apply(id:String) = proteinMap(id)
  
  /** Tests if a protein with the given ID exists */
  def contains(id:String) = proteinMap.contains(id)
  
  /** Tests if the given protein is already within the network */
  def contains(protein:Protein) = proteinMap.contains(protein.id)

  /** Gets a protein with the given ID or returns the given protein if not existing */
  def getOrElse(id:String, protein:Protein) = proteinMap.getOrElse(id, protein)
  
  /** Returns the number of proteins within the network */
  override def size = proteinMap.size
  
   /** Returns an iterable over all proteins */
  def iterator = proteinMap.valuesIterator
}
