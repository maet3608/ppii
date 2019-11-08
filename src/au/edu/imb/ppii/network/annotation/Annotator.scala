package au.edu.imb.ppii.network.annotation

import au.edu.imb.ppii.network.{Protein, Network}
import au.edu.imb.ppii.network.Protein._
import au.edu.imb.ppii.utils.Annotations

/**
 * Abstract base class to annotate proteins within a network.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 24/08/2010
 */

abstract class Annotator {
  /** Create an annotated protein for the given id */
  def annotate(id:String):Protein = {
    val protein = new Protein(new Annotations(List((P_ID,id))))
    annotate(protein)
    protein
  }

  /** Annotates the given protein */
  def annotate(protein:Protein):Unit

  /** Annotates the given network */
  def annotate(network:Network):Unit =
    network.map(annotate)
}


/**
 * Removes all annotation values for the given tag from a protein
 */
class AnnotatorRemove(tag:String) extends Annotator {
  def annotate(protein:Protein) = protein.annotations.remove(tag)
}





/** Path constants to annotation databases */
object Annotator {
  val BASE_PATH                 = "f:/PPII/Data/"
  val UNIPROT_PATH              = BASE_PATH+"Uniprot/xml/"
  val PHYLOPROFILES_PATH        = BASE_PATH+"PhyloProfiles/"
}