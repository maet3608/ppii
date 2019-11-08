package au.edu.imb.ppii.network.annotation

import scala.io.Source
import java.io.File
import au.edu.imb.ppii.network.Protein
import au.edu.imb.ppii.network.Protein._
import au.edu.imb.ppii.utils.Annotations

/**
 * Annotates proteins with their genetic locus.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 11/10/2010
 */

class AnnotatorLoci(filepath:String) extends Annotator {
  private var loci = Map[String,String]()

  for(line <- Source.fromFile(new File(filepath)).getLines())
    loci += AnnotatorLoci.locus(line)

  def annotate(protein:Protein) =
    loci.get(protein.id).foreach(l => protein.annotations += P_LOCUS -> l)  
  
}

/**
 * Helper methods to read create loci tags
 */
object AnnotatorLoci {
  /** The chromosome is split into sections of the given size and gene loci
   * are mapped to section ids */
  var SECTIONSIZE:Int = 10000

  /** takes line and returns tuple (accession, locus) */
  def locus(line:String) = {
    val elems = line.split('\t')
    val acc = elems(0)                             // accession number
    val chr = elems(1)                             // chromosome name
    val pos = (elems(2).toInt+elems(3).toInt)/2    // middle position of gene
    val locus = "%s_%05d" format (chr, pos/SECTIONSIZE)
    (acc, locus)
  }
}


/** Usage example */
object AnnotatorLociExample extends App {
  println("running...")
  val filepath = "data/ecoli.loci"
  val annotator = new AnnotatorLoci(filepath)
  println("anotator loaded...")
  val protein = new Protein(new Annotations(List((P_ID,"P0AG59"))))
  println("annotating...")
  annotator.annotate(protein)
  println(protein.annotations)
  println("finished.")
}