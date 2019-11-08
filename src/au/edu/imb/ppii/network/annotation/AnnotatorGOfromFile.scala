package au.edu.imb.ppii.network.annotation

import scala.io.Source
import java.io.File
import au.edu.imb.ppii.network.Protein
import au.edu.imb.ppii.network.Protein._


/** Reads GO annotations for proteins from a file.
 *  Line format is: accession_number \t go_id, go_id, go_id ... \n
 */
class AnnotatorGOfromFile(filepath:String) extends Annotator {
  private var goMap = Map[String,Array[String]]()

  for(line <- Source.fromFile(new File(filepath)).getLines()) {
    line.split('\t') match {
      case Array(accnr, goids) => goMap += accnr -> goids.split(',').map(_.trim)
      case _ => /* no goids */
    }
  }

  def annotate(protein:Protein) {
    goMap.get(protein.id).foreach( _.foreach(goid => protein.annotations += P_GOIDS -> goid) )
  }

  /** Set of all GO ids stored within the map */
  def goids = goMap.valuesIterator.map(_.toSet).reduceLeft(_|_)

}