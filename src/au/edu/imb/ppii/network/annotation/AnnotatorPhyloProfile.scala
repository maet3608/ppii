package au.edu.imb.ppii.network.annotation

import scala.io.Source
import java.io.File
import au.edu.imb.ppii.network.Protein
import au.edu.imb.ppii.network.Protein._
import au.edu.imb.ppii.utils.Annotations

/**
 * Annotates proteins with their phylogentic profile
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 03/09/2010
 */

class AnnotatorPhyloProfile(filepath:String) extends Annotator {
  private val lines = Source.fromFile(new File(filepath)).getLines()
  private val profiles = readProfiles()

  private def readTaxids:Map[Int,String] = {
    var taxids = Map[Int,String]()
    while(lines.hasNext) {
      val line = lines.next()
      if(!line.startsWith("#")) return taxids
      val elems = line.split(',')
      taxids += elems(1).toInt -> elems(2)
    }
    sys.error("Can't find end of tax id map!"+filepath)
  }

  private def readProfiles(evalue:Double = 1e-20) = {
    var profiles = Map[String,Array[String]]()
    val taxids = readTaxids
    for(line <- lines) {
      val elems = line.split(',')
      val id = elems(0)
      val profile = elems.drop(1).map(_.toDouble).zipWithIndex.filter(_._1 < evalue).map(t => taxids(t._2))
      profiles += id -> profile
    }
    profiles
  }


  def annotate(protein:Protein) {
    profiles.get(protein.id).foreach(profile =>
      profile.foreach{taxid => protein.annotations += P_PROFILE -> taxid})
  }
}


/** Usage example */
object AnnotatorPhyloProfileExample extends App {
  println("running...")
  val filepath = "f:/PPII/Data/PhyloProfiles/saccharomyces.profile"
  val annotator = new AnnotatorPhyloProfile(filepath)
  println("anotator loaded...")
  val protein = new Protein(new Annotations(List((P_ID,"P32492"))))
  println("annotating...")
  annotator.annotate(protein)
  println(protein.annotations)
  println("finished.")
}


