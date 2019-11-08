package au.edu.imb.ppii.network.annotation

import au.edu.imb.ppii.network.Protein
import au.edu.imb.ppii.network.Protein._
import java.net.URL
import xml.XML

/**
 * Abstract base class that extracts information online from the uniprot website.
 */
abstract class AnnotatorUniprotWeb extends Annotator {
  /** getter for the xml data of the given protein */
  protected def xml(protein:Protein) =
    XML.load(new URL("http://www.uniprot.org/uniprot/"+protein.id+".xml"))

  /** getter for dbReference entries in xml data of the given protein */
  protected def dbrefs(protein:Protein) =
     xml(protein) \ "entry" \ "dbReference"

  /** annotates with the attribute data extracted from a dbReference tag */
  protected def annotate(protein:Protein, uid:String, refType:String, refValue:String) =
    try {
      (xml(protein) \ "entry" \ "dbReference").filter(dbref => (dbref \ "@type").text == refType).
         foreach{dbref => protein.annotations += ((uid,(dbref \ refValue).text))}
    } catch {
      case e: Exception => println("Cannot find protein with id="+protein.id)
    }

  /** annotates with the content value of the specified node */
  protected def annotate(protein:Protein, uid:String, nodeName:String) =
    try {
      (xml(protein) \ "entry" \ nodeName).foreach{n => protein.annotations += ((uid,n.text))}
    } catch {
      case e: Exception => println("Cannot find protein with id="+protein.id)
    }
}


/**
 * Annotates proteins with their gene ontology term identifiers retrieved
 * from the GO website.
 */
class AnnotatorGOWeb extends AnnotatorUniprotWeb {
  def annotate(protein:Protein) = annotate(protein, P_GOIDS, "GO", "@id")
}

/**
 * Annotates proteins with their EC number retrieved from the GO website.
 */
class AnnotatorECWeb extends AnnotatorUniprotWeb {
  def annotate(protein:Protein) = annotate(protein, P_EC, "EC", "@id")
}

/**
 * Annotates proteins with their keywords retrieved from the GO website.
 */
class AnnotatorKeywordsWeb extends AnnotatorUniprotWeb {
  def annotate(protein:Protein) = annotate(protein, P_KEYWORDS, "keyword")
}

/**
 * Annotates proteins with their sequences retrieved from the GO website.
 */
class AnnotatorSequenceWeb extends AnnotatorUniprotWeb {
  def annotate(protein:Protein) =
    (xml(protein) \ "entry" \ "sequence").
            foreach{n => protein.annotations += ((P_SEQUENCE,n.text.replace("\n","")))}
}