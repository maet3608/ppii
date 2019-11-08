package au.edu.imb.ppii.network.annotation

import au.edu.imb.ppii.network.annotation.Annotator._
import au.edu.imb.ppii.network.Protein
import au.edu.imb.db.xml.BDB
import au.edu.imb.ppii.utils.Annotations
import au.edu.imb.ppii.network.Protein._

import au.edu.imb.ppii.go.{Term, OboParser, Ontology}


/**
 * Abstract base class that extracts information from the uniprot database stored
 * in a Berkeley XML DB to annotate proteins.
 */
abstract class AnnotatorUniprotDbxml extends Annotator {
  protected var name      = "uniprot_sprot.dbxml"
  protected var namespace = "http://uniprot.org/uniprot"
  protected var entry     = "for $ref in collection('%s')//entry[accession = '%s']"
  protected val bdb = BDB(name, UNIPROT_PATH, namespace)

  /** Override, creates the database query string for the given
   * container name and uniprot id */
  protected def createQuery(name:String, uid:String):String

  def annotate(protein:Protein) = {
    val query = createQuery(name, protein.id)
    val result = bdb.xquery(query)
    while(result.hasNext)
      protein.annotations += ((result.next.asString, result.next.asString))
    result.delete
  }

  override def finalize = bdb.delete
}

/**
 * Annotates proteins with their gene ontology term identifiers.
 */
class AnnotatorGO(accept:EvidenceFilter = EvidenceFilterAll) extends AnnotatorUniprotDbxml {
  def createQuery(name:String, uid:String) =
    entry +
    """/dbReference[@type = 'Go']
    return ($ref/property[@type = 'evidence']/@value/string(),$ref/@id/string())
    """  format (name, uid)

  override def annotate(protein:Protein) = {
    val query = createQuery(name, protein.id)
    val result = bdb.xquery(query)
    while(result.hasNext) {
      val (evidence, goid) = (result.next.asString, result.next.asString)
      if(accept(evidence)) protein.annotations += ((P_GOIDS, goid))
    }
    result.delete
  }
}

/**
 * Annotates with slimmed GO terms.
 * @param slimFile Path to a GO slim file in OBO format.
 * @param ontologyFile Path to GO ontology file in OBO format
 */
class AnnotatorGOSlim(slimFile:String, ontologyFile:String) extends AnnotatorUniprotDbxml {
  private val slimSet = OboParser(slimFile).toSet
  private val ontology = new Ontology(OboParser(ontologyFile), Ontology.all)

  def createQuery(name:String, uid:String) =
    entry +
    """/dbReference[@type = 'Go']
    return $ref/@id/string()
    """  format (name, uid)

  override def annotate(protein:Protein) = {
    val query = createQuery(name, protein.id)
    val result = bdb.xquery(query)
    var goids = Set[Term]()
    while(result.hasNext)
      ontology.get(result.next.asString).foreach(goids += _)
    result.delete
    for(goid <- ontology.slim(goids, slimSet))
      protein.annotations += ((P_GOIDS, goid.id))
  }
}


/**
 * Flags membrane proteins by looking for the uniprot keyword tag "Membrane".
 */
class AnnotatorMembrane extends AnnotatorUniprotDbxml {
  def createQuery(name:String, uid:String) =
    entry +
    """[keyword = 'Membrane']
    return ('%s',"true")
    """  format (name, uid, P_MEMBRANE)
}

/**
 * Annotates proteins with their Interpro domain identifiers.
 */
class AnnotatorInterpro extends AnnotatorUniprotDbxml {
  def createQuery(name:String, uid:String) =
    entry +
    """/dbReference[@type = 'InterPro']
    return ('%s',$ref/@id/string())
    """  format (name, uid, P_INTERPRO)
}

/**
 * Annotates proteins with their KEGG ids.
 */
class AnnotatorKEGG extends AnnotatorUniprotDbxml {
  def createQuery(name:String, uid:String) =
    entry +
    """/dbReference[@type = 'KEGG']
    return ('%s',$ref/@id/string())
    """  format (name, uid, P_KEGG)
}

/**
 * Annotates proteins with their EC number.
 */
class AnnotatorEC extends AnnotatorUniprotDbxml {
  def createQuery(name:String, uid:String) =
    entry +
    """/dbReference[@type = 'EC']
    return ('%s',$ref/@id/string())
    """  format (name, uid, P_EC)
}

/**
 * Guess what? takes all keywords.
 */
class AnnotatorKeyword extends AnnotatorUniprotDbxml {
  def createQuery(name:String, uid:String) =
    entry +
    """/keyword
    return ('%s',$ref/string())
    """  format (name, uid, P_KEYWORDS)
}

/**
 * Annotates with dbReference type and id, e.g. InterPro:IPR000353
 */
class AnnotatorDBRef extends AnnotatorUniprotDbxml {
  def createQuery(name:String, uid:String) =
    entry +
    """/dbReference
    return ('%s',concat($ref/@type/string(),":",$ref/@id/string()))
    """  format (name, uid, P_DBREFS)
}

/**
 * Extracts the protein sequence from uniprot for a given protein.
 */
class AnnotatorSequence extends AnnotatorUniprotDbxml {
  def createQuery(name:String, uid:String) =
    entry +
    """/sequence
    return ('%s',$ref/string())
    """  format (name, uid, P_SEQUENCE)
}

/**
 * Extracts the protein sequence from uniprot for a given protein
 * and converts it to its n-gram set.
 */
class AnnotatorNGram(n:Int =3) extends AnnotatorUniprotDbxml {
  def createQuery(name:String, uid:String) =
    entry +
    """/sequence
    return ('%s',$ref/string())
    """  format (name, uid, P_NGRAMS)

  private def nGrams(sequence:String) =
    Set()++sequence.toUpperCase.sliding(n,1)

  override def annotate(protein:Protein) = {
    val query = createQuery(name, protein.id)
    val result = bdb.xquery(query)
    while(result.hasNext) {
      val id = result.next.asString
      val sequence = result.next.asString
      nGrams(sequence).foreach(nGram => protein.annotations += id->nGram)      
    }
    result.delete
  }
}


/** Usage example */
object AnnotatorUniprotExample {
  def main(args:Array[String]) = {
    println("running...")
    //val annotator = new AnnotatorGO(EvidenceFilterAll)
    val annotator = new AnnotatorGOSlim("data/goslim_generic.obo","data/go.obo")
    //val annotator = new AnnotatorSequenceWeb()
    println("anotator loaded...")
    val protein = new Protein(new Annotations(List((P_ID,"P33299"))))
    println("annotating...")    
    annotator.annotate(protein)
    println(protein.annotations)
    println("finished.")
  }
}