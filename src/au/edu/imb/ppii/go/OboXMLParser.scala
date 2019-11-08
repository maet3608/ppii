package au.edu.imb.ppii.go

import scala.xml.{XML,Node}
import au.edu.imb.ppii.utils._
/**
 *  UNFINISHED IMPLEMENTATION
 */

/**
 * Parses GO ontologies in OBO XML format. 
 * http://www.geneontology.org/GO.downloads.ontology.shtml
 * Note that the class implements an Iterable over the Go terms read.
 * @param filepath path to file in OBO XML format to read
 */
/*
class OboXMLParser(val filepath:String) extends Iterable[Term] {
  private val tags  = List("id", "namespace", "is_a", "part_of", "is_obsolete")

  private def parse(term:Node) = 
    for(tag <- tags; node <- term \\ tag) yield (tag,node.text)
       
  /** Returns an iterator over read annotations */
  def iterator:Iterable[Term] = 
    for(term <- XML.loadFile(filepath) \ "term") yield new Term(new Annotation(parse(term)))
                                         
}
*/


object ExampleOboXMLParser {
  def main(args: Array[String]) {
    println("running...")    
    val tags = Set("id", "namespace", "is_a", "part_of", "is_obsolete")
    val oboxml = XML.loadFile( "go.obo-xml")
    for(term <- oboxml \ "term" ) {
      for(tag <- tags) {
        for(node <- term \\ tag)
          printf("%s: %s\n",tag,node.text)
      }
    }  
    println("finished...")
  }
}