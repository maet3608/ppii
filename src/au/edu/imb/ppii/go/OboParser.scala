package au.edu.imb.ppii.go

import scala.io.Source
import au.edu.imb.ppii.utils._

/**
 * Parses GO ontologies in OBO format. Line endings are expected to be in
 * Windows format (\n\r)! 
 * http://www.geneontology.org/GO.format.obo-1_2.shtml
 * Note that the class implements an Iterable over the Go terms read.
 * @param filepath path to file in OBO flat-file format to read
 */
class OboParser(val filepath:String) extends Iterable[Term] {  
  /** Regex that describes a tag-value pair separated by a colon */
  private val tagvalue = """(.+?): ?(.+) *""".r  
  /** Iterator over lines of an OBO file */
  private val lines = Source.fromFile(filepath).getLines()
  
  /** Tests if line is start of a term definition */
  private def isStart(line:String) = line startsWith "[Term]" 
  /** Tests if line is part of the definition body */
  private def isBody(line:String) = line.length > 1 
  
  /** Parses the definition body and creates a Go term */
  private def term() = new Term(new Annotations(lines.takeWhile(isBody).map(parse)))
  /** Parses a single tag-value annotation line. If it fails check line endings! */
  private def parse(line:String) = line match {case tagvalue(tag,value)=>(tag,value)}
  
  /** Returns an iterable over read GO terms */
  def iterator = for(line <- lines.filter(isStart)) yield term()
}


/**
 * Factory
 */
object OboParser {
  def apply(filepath:String) = new OboParser(filepath)
}


/**
 * Usage example
 */
object ExampleOboParser {
  def main(args: Array[String]) {
    println("Parser is running...")
        
    // print out id and name of all cellular components that are not obsolete
    for(term <- OboParser("data/go.obo") if 
          term("namespace")=="cellular_component" && 
          !term.contains("is_obsolete")) 
      println(term.id, term.name)
    
    println("finished!")  
  }
}