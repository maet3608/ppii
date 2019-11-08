package au.edu.imb.ppii.utils

import scala.util.parsing.combinator._
import java.io.{File, FileReader}

/**
 * Parses tree data in Newick format.
 * context free grammar derived from
 * http://evolution.genetics.washington.edu/phylip/newick_doc.html
 * see also
 * http://evolution.genetics.washington.edu/phylip/newicktree.html
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 07/10/2010
 */
class NewickParser extends JavaTokenParsers {
  type Tuple = (String,Double)
  def tree: Parser[List[Any]] = descendants~name_len<~";" ^^ {case d~nl => List(d,nl)}
  def descendants: Parser[List[Any]]  = "("~>repsep(subtree,",")<~")"
  def subtree: Parser[Any] = descendants~name_len ^^ (_.productIterator.toList) | name_len
  def name_len: Parser[Tuple] = name~len ^^ {case n~l => (n,l.toDouble)}
  def len: Parser[String] = opt(":"~>floatingPointNumber) ^^ (_.getOrElse("0"))
  def name: Parser[String] = quoted ^^ unquote | unquoted
  val quoted: Parser[String]  = """'.*'""".r
  val unquoted: Parser[String]  = """[^(),:;]*""".r
  def unquote(text:String) = text.substring(1,text.length-1)
}


/**
 * Parser factory
 */
object NewickParser extends NewickParser {
  /** parses the text and returns the tree as a list structure or throws an exception */
  def apply(text:String) = result(parseAll(tree, text))
  /** parses the file and returns the tree as a list structure or throws an exception */
  def apply(file:File) = result(parseAll(tree, new FileReader(file)))

  private def result(r:ParseResult[List[Any]]) =
    if(r.successful) r.get else sys.error(r.toString)
}


/**
 * Usage Example
 */
object NewickParserExample extends App {
  // pretty printing
  def pretty(tree:Any, level:Int=0):Unit = tree match {
    case list:List[_] => list.foreach(pretty(_,level+1))
    case (name,len) => printf("%s%s:%s\n", "-"*level,name,len)
  }
  //val input = "(A,B,(C,D)E)F"  // missing semicolon => exception
  val input = "(A,'B',(C:0.5,D)E:1)F:20;"
  //val input = """(Bovine:0.69395,(Gibbon:0.36079,(Orang:0.33636,(Gorilla:0.17147,(Chimp:0.19268, Human:0.11927):0.08386):0.06124):0.15057):0.54939,Mouse:1.21460):0.10;"""
  //val input = new File("f:/PPII/Data/PhyloProfiles/taxomonicTreeNCBI.nwk")
  pretty(NewickParser(input))
}