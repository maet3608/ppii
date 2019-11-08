package au.edu.imb.ddi

import scala.io.Source

/**
 * Readers for protein domain information.
 * Author:  Stefan Maetschke
 * Version: 1.02
 */

object DomainReader {
  // BlastProDom predictions are awkward/wrong. We filter them out.
  private def accept(line:String) = !line.contains("BlastProDom")
  
  /** Returns iterator over domains read from given file */
  def apply(filepath:String) = 
    Source.fromFile(filepath).getLines().filter(accept).
            map(_.split('\t')).filter(_.length == 13).
            map(es => new Domain(es(0),es(4),es(12),es(3),es(6).toInt-1,es(7).toInt-1))
}


/**
 * Usage example.
 */
object ExampleDomainReader extends App {
  DomainReader("c:/DDI/Domain24kproteins/domains.txt").
          toSeq.groupBy(_.start).toSeq.sortBy(_._1).
          foreach(t => printf("%6d %d\n",t._1,t._2.length))
}
