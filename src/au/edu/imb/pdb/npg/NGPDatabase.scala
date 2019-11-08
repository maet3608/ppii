package au.edu.imb.pdb.npg

import au.edu.imb.db.xml.BDB
import au.edu.imb.ppii.utils.{Writer, Writeln}
import scala.collection.mutable.Set
import io.Source

/**
 * Extracts pairs of interacting n-grams (NGPs) from an XML file that contains
 * interacting residue pairs and writes the n-gram pairs to a text file.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 08/09/2010
 */

object NGPDatabase {
  val inname    = "f:/PPII/Data/PDB/Interactions/interactions.xml"
  val outname   = "f:/PPII/Data/PDB/Interactions/ngrams75.txt"


// This would have been nice but Berkeley DB bails out :-((((
//  val container = "interactions.dbxml"
//  val namespace = ""
//  val path      = "f:/PPII/Data/PDB/Interactions/"
//  def readNGPs(n:Int = 3) = {
//    val ngps = Set[(String,String)]()
//    def nGram(chain:String, pos:Int) = chain.slice(pos-n/2,pos+n/2+1)
//    val query = """
//                for $i in collection('%s')/Structures/Structure/ChainInteraction/Interactions/Interaction
//                let $ci := $i/../..
//                where xs:double($i/@distance) <= 7.5
//                return ($ci/Chain_1/string(),$ci/Chain_2/string(), $i/@position_1/string(),$i/@position_2/string())
//                """ format container
//
//    var progress = 0
//    val bdb = BDB(container, path, namespace)
//    val res = bdb.xquery(query)
//    while(res.hasNext) {
//      val chain1 = res.next.asString
//      val chain2 = res.next.asString
//      val pos1   = res.next.asString.toInt
//      val pos2   = res.next.asString.toInt
//      val nGram1 = nGram(chain1,pos1)
//      val nGram2 = nGram(chain2,pos2)
//      if(nGram1.length == n && nGram2.length == n)
//        ngps += (if(nGram1 < nGram2) (nGram1,nGram2) else (nGram2,nGram1))
//      progress += 1
//      if(progress % 10000 == 0) println(progress+" npgs: "+ngps.size)
//    }
//    res.delete
//    bdb.delete
//    ngps
//  }

  // Ugly but fast and since Berkeley DB has failed me ...
  def readNGPs(n:Int = 3) = {
    def nGram(chain:String, pos:Int) = chain.slice(pos-n/2,pos+n/2+1)
    val ngps = Set[(String,String)]()
    val lines = Source.fromFile(inname).getLines()
    val chain = """.+>(.+)</.+""".r
    val interaction = """<Interaction position_1="(.+)" position_2="(.+)" distance="(.+)"/>""".r

    var progress = 0
    while(lines.hasNext) {
      var line = lines.next
      if(line.contains("<ChainInteraction>")) {
        val chain(chain1) = lines.next.trim
        val chain(chain2) = lines.next.trim
        lines.next
        line = lines.next
        while(line.contains("<Interaction position")) {
          val interaction(p1,p2,d) = line.trim
          if(d.toDouble <= 7.5) {
            val nGram1 = nGram(chain1,p1.toInt)
            val nGram2 = nGram(chain2,p2.toInt)
            if(nGram1.length == n && nGram2.length == n)
              ngps += (if(nGram1 < nGram2) (nGram1,nGram2) else (nGram2,nGram1))
            progress += 1
            if(progress % 10000 == 0) println(" npgs: "+ngps.size)              
          }
          line = lines.next
        }
      }
    }
    ngps
  }

  def writeNGPS(nGrams:Set[(String,String)]) = {
    val writer = Writer(outname)
    for( (nGram1, nGram2) <- nGrams )
      writer.writeln(nGram1+"\t"+nGram2)
    writer.close
  }

  def main(args:Array[String]) = {
    println("running...")
    val ngps = readNGPs()
    println("ngps : "+ngps.size)
    writeNGPS(ngps)
    println("finished.")
  }
}