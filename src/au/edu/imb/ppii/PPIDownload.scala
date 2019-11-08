package au.edu.imb.ppii

import java.io._
import java.net._
import java.util.Properties

/**
 * Downloads PPI data sets from the web
 * 
 * PSI-MI format details: http://www.psidev.info/index.php?q=node/60
 *
 * DIP
 * http://dip.doe-mbi.ucla.edu/dip/Download.cgi?SM=7
 *
 * MINT
 * XML: ftp://mint.bio.uniroma2.it/pub/release/psi/current/psi25/dataset/
 * Tab: ftp://mint.bio.uniroma2.it/pub/release/mitab26/current/
 *
 * MPACT/MIPS
 * ftp://ftpmips.gsf.de/yeast/PPI/
 *
 * IntACT
 * XML: ftp://ftp.ebi.ac.uk/pub/databases/intact/current/psi25/
 * Tab: ftp://ftp.ebi.ac.uk/pub/databases/intact/current/psimitab/
 */
class PPIDownload(url:String) extends Iterable[String] {
  
  /** Iterator iterator over lines read from URL */
  class LineIterator(url:String) extends Iterator[String] {    
    private val reader = new BufferedReader(new InputStreamReader(
                   (new URL(url)).openConnection().getInputStream()))
    private var line = read()
    
    private def read() = reader.readLine()
    def hasNext():Boolean = if(line == null) {reader.close(); false} else true; 
    def next():String = {val temp = line; line = read(); temp} 
  }
  
  /** saves the downloaded data set at the specified filepath */
  def save(filepath: String) = {
    val writer = new BufferedWriter(new FileWriter(filepath))
    this.map(line => writer.write(line+"\n"))
    writer.close()
  } 
    
  /** implements iterable interface */
  def iterator = new LineIterator(url)
}

/**
 * Factory
 */
object PPIDownload {
  def apply(url:String)=  new PPIDownload(url)
}

/**
 * Just a usage example.
 */
object ExamplePPIDownload {
    
  def main(args: Array[String]) {
    println("downloading...")
    
    val downloads = List(
      //("dip.txt", "http://dip.doe-mbi.ucla.edu/dip/Download.cgi?SM=7"),
      ("mint.xml", "ftp://mint.bio.uniroma2.it/pub/release/psi/current/psi25/dataset/Saccharomyces%20cerevisiae.psi25.zip"),
      ("mips.xml", "ftp://ftpmips.gsf.de/yeast/PPI/"),
      ("intact.xml", "ftp://ftp.ebi.ac.uk/pub/databases/intact/current/psi25/ftp://ftpmips.gsf.de/yeast/PPI/mpact-complete.psi25.xml.gz")
    )
    
    val path = "c:/Maet/"
    
    for((name, url) <- downloads) {
      println(name)
      PPIDownload(url).save(path+name)
    }
    
    println("finished.")
  }
}

