package au.edu.imb.db.sandbox

import au.edu.imb.db.xml.BDB
import scala.io.Source._
import au.edu.imb.ppii.utils.Writer


/**
 * Various queries to the uniprot database.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 16/09/2010
 */

/** Retrieves protein and gene names for a list of given accession numbers */
object AccessionToGeneName extends App {
  val bdb = BDB("uniprot_sprot.dbxml", "f:/PPII/Data/Uniprot/xml/", "http://uniprot.org/uniprot")

    def retrieve(accession:String) = {
      val query =
      """for $e in collection('%s')//entry[accession = '%s']
         return (string-join($e/name,":"),string-join($e/gene/name,":"))
      """ format (bdb.name,accession)
      val res = bdb.xquery(query)
      val ret = if(res.hasNext) (res.next.asString,res.next.asString) else (None,None)
      res.delete()
      ret
    }

    println("querying... ")
    for(accession <- fromFile("c:/Maet/upreg_nodes.txt").getLines();
        val (pname,gname) = retrieve(accession))
      printf("%s\t%s\t%s\n", accession, pname, gname)
    println("finished.")

  bdb.delete()
}

/** Retrieves GO terms for a list of given accession numbers */
object AccessionToGO extends App {
  val bdb = BDB("uniprot_sprot.dbxml", "f:/PPII/Data/Uniprot/xml/", "http://uniprot.org/uniprot")

    def retrieve(accession:String) = {
      val query =
      """for $e in collection('%s')//entry[accession = '%s']
         let $go := $e/dbReference[@type="Go"]
         return string-join($go/@id/string(),",")
      """ format (bdb.name,accession)
      val res = bdb.xquery(query)
      val ret = res.next.asString
      res.delete()
      ret
    }

    println("querying... ")
    for(line <- fromFile("c:/Maet/Temp/nuc.txt").getLines()) {
      val accession = line.split("\\s+")(0)
      val goids = retrieve(accession)
      printf("%s\t%s\n", accession, goids)
    }
    println("finished.")

  bdb.delete
}


/** Retrieves protein and gene names for a set of GO ids */
object GOGenes extends App {
  val bdb = BDB("uniprot_sprot.dbxml", "f:/PPII/Data/Uniprot/xml/", "http://uniprot.org/uniprot")

  val ids = Set("GO:0006284", "GO:0006285", "GO:0006288", "GO:0006287",
                "GO:0000729","GO:0010792","GO:0006302","GO:0000724",
                "GO:0010569", "GO:0006303").map("@id='"+_+"'").mkString("", " or ","")
  val query =
      """for $e in collection('%s')//entry
         let $acc := $e/accession
         let $go := $e/dbReference[%s]/@id
         let $gos := $e/dbReference[@type="Go"]/@id
         let $org := $e/organism/name[@type="scientific"]
         where exists($go) and $org = "Homo sapiens"
         return (string-join($acc,","),string-join($e/gene/name,","),string-join($gos,","))
      """ format (bdb.name, ids)


  println("querying... ")
  val res = bdb.xquery(query)
  while(res.hasNext) {
    println(res.next.asString+"\t"+res.next.asString+"\t"+res.next.asString.split(',').filter(id => ids.contains(id)).mkString(","))
  }
  println("finished.")
  res.delete()
  bdb.delete
}


/** Retrieves subcellular localization information for human */
object SubcellularLocalizationHuman extends App {
  println("querying... ")
  val bdb = BDB("uniprot_sprot.dbxml", "f:/PPII/Data/Uniprot/xml/", "http://uniprot.org/uniprot")

  val query =
  """for $e in collection('%s')//entry
     let $acc := $e/accession
     let $scl := $e/comment[@type="subcellular location"]/subcellularLocation/location
     let $go := $e/dbReference[@type="Go"]/property[@type="term" and starts-with(@value,'C:')]/substring(@value,3)
     let $taxid := $e/organism/dbReference[@type="NCBI Taxonomy"]/@id
     where $taxid = "9606" and exists($scl) and exists($go)
     return (string-join($acc,","), string-join($scl,","), string-join($go,","))
  """ format bdb.name
  
  val res = bdb.xquery(query)
  val output = Writer("c:/Maet/Temp/human_scl.txt")
  while(res.hasNext) {
    println(".")
    output.writeln(res.next.asString+"\t"+res.next.asString+"\t"+res.next.asString)
    output.flush()
  }
  res.delete()
  output.close()

  bdb.delete
  println("finished.")
}



/** Adds an index to the uniprot_trembl database */
object AddDatabaseIndex extends App {
  println("opening...")
  val bdb = BDB("uniprot_trembl.dbxml", "f:/PPII/Data/Uniprot/xml/", "http://uniprot.org/uniprot")
  println("getCacheMax: "+bdb.environmentConfig.getCacheMax)
  println("getCacheSize: "+bdb.environmentConfig.getCacheSize)
  println("adding...")
  bdb.container.addIndex("http://uniprot.org/uniprot", "accession", "edge-element-equality-string")
  bdb.delete
  println("finished.")
}

