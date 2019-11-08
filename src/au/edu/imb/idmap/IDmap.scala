package au.edu.imb.idmap

import scala.io.Source
import java.io.FileWriter

/**
 * Mapping of protein/gene identifiers to UniProt identifiers
 * using the uniprot mapping database file "idmapping.dat".
 * For more info see: http://www.uniprot.org/help/mapping
 * idmapping.dat downloaded from
 * ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 9/08/11
 */

object IDmap {
  // path to uniprot mapping file
  val idmapfile = "f:\\PPII\\Data\\IDMap\\idmapping.dat.example"
  // function to extract ids from a line
  type Extractor = String => Seq[String]

  // each line contains one id
  def extractorOne(line:String) = line.trim
  // all identifiers in the line are separated by tabs are taken
  def extractorTabs(line:String) = line.split("\t")
  // first two tab-separated identifiers are taken
  def extractorTabs2(line:String) = line.split("\t").take(2)

  // extracts all ids from a collection of lines using an extractor
  def extractIDs(lines:Iterable[String], extractor:Extractor):Set[String] =
    lines.flatMap(extractor).map(_.toUpperCase.trim).toSet

  // reads all lines of the files that contains the ids to map
  def readLines(fname:String) = Source.fromFile(fname).getLines().toList

  // creates an id map from a file with ids and the uniprot mapping database file
  // ids are extracted from the lines of the file <fname> using the given extractor
  def createIdMap(fname:String, extractor:Extractor) = {
    val lines = readLines(fname)
    var idSet = extractIDs(lines, extractor)
    var nIds = idSet.size
    idSet.foreach(println)

    var idMap = Map[String,String]()
    for(line <- Source.fromFile(idmapfile).getLines() if idSet.size > 0) {
      val Array(acc,idtype,id) = line.split("\t").map(_.toUpperCase.trim)
      if(idSet.contains(id)) {
        idMap += id -> acc
        idSet -= id
        printf("%10s <--> %s:%s\n",acc,id,idtype)
      }
    }
    printf("\n%d of %d identifiers not mapped!\n", idSet.size, nIds)
    idMap
  }

  // loads an id map from a file <fname> where each line is of the form <id><tab><uniprot_id>
  def loadIdMap(fname:String) = Map[String,String]() ++
      (for(line <- Source.fromFile(fname).getLines(); Array(id,acc) = line.split("\t")) yield (id,acc))

  // loads an id map from a file <fname> where each line is of the form <id><tab><uniprot_id>;{<uniprot_id>;}
  // output format used by http://pir.georgetown.edu/pirwww/search/idmapping.shtml
  def loadIdMapPIR(fname:String) = Map[String,String]() ++
      (for(line <- Source.fromFile(fname).getLines(); Array(id,accs) = line.split("\t")) yield (id,accs.split(";")(0)))


  // writes a id mapped version of the file <fname>. Output file is <fname>.mapped
  def writeMapped(fname:String, idMap:Map[String,String], extractor:Extractor) {
    val writer = new FileWriter(fname+".mapped")
    for(line <- readLines(fname); ids = extractor(line) if ids.forall(idMap.keySet.contains))
      writer.write(ids.foldLeft(line)((l,id) => l.replace(id, idMap(id)))+"\n")
    writer.close()
  }


  def main(args:Array[String]) {
    println("mapping ...")
    val fname = "c:\\Maet\\Temp\\BenHur\\dipMIPS.data"
    val extractor:Extractor = extractorTabs2
    val idMap = loadIdMapPIR("c:\\Maet\\Temp\\BenHur\\dipMIPS.map")
    //val idMap = createIdMap(fname,extractor)
    writeMapped(fname,idMap,extractor)
    println("finished")
  }
}