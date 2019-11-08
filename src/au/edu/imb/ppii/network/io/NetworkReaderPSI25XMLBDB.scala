package au.edu.imb.ppii.network.io

import java.io.File
import au.edu.imb.db.xml.BDB
import au.edu.imb.ppii.utils.Annotations
import au.edu.imb.ppii.network.Protein._
import au.edu.imb.ppii.network.{Interaction, Protein, Network}

/**
 * Reads an interaction network from an XML file in PSI-2.5 format stored
 * in a Berkeley XML database.
 * Note: Requires that there is only one list of interactors (= one document)
 * stored within the database and that interactors do have a uniprot id!
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 24/08/2010
 */
object NetworkReaderPSI25XMLBDB {
  /**
   * Reads a network from the database.
   * @param filepath: Path of database.
   * @param namespace: XML namespace used within XML document stored in the DB
   * @param threshold: Only interactions detected by (low throughput) experiments
   * with not more than then the given threshold of interactions studied are included.  
   */
  def read(filepath:String, namespace:String="", threshold:Int=10):Network =
    read(new File(filepath), namespace, threshold)

  def read(filepath:File, namespace:String, threshold:Int):Network = {
    val container = filepath.getName
    val bdb = BDB(container, filepath.getParent, namespace)
    val network = new Network(filepath.getName)

    val interactorsQuery =
    """for $ia in collection('%s')/entrySet/entry/interactorList/*
       let $uid := $ia/xref/*[@db='uniprot knowledge base'][1]/@id/string()
       where exists($uid) 
       return ($ia/@id/string(), $uid)
    """ format container
    var proteinMap:Map[String,Protein] = Map()
    val resInteractors = bdb.xquery(interactorsQuery)
    while(resInteractors.hasNext) {
      val (id, uid) = (resInteractors.next.asString, resInteractors.next.asString)
      proteinMap += id -> new Protein(new Annotations(List((P_ID,uid))))
    }
    resInteractors.delete()

    val interactionsQuery =
    """for $ia in collection('%s')/entrySet/entry/interactionList/*
       let $ref := $ia/experimentList/experimentRef/string()
       return ($ref, string-join($ia/participantList/participant/@id/string(), ','))
    """ format container
    var interactionList = List[(Protein,Protein,String)]()
    var refMap = Map[String,Int]().withDefaultValue(0)
    val resInteractions = bdb.xquery(interactionsQuery)
    while(resInteractions.hasNext) {
      val ref = resInteractions.next.asString
      refMap = refMap.updated(ref,refMap(ref)+1)
      val ids = resInteractions.next.asString.split(",")
      for(i <- 0 until ids.length; j <- i+1 until ids.length) {
        val p1 = proteinMap.getOrElse(ids(i),null)
        val p2 = proteinMap.getOrElse(ids(j),null)
        if(p1 != null && p2 != null) interactionList ::= (p1,p2,ref)
      }
    }
    resInteractions.delete()

    // Filter for low throughput experiments and add set of unique interactions to network
    interactionList.filter(t => refMap(t._3) <= threshold).
      map(t => Interaction(t._1,t._2)).toSet[Interaction].
      foreach(i => network += (i.protein1, i.protein2))

    bdb.delete
    network
  }
}


/**
 * Usage Example.
 */
object NetworkReaderPSI25XMLBDBExample extends App {
  val path = "f:/PPII/Data/PPI/BioGrid/xml/"
  val namespace = "net:sf:psidev:mi"

  def writeEdgelist(container:String, threshold:Int) {
    println("loading network  : "+container)
    val network = NetworkReaderPSI25XMLBDB.read(path+container, namespace, threshold)
    println("  proteins   : "+network.size)
    println("  interaction: "+network.interactions.size)
    println("writing edge list")
    NetworkWriterEdgeList.write(network, "data/%s.txt" format container)
    println("finished.")
  }

  def printNetworkStatistics(container:String, threshold:Int) {
    val network = NetworkReaderPSI25XMLBDB.read(path+container, namespace, threshold)
    println("container  : "+container)
    println("proteins   : "+network.size)
    println("interaction: "+network.interactions.size)
  }

  // counts how frequent a specific experiment is used by multiple interactions.
  // used to distinguish between high and low throughput experiments
  def printExperimentFrequencies(container:String) {
    val bdb = BDB(container, path, namespace)
    val query =
    """
    for $i in collection('%s')/entrySet/entry/interactionList/interaction/experimentList/experimentRef
    return $i/string()
    """ format container
    val result = bdb.xquery(query)
    var data = Seq[String]()
    while(result.hasNext)
      data :+= result.next.asString
    result.delete()
    data.groupBy(n => n).toSeq.sortBy(_._2.length).foreach(t => printf("%d\n",t._2.length))
    bdb.delete
  }

  def printEvidenceCodes(container:String) {
    val bdb = BDB(container, path, namespace)
    val query =
    """
    distinct-values(for $ia in collection('%s')/entrySet/entry/interactionList/*
    let $evidence := $ia//attribute[@name="BioGRID Evidence Code"]
    return $evidence/string())
    """ format container
    val result = bdb.xquery(query)
    while(result.hasNext)
      println(result.next.asString)
    result.delete()
    bdb.delete
  }


  println("loading ...")
  //val container = "Mus_musculus.dbxml"
  //val container = "Homo_sapiens.dbxml"
  //val container = "Saccharomyces_cerevisiae.dbxml"
  //val container = "Drosophila_melanogaster.dbxml"
  //val container = "Arabidopsis_thaliana.dbxml"
  val container = "Escherichia_coli_K12_MG1655.dbxml"

  writeEdgelist(container, 10)
  //printExperimentFrequencies(container)
  printNetworkStatistics(container, 10)
  //printEvidenceCodes(container)

  println("finished.")
}