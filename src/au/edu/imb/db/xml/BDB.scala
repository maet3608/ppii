package au.edu.imb.db.xml

import java.io.File
import com.sleepycat.dbxml.{XmlQueryContext, XmlManager, XmlManagerConfig}
import com.sleepycat.db.{EnvironmentConfig, Environment}


/**
 * Wrapper around the Berkeley XML DB.
 * Useful link: http://java.sys-con.com/node/175405
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 18/08/2010
 * @param name Name of container, e.g. mus.dbxml
 * @param path Filepath to folder that contains container, e.g. c:/databases
 * @param namespace Namespace of the xml document, e.g. net:sf:psidev:mi
 */
class BDB(val name:String, val path:String, val namespace:String) {
  val environmentConfig = new EnvironmentConfig
  environmentConfig.setUseEnvironment(true)
  environmentConfig.setAllowCreate(true)
  environmentConfig.setInitializeCache(true)
  //environmentConfig.setCacheSize(1048576)

  val environment = new Environment(new File(path), environmentConfig)
  val managerConfig = new XmlManagerConfig()
  managerConfig.setAdoptEnvironment(true)
  managerConfig.setAllowAutoOpen(true)

  val manager = new XmlManager(environment, managerConfig)
  val container = manager.openContainer(name)
  val queryContext = manager.createQueryContext
  queryContext.setEvaluationType(XmlQueryContext.Lazy)  
  if(namespace != "") queryContext.setNamespace("", namespace)

  def xquery(querystr:String) =
    manager.query(querystr, queryContext)

  /** adds the prefix "collection('container_name')" to the query! */
  def query(querystr:String) =
    manager.query(("collection('%s')" format name)+querystr, queryContext)

  def delete() {
    container.delete()
    manager.delete()
  }
}

/** database factory */
object BDB {
  def apply(name:String, path:String, namespace:String=""):BDB =
    new BDB(name, path, namespace)

  def apply(filepath:File, namespace:String):BDB =
    BDB(filepath.getName, filepath.getParent, namespace)

  def apply(filepath:String, namespace:String):BDB =
    BDB(new File(filepath), namespace)
}



/** Usage example */
object BDBExample {
  def main(args:Array[String]) {
    println("opening...")
    //val bdb = BDB("mus.dbxml", "f:/PPII/Data/PPI/BioGrid/xml", "net:sf:psidev:mi")
    //val bdb = BDB("saccharomyces.dbxml", "f:/PPII/Data/PPI/BioGrid/xml", "net:sf:psidev:mi")
    //val bdb = BDB("saccharomyces_EuBac.dbxml", "f:/PPII/Data/PhyloProfiles")
    val bdb = BDB("uniprot_sprot.dbxml", "f:/PPII/Data/Uniprot/xml/", "http://uniprot.org/uniprot")
    //val res = bdb.query("/uniprot/entry/accession/text()")
    //val res = bdb.query("//interactor/@id/string()")

//    val query =
//    """
//    declare namespace fx = "http://www.functx.com";
//    declare function fx:sort( $seq as item()* )  as item()* {
//       for $item in $seq
//       order by $item
//       return $item
//    };
//    count(distinct-values(for $ia in collection('%s')//entry/interactionList/*
//       return string-join(fx:sort($ia/participantList/participant/@id/string()), ',')))
//    """ format bdb.name

    val query =
       """
       for $ref in collection('uniprot_sprot.dbxml')//entry//dbReference[@type = 'Go']
       return ($ref/property[@type = 'evidence']/@value/string(),$ref/@id/string())     
       """
    val res = bdb.xquery(query)
    println("querying... ")
    while(res.hasNext)
		  println(res.next.asString)
    res.delete()
    bdb.delete()
    println("finished.")
  }
}