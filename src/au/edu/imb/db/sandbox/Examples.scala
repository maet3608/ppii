package au.edu.imb.db.sandbox

import com.sleepycat.db.{Environment,EnvironmentConfig}
import com.sleepycat.dbxml._
import java.io.File

/**
 * Some examples how to use the API to the Berkley XML database
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 06/08/2010
 */

object Uniprot {
  def main(args:Array[String]) {
    //val containerName = "uniprot_sprot.dbxml"
    val containerName = "uniprot_test.dbxml"
    val containerPath = "c:/Maet/Software/Berkeley DB XML"
    val collection = "collection('%s')" format containerName
    println("running ...")
    val envConfig = new EnvironmentConfig
    envConfig.setUseEnvironment(true)
    envConfig.setAllowCreate(true)
    envConfig.setInitializeCache(true)
    val env = new Environment(new File(containerPath), envConfig)
    val mgrConfig = new XmlManagerConfig()
    mgrConfig.setAdoptEnvironment(true)
    mgrConfig.setAllowAutoOpen(true)
    //mgrConfig.setAllowExternalAccess(true)    
    val mgr = new XmlManager(env, mgrConfig)  
    val cont = mgr.openContainer(containerName)
    val qc = mgr.createQueryContext
    qc.setNamespace("", "http://uniprot.org/uniprot")
    qc.setEvaluationType(XmlQueryContext.Lazy)
    val res = mgr.query(collection+"/uniprot/entry/accession/text()", qc)
    //val res = mgr.query("for $a in %s//accession where $a = 'Q91G55' return $a/../name/text()" format collection, qc)
    //val res = mgr.query("for $a in %s//accession return count($a)" format collection, qc)
    //val res = mgr.query("for $a in %s//entry where $a/keyword = 'Transmembrane' return $a/accession/text()" format collection, qc)
    //val res = mgr.query("for $a in %s/uniprot return count($a/entry)" format collection, qc)
    while(res.hasNext)
		  println(res.next.asString)
    res.delete
    cont.delete
    mgr.delete
    println("finished")
  }
}


object Profiles {
  def main(args:Array[String]) = {
    val containerName = "saccharomyces_EuBac.dbxml"
    val containerPath = "f:/PPII/Data/PhyloProfiles"
    //val containerPath = "c:/Maet/Software/Berkeley DB XML"
    val collection = "collection('%s')" format containerName
    println("running ...")
    val envConfig = new EnvironmentConfig
    envConfig.setUseEnvironment(true)
    envConfig.setAllowCreate(true)
    //envConfig.setInitializeCache(true)
    val env = new Environment(new File(containerPath), envConfig)
    val mgrConfig = new XmlManagerConfig()
    mgrConfig.setAdoptEnvironment(true)
    //mgrConfig.setAllowAutoOpen(true)
    //mgrConfig.setAllowExternalAccess(true)
    val mgr = new XmlManager(env, mgrConfig)
    val cont = mgr.openContainer(containerName)
    val qc = mgr.createQueryContext
    qc.setEvaluationType(XmlQueryContext.Lazy)
    val res = mgr.query(collection+"/profiles/profile/id/text()", qc)
    //val res = mgr.query(collection+"/profiles/profile[id='P53174']/evalues/evalue/text()", qc)
    while(res.hasNext)
		  println(res.next.asString)
    res.delete
    cont.delete
    mgr.delete
    println("finished")
  }
}


object Examples {
  def main(args:Array[String]) = {
    val content = "<people><person><name>joe</name></person><person><name>mary</name></person></people>"
    //val queryString = "collection('people.dbxml')/people/person[name=$name]"
    val docName = "people"
    val containerName = "people.dbxml"
    val mgr = new XmlManager()

    if(mgr.existsContainer(containerName) != 0) mgr.removeContainer(containerName)

    val cont = mgr.createContainer(containerName)

    cont.putDocument(docName, content)

	  // Querying requires an XmlQueryContext
	  val qc = mgr.createQueryContext()
	    // Add a variable to the query context, used by the query
	  //qc.setVariableValue("name", new XmlValue("mary"))
    val queryString = "collection('people.dbxml')/people/person"
    val res = mgr.query(queryString, qc)

	  // Process results -- just print them
	  var value:XmlValue = null
	  print("Result: ")
    while(res.hasNext)
		  println(res.next.asString())

    res.delete
    cont.delete
    mgr.delete
  }
}