package au.edu.imb.ppii.network.io

import scala.io.Source
import au.edu.imb.ppii.utils._
import au.edu.imb.ppii.network.Protein._
import java.io.File
import au.edu.imb.ppii.network.{Interaction, Protein, Network}
import javax.management.remote.rmi._RMIConnection_Stub

/**
 * A network reader that reads pairs of proteins with their interaction weight.
 * Convention is that a weight of -1.0 indicates non-existing interactions and
 * a weight of +1.0 indicates an existing interaction but the reader accepts any
 * range of weights.
 * Proteins are identified by ids (e.g. Uniprot IDs) and the reader ensures
 * that the interactions are unique.
 * Version: 1.00
 * Author : Stefan Maetschke
 */
object NetworkReaderWeightedEdgeList extends NetworkReader {
  
  /** Reads a network from the given file */
  def read(file:File):Network = {
    val network = new Network(file.getName)

    /** Parses info and creates a protein object from it or returns existing */
    def toProtein(id:String) = {
      if(network.contains(id))
        network(id)
      else  // only create Protein if not existing
        new Protein(new Annotations(List((P_ID,id.trim))))
    }

    var interactionSet:Set[Interaction] = Set()
    for(line <- Source.fromFile(file).getLines()) {
      val Array(id1,id2,weight) = line.split("\t")
      interactionSet += Interaction(toProtein(id1), toProtein(id2), weight.toDouble)
    }
    interactionSet.foreach(i => network += (i.protein1, i.protein2, i.weight))
    network
  }
}


object ExampleNetworkReaderWeightedEdgeList extends App {
  println("running...")

  for(name <- List("SC","HS","MM","AT","DM","SP","EC","CT")) {
    val net = NetworkReaderWeightedEdgeList.read("data/%s_string.txt" format name)
    println(name)
    println("nodes:             "+net.size)
    println("pos interactions:  "+net.interactions.filter(_.weight>0.0).length)
    println("neg interactions:  "+net.interactions.filter(_.weight<0.0).length)
    println("sum interactions:  "+net.interactions.length)
    println("max interactions:  "+net.map(_.interactions.length).reduceLeft(_ max _))
    println("loose nodes:       "+net.count(_.interactions.length==0))
    println("self inter nodes:  "+net.interactions.count(i=>i.protein1==i.protein2))
    val inter = net.map(_.interactions.length).toSeq
    println("mean interactions: "+(inter.reduceLeft(_ + _)/inter.length))
  }
    
  println("finished.")
}