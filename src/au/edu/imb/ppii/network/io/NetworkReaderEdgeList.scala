package au.edu.imb.ppii.network.io

import scala.io.Source
import au.edu.imb.ppii.utils._
import au.edu.imb.ppii.network.Protein._
import java.io.File
import au.edu.imb.ppii.network.{Interaction, Protein, Network}

/**
 * A simple network reader that reads pairs of interacting proteins 
 * described by their protein ids and ensures that interactions are unique.
 * Version: 1.01
 * Author : Stefan Maetschke
 */
object NetworkReaderEdgeList extends NetworkReader {
  
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
      val elems = line.split("\t") 
      interactionSet += Interaction(toProtein(elems(0)), toProtein(elems(1)))
    }
    interactionSet.foreach(i => network += (i.protein1, i.protein2))
    network
  }
}


object ExampleNetworkReaderEdgeList extends App {
  println("running...")
    
  val net = NetworkReaderEdgeList.read("data/mips.txt")
    
  println("nodes:             "+net.size)
  println("sum interactions:  "+net.interactions.length)
  println("max interactions:  "+net.map(_.interactions.length).reduceLeft(_ max _))
  println("loose nodes:       "+net.count(_.interactions.length==0))
  println("self inter nodes:  "+net.interactions.count(i=>i.protein1==i.protein2))
    
  val inter = net.map(_.interactions.length).toSeq
  println("mean interactions: "+(inter.reduceLeft(_ + _)/inter.length))
    
  println("finished.")
}