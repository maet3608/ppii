package au.edu.imb.ppii.network.io

import scala.io.Source
import au.edu.imb.ppii.utils._
import au.edu.imb.ppii.network.Protein._
import java.io.File
import au.edu.imb.ppii.network.{Protein, Network}

/**
 * A simple network reader that reads pairs of interacting proteins 
 * described by their protein ids and corresponding
 * lists of GO ids (cellular component) annotating their sub-cellular 
 * localization.
 * The format was devised by Melissa Davis
 * Version: 1.05
 * Author : Stefan Maetschke
 */
object NetworkReaderMel extends NetworkReader {
  
  /** Reads a network from the given file */
  def read(file:File):Network = {
    val network = new Network(file.getName)

    /** Parses info and creates a protein object from it or returns existing */
    def toProtein(elems:Array[String], index:Int) = {
      val id = elems(index+2)
      if(network.contains(id))
        network(id)
      else  // only create Protein if not existing
        new Protein(new Annotations((P_ID,id)::(P_NAME,elems(index+1))::
          elems(index+3).split(", ").toList.map(id => (P_GOIDS,id.trim))))
    }

    for(line <- Source.fromFile(file).getLines()) {
      val elems = line.split("\t")
      if(elems(2) != elems(6)) network += (toProtein(elems,0), toProtein(elems,4))    
    }
    network
  }
}


object ExampleNetworkReaderMel extends App {
  println("running...")
    
  val net = NetworkReaderMel.read("data/mips_with_GOA_cc.txt")
    
  println("nodes:             "+net.size)
  println("sum interactions:  "+net.interactions.length)
  println("max interactions:  "+net.map(_.interactions.length).reduceLeft(_ max _))
  println("loose nodes:       "+net.count(_.interactions.length==0))
  println("self inter nodes:  "+net.interactions.count(i=>i.protein1==i.protein2))
    
  val inter = net.map(_.interactions.length).toSeq
  println("mean interactions: "+(inter.reduceLeft(_ + _)/inter.length))
    
  println("finished.")
}