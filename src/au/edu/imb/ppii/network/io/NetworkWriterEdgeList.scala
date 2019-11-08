package au.edu.imb.ppii.network.io

import au.edu.imb.ppii.network.Network
import java.io.{File, BufferedWriter, FileWriter}

/**
 * A network writer that writes an edge list.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 24/08/2010
 */

object NetworkWriterEdgeList extends NetworkWriter {
  def write(network:Network, file:File) {
    val writer = new BufferedWriter(new FileWriter(file))
    for(i <- network.interactions)
      writer.write("%s\t%s\n" format (i.protein1.id, i.protein2.id))
    writer.close
  }
}

/**
 * Usage example.
 */
object NetworkWriterEdgeListExample extends App {
  println("running...")

  val network = NetworkReaderMel.read("data/mips_with_GOA_cc.txt")
  NetworkWriterEdgeList.write(network, "data/mips.txt")

  println("finished.")
}