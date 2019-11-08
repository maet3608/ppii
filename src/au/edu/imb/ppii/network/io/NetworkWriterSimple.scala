package au.edu.imb.ppii.network.io

import au.edu.imb.ppii.network.Network
import java.io.{File, BufferedWriter, FileWriter}

/**
 * A network writer that writes protein ids and interactions
 * as a simple text format.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 24/08/2010
 */

object NetworkWriterSimple extends NetworkWriter {
  def write(network:Network, file:File) {
    val writer = new BufferedWriter(new FileWriter(file))
    writer.write("PPI_Matrix %d %d\n" format (network.size, network.interactions.size))
    for(p <- network.toSeq.sortBy(_.index))
      writer.write("protein %d %s\n" format (p.index, p.id))
    for(i <- network.interactions)
      writer.write("interaction %d %d\n" format (i.protein1.index, i.protein2.index))
    writer.close()
  }
}

/**
 * Usage example.
 */
object NetworkWriterSimpleExample extends App {
  println("running...")

  val network = NetworkReaderMel.read("data/mips_with_GOA_cc.txt")
  NetworkWriterSimple.write(network, "data/mips_with_GOA_cc.mat")

  println("finished.")
}