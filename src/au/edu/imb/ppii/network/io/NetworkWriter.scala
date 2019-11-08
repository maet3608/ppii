package au.edu.imb.ppii.network.io

import java.io.{FileWriter, BufferedWriter, File}
import au.edu.imb.ppii.network.Network

/**
 * A writer for PPI networks.
 * Version: 1.00
 * Author : Stefan Maetschke
 */

/**
 * Base class for PPI network writers. Derived classes have to implement
 * the write method.
 */
abstract class NetworkWriter {
  /** Abstract method to write a network to the given file. Override. */
  def write(network:Network, file:File):Unit

  /** Writes a network to the given filepath */
  def write(network:Network, filepath:String):Unit = write(network, new File(filepath))
}


