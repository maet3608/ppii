package au.edu.imb.ppii.network.io

import java.io.File
import au.edu.imb.ppii.network.Network

/**
 * Abstract base class for PPI network readers. Derived classes have to implement
 * the read method.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 24/08/2010
 */
abstract class NetworkReader {
  /** Abstract method to read a network from the given file. Override. */
  def read(file:File):Network

  /** Reads a network from the given file path */
  def read(filepath:String):Network = read(new File(filepath))
}

