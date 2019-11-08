package au.edu.imb.ppii.utils

import java.io.{File, FileWriter, BufferedWriter}

/**
 * A wrapper around a buffered file writer.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 08/09/2010
 */

/**
 * Extension for buffered writer to write a line with new line.
 */
trait Writeln {
  def write(string:String)
  def writeln(string:String) = { write(string); write("\n") }
}


/**
 * Factory for writers.
 */
object Writer {
  def apply(filepath:String) =
    new BufferedWriter(new FileWriter(new File(filepath))) with Writeln
}