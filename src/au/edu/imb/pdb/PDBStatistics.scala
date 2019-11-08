package au.edu.imb.pdb

import java.io.File
import io.Source
import au.edu.imb.structure.PDBReader

/**
 * Calculates various statistics over PDB structures.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 29/07/2010
 */


class Counter[T] {
  var counts = Map[T,Int]().withDefaultValue(0)
  def +=(value:T) = {counts += value->(counts(value)+1); this }
  def ++=(values:Iterable[T]) = { values.foreach(+=); this }
  override def toString =
    counts.toSeq.sortBy(_._2).map(c => "%6d  %s".format(c._2,c._1)).mkString("\n")
}

object PDBStatistics  {
  val pdbFilepath = "f:/PPII/Data/PDB/raw/"

  def pdbFiles(path:String) = (new File(path)).listFiles.filter(_.getName.endsWith(".PDB"))

  def filterLines(f:File, p: String => Boolean) = Source.fromFile(f).getLines().filter(p)
  def countLines(f:File, p: String => Boolean) = filterLines(f,p).length

  def numberModels =
    new Counter[Int] ++= pdbFiles(pdbFilepath).map{f =>
      countLines(f, l => l.startsWith("MODEL "))}

  def numberMolecules =
    new Counter[Int] ++= pdbFiles(pdbFilepath).map(f => PDBReader.read(f).molecules.length)

  def numberChains =
    new Counter[Int] ++= pdbFiles(pdbFilepath).map(f => PDBReader.read(f).chains.length)

  def multiModelFiles = {
    val groups = pdbFiles(pdbFilepath).map{f => 
      (f.getName, countLines(f, l => l.startsWith("MODEL ")))}.groupBy(_._2)
       groups.toSeq.sortBy(_._1).map(t => "%4d (%4d):".format(t._1,t._2.length)+t._2.take(5).
            mkString(",")).mkString("\n")
  }

  def nmrPDBFiles =
    pdbFiles(pdbFilepath).map{f => (f.getName, countLines(f, l => l.startsWith("MODEL ")))}.
    groupBy(_._2).mkString("\n")

  def experimentTypes = {
    val regex = """REMARK .+ : (.+)""".r
    new Counter[String] ++= pdbFiles(pdbFilepath).flatMap(f =>
     Source.fromFile(f).getLines().find(_.contains("EXPERIMENT TYPE"))).map(
       regex.findFirstMatchIn(_).get.group(1).trim)
  }

  def chainLengths =
    new Counter[Int] ++= pdbFiles(pdbFilepath).flatMap(f => PDBReader.read(f).chains).map(_.residues.length)

  def main(args:Array[String]) {
    println("running")
    //println(experimentTypes)
    //println(numberModels)
    //println(multiModelFiles)
    println(numberMolecules)
    println(numberChains)
    println("finished")
  }
}