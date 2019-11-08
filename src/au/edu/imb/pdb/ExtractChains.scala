package au.edu.imb.pdb

import au.edu.imb.structure.PDBReader
import io.Source
import java.io.{FileWriter, File, FileInputStream, FileOutputStream}

/**
 * Filters protein structures loaded from PDB files
 * - excluding structures with more than one model,
 * - chains shorter than 30 residues
 * - non amino-acid chains
 * and extracts unique chains over all structures and
 * writes them into a single file in FASTA format.
 *
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 15/07/2010
 */


object ExtractChains {
  val inpath  = "f:/PPII/Data/PDB/raw/"
  val outpath = "f:/PPII/Data/PDB/chains/"

  def pdbFiles(path:String):Array[String] = 
    (new File(path)).list.filter(_.endsWith(".PDB"))
    
  def filterLines(filepath:String, p: String => Boolean) =
    Source.fromFile(filepath).getLines().filter(p)
  def countLines(filepath:String, predicate: String => Boolean) =
    filterLines(filepath,predicate).length

  /** Files with multiple models, usually NMR */
  def isMultiModel(filepath:String) =
    countLines(filepath, l => l.startsWith("MODEL ")) > 0

  /** Copies a file */
  def copy(src:File, dest:File) = {
    new FileOutputStream(src).getChannel.transferFrom(
        new FileInputStream(dest).getChannel, 0, Long.MaxValue )
  }


  def main(args:Array[String]) {
    var sequences = Seq[(String,String)]()
    val filenames = pdbFiles(inpath)
    for((filename,i) <- filenames.zipWithIndex) {
      val filepath = inpath+filename
      printf("%.2f   %s \n", 100.0*i/filenames.length, filename.take(4))
      if(!isMultiModel(filepath)) {
        val structure = PDBReader.read(filepath)
        for(chain <- structure.chains) {
          val sequence = chain.residues.map(_.code).mkString
          if(sequence.length >= 30)
            sequences = sequences :+ ((structure.name+"_"+chain.name, sequence))
        }
      }
    }
    val groupedSeqs = sequences.groupBy(_._2)
    val writer = new FileWriter(new File(outpath+"chains.ffa"))
    for((seq,entries) <- groupedSeqs)
      writer.write(entries.map(_._1).mkString(">",":","\n")+seq+"\n")
    writer.close()
  }

}