package au.edu.imb.pdb.rri

import java.io.{File}
import au.edu.imb.structure.{Atom, Residue, Structure, PDBReader}
import au.edu.imb.ppii.utils.{Writer}

/**
 * Creates a database (actually only an XML file) of interacting residues
 * by scanning PDB structures for residue atoms on different chains that
 * are closer than 10 Angstrom to each other.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 01/09/2010
 */
object InteractionDatabase {
  val pdbDir = "f:/PPII/Data/PDB/"
  val pdbRawDir = pdbDir+"raw/"
  val pdbInterDir = pdbDir+"interactions/"


  /** Shortest atomic distance between two residues */
  def shortestDistance(r1:Residue,r2:Residue) = {
    var minDist = 1e10
    for(i <- 0 until r1.atoms.length;
        j <- 0 until r2.atoms.length;
        val dist = r1.atoms(i).distance(r2.atoms(j)) )
      if(dist < minDist) minDist = dist
    minDist
  }
  
  /** Writes residue interactions between chains to an xml file */
  def writeInteractions(indir:String, outfile:String, threshold:Double):Unit = {
    val writer = Writer(outfile)
    def writeSites(structure:Structure) = {
      println(structure.name)
      writer.writeln("  <Structure id=\"%s\">" format structure.name)
      val chains = structure.chains
      for(i <- 0 until chains.length; j <- i+1 until chains.length) {
          val (ci,cj) = (chains(i),chains(j))
          writer.writeln("    <ChainInteraction>")
          writer.writeln("      <Chain_1 id=\"%s\">%s</Chain_1>" format (ci.name,ci.sequence))
          writer.writeln("      <Chain_2 id=\"%s\">%s</Chain_2>" format (cj.name,cj.sequence))
          writer.writeln("      <Interactions>")
          for(ri <- ci.residues; rj <- cj.residues;
              val d = shortestDistance(ri,rj)
              if(d < threshold) )
            writer.writeln("        <Interaction position_1=\"%d\" position_2=\"%d\" distance=\"%.4f\"/>"
                    format (ri.index,rj.index,d))
          writer.writeln("      </Interactions>")
          writer.writeln("    </ChainInteraction>")
      }
      writer.writeln("  </Structure>")
    }
    writer.writeln("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>")
    writer.writeln("<Structures>")

    for(file <- (new File(indir)).listFiles if file.getName.endsWith(".PDB")) {
      val structure = PDBReader.read(file)
      if(structure.molecules.length > 1) writeSites(structure)
    }
    writer.writeln("</Structures>")
    writer.close()
  }
    

  def main(args: Array[String]) {
    println("running ...")
    writeInteractions(pdbRawDir, pdbInterDir+"interactions.xml", 10.0)
    println("finished")
  }
}