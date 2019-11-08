package au.edu.imb.structure

/**
 * Parses a PDB file and creates a structural model consisting
 * of molecules, chains, residues and atoms.
 *
 * Version 1.00
 * Author: Stefan Maetschke
 */

import scala.io.Source
import java.io.File


/**
 * Reader for structure data in PDB format.
 * http://www.pdb.org
 */
object PDBReader {
  /** Extracts the PBD id from a pdb file with a pdb id as filename. */
  private def pdbID(filepath:File) = filepath.getName.take(4).toUpperCase

  /** acceptance functions for amino acids only */
  def onlyAA(line:String) = line.slice(17,20).trim.length == 3
  /** acceptance functions for nucleotides only */
  def onlyNucleotide(line:String) = line.slice(17,20).trim.length == 1
  /** accepts all */
  def acceptAll(line:String) = true


  /**
   * Reads a protein structure from a PDB file and returns a structure.
   * @param filepath File path to PDB file to read.
   */
  def read(filepath:String):Structure = read(new File(filepath))

  /**
   * Reads a protein structure from a PDB file and returns a structure.
   * @param filepath File path to PDB file to read.
   * @param accept Filter function, e.g. PDBModel.onlyAA accepts only amino acids.
   * @return Returns a protein structure.
   */
  def read(filepath:File, accept:(String) => Boolean = PDBReader.onlyAA):Structure = {
    var structure = Structure(pdbID(filepath))
    val lines = Source.fromFile(filepath).getLines()
    val molID = """MOL_ID: ([^;]+)""".r
    val chainInfo = """CHAIN: ([^;]+)""".r
    val chainInfoNext = """((\s\w[,;\s])+\s)""".r
    var lastResiduePos = ""

    for(line <- lines) {
      def slice(start:Int, end:Int) = line.slice(start,end).trim
      def atomCoords = List(slice(30,38),slice(38,46),slice(46,54)).map(_.toFloat)
      def addMolecule(name:String) = Molecule(structure, name)
      def addChains(names:String):Unit = {
        names.split("[,;]").map(name => Chain(structure.molecules.last,name.trim))
        if(names.trim.endsWith(","))
          chainInfoNext.findFirstMatchIn(lines.next.drop(10)).foreach(m => addChains(m.group(1)))
      }

      if(line.startsWith("COMPND")) {
        molID.findFirstMatchIn(line).foreach(m => addMolecule(m.group(1)))
        chainInfo.findFirstMatchIn(line).foreach(m => addChains(m.group(1)))
      }
      else if(line.startsWith("ATOM") && accept(line)) {
        val (atomPos,atomName,residueName,chainName,residuePos) =
          (slice(4,11),slice(11,17),slice(17,20),slice(20,22),slice(22,27))
        val currChain = structure.chains.find(_.name == chainName).get

        if(lastResiduePos != residuePos) {
          Residue(currChain,residueName)
          lastResiduePos = residuePos
        }
      
        Atom(structure.residues.last,atomName,atomPos,atomCoords)
      }
    }
    // remove empty chains and structures. Can occur due to filtering with accept function
    structure.chains = structure.chains.filter(_.residues.nonEmpty)
    structure.molecules = structure.molecules.filter(_.chains.forall(_.residues.nonEmpty))
    structure
  }
}


/**
 * Usage examples.
 */
object ExamplePDBReader extends App {
  val structure = PDBReader.read("c:/PPII/Data/PDB/raw/1ZTG.PDB")
  //println(structure.chains)
  println(structure)

  /*
  def pdbFiles(path:String) = (new File(path)).listFiles.filter(_.getName.endsWith(".pdb"))
  for((file,i) <- pdbFiles("c:/DDI/PDB").zipWithIndex) {
    printf("%5d: %s\n", i, file.getName.stripSuffix(".pdb"))
    PDBReader.read(file)
  }
  */
}