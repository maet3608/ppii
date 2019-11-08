package au.edu.imb.experiment.ppii.NGramEncoding

import java.io.{BufferedWriter, FileWriter, File}
import scala.io.Source
import scala.math.abs
import au.edu.imb.structure.{Atom, Residue, Structure, PDBReader}

/**
 * Computes statistical data concerning residue-residue interactions,
 * e.g. calculates distances between interacting residues and maps
 * interacting residues on sequences.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 30/07/2010
 */

object InteractionsStatistics {
  val pdbidsPath = "f:/PPII/Data/PDB/chains/pdbids.txt"
  val pdbDir = "f:/PPII/Data/PDB/raw/"
  val outPath = "f:/PPII/Data/PDB/chains/"

  def pdbids = Source.fromFile(pdbidsPath).getLines()

  /** Shortest atomic distance between two residues */
  def shortestDistance(r1:Residue,r2:Residue) = {
    var closest:(Double,Atom,Atom) = (1e10,null,null)

    for(i <- 0 until r1.atoms.length;
        j <- 0 until r2.atoms.length;
        val dist = r1.atoms(i).distance(r2.atoms(j)) )
      if(dist < closest._1) closest = (dist, r1.atoms(i), r2.atoms(j))
    closest
  }

  /** distances between interacting residues within a chain */
  def intraDistances(structure:Structure, writer:BufferedWriter, threshold:Double) = {
    for(chain <- structure.chains) {
      val residues = chain.residues
      for(i <- 0   until residues.length; j <- i+1 until residues.length ) {
        val ri = residues(i)
        val rj = residues(j)
        val (d,a1,a2) = shortestDistance(ri,rj)
        if(d < threshold)
          writer.write("%s,%s,%s,%s,%s,%s,%s,%.3e\n" format
                  (structure.name, chain.name, chain.name, ri.code, rj.code, a1.name, a2.name, d))
      }
    }
  }

  /** interacting residues within a chain sequence */
  def intraSequences(structure:Structure, writer:BufferedWriter, threshold:Double) = {
    for(chain <- structure.chains) {
      val residues = chain.residues
      val seq = (" "*residues.length).toArray
      var interacts = false
      for(i <- 0 until residues.length; j <- i+1 until residues.length ) {
        val ri = residues(i)
        val rj = residues(j)
        val (d,a1,a2) = shortestDistance(ri,rj)
        if(d < threshold && abs(i-j)>3) {
          seq(i) = ri.code; seq(j) = rj.code; interacts = true
        }
      }
      if(interacts)
        writer.write(seq.mkString(structure.name+"_"+chain.name+":","","|\n"))
    }
  }


  /** distances between interacting residues across chains */
  def interDistances(structure:Structure, writer:BufferedWriter, threshold:Double) = {
    val chains = structure.chains
    for(i <- 0 until chains.length; j <- i+1 until chains.length ) {
      val ci = chains(i)
      val cj = chains(j)
      for(ri <- ci.residues; rj <- cj.residues) {
        val (d,a1,a2) = shortestDistance(ri,rj)
        if(d < threshold)
          writer.write("%s,%s,%s,%s,%s,%s,%s,%.3e\n" format
                  (structure.name, ci.name, cj.name, ri.code, rj.code, a1.name, a2.name, d))
      }
    }
  }

  /** interacting residues across chain sequences */
  def interSequences(structure:Structure, writer:BufferedWriter, threshold:Double) = {
    val chains = structure.chains
    for(i <- 0   until chains.length;
        j <- i+1 until chains.length ) {
      val ci = chains(i)
      val cj = chains(j)
      val seqi = (" "*ci.residues.length).toArray
      val seqj = (" "*cj.residues.length).toArray
      var interacts = false
      for(ii <- 0 until ci.residues.length; jj <- 0 until cj.residues.length) {
        val ri = ci.residues(ii)
        val rj = cj.residues(jj)
        val (d,a1,a2) = shortestDistance(ri,rj)
        if(d < threshold) {
          seqi(ii) = ri.code; seqj(jj) = rj.code; interacts = true
        }
      }
      if(interacts) {
        writer.write(seqi.mkString(structure.name+"_"+ci.name+":","","|\n"))
        writer.write(seqj.mkString(structure.name+"_"+cj.name+":","","|\n"))
        writer.write("\n")
      }
    }
  }

  /** distances between interacting residues across chains in different molecules = complexes */
  def complexDistances(structure:Structure, writer:BufferedWriter, threshold:Double) = {
    val molecules = structure.molecules
    for(mi <- 0 until molecules.length; mj <- mi+1 until molecules.length ) {
      for(ci <- molecules(mi).chains; cj <- molecules(mj).chains) {
        for(ri <- ci.residues; rj <- cj.residues) {
          val (d,a1,a2) = shortestDistance(ri,rj)
          if(d < threshold)
            writer.write("%s,%s,%s,%s,%s,%s,%s,%.3e\n" format
                    (structure.name, ci.name, cj.name, ri.code, rj.code, a1.name, a2.name, d))
        }
      }
    }
  }  

  /** Creates buffered file writer */
  def createWriter(filepath:String) =
    new BufferedWriter(new FileWriter(new File(filepath)))

             
  def main(args: Array[String]) {
    println("running ...")
    val complexDistWriter = createWriter(outPath+"complex.csv")
    //val interDistWriter = createWriter(outPath+"inter.csv")
    //val intraDistWriter = createWriter(outPath+"intra.csv")
    //val interSeqWriter = createWriter(outPath+"inter.txt")
    //val intraSeqWriter = createWriter(outPath+"intra.txt")
    for(pdbid <- pdbids.take(400)) {
      println(pdbid)
      val structure = PDBReader.read(pdbDir+pdbid+".PDB")
      complexDistances(structure, complexDistWriter, 20.0)
      //interDistances(structure, interDistWriter, 20.0)
      //intraDistances(structure, intraDistWriter, 20.0)
      //interSequences(structure, interSeqWriter, 7.5)
      //intraSequences(structure, intraSeqWriter, 7.5)
    }
    complexDistWriter.close()
    //interDistWriter.close()
    //intraDistWriter.close()
    //interSeqWriter.close()
    //intraSeqWriter.close()
    println("finished")
  }
}