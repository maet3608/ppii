package au.edu.imb.pdb.npg

import java.io.{BufferedWriter, FileWriter, File}
import scala.io.Source
import au.edu.imb.structure.{Atom, Residue, Structure, PDBReader}

/**
 * Encodes interaction sites as sets of pairs of interacting n-grams to
 * infer PPI.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 30/07/2010
 */


/** Pair of interacting n-grams */
class NGP(ngram1:String, ngram2:String) {
  override def toString = "%s -  %s" format (ngram1,ngram2)
}

/** Set of n-gram pairs */
class Site(ngpairs:Seq[NGP]) extends Seq[NGP] {
  def apply(idx:Int) = ngpairs(idx)
  def iterator = ngpairs.iterator
  def length = ngpairs.length
  override def toString = ngpairs.mkString("Site:\n","\n","\n")
}


object NGramEncoding {
  val pdbDir = "f:/PPII/Data/PDB/"
  val pdbRawDir = pdbDir+"raw/"
  val pdbInterDir = pdbDir+"interactions/"
  val pdbidsPath = pdbDir+"chains/pdbids.txt"
  val pdbidsMultiPath = pdbDir+"chains/pdbidsmulti.txt"

  /** Loads a list of pdb ids from the given file */
  def pdbids(filepath:String) = Source.fromFile(filepath).getLines()

  /** Writes a file with pdbids of structures with multiple chains */
  def multiChains() = {
    val writer = new BufferedWriter(new FileWriter(new File(pdbidsMultiPath)))
    for(pdbid <- pdbids(pdbidsPath) if PDBReader.read(pdbDir+pdbid+".PDB").molecules.length > 1)
      writer.write(pdbid+"\n")
    writer.close()
  }

  /** Iterator over all n-grams of a sequence */
  def ngrams(seq:String, n:Int) = seq.sliding(n)

  /** Shortest atomic distance between two residues */
  def shortestDistance(r1:Residue,r2:Residue) = {
    var minDist = 1e10
    for(i <- 0 until r1.atoms.length;
        j <- 0 until r2.atoms.length;
        val dist = r1.atoms(i).distance(r2.atoms(j)) )
      if(dist < minDist) minDist = dist
    minDist
  }

  /** returns collection of interaction sites describe by n-grams of the given size */
  def sites(structure:Structure, threshold:Double, n:Int):Iterable[Site] = {
    val chains = structure.chains
    val (n2,nn2) = (n/2, n-n/2)
    (for(i <- 0   until chains.length;
        j <- i+1 until chains.length;
        val (ci,cj) = (chains(i),chains(j)) )
      yield new Site(
        for(ii <- n2 to ci.residues.length-nn2;
            jj <- n2 to cj.residues.length-nn2;
            val (ri,rj) = (ci.residues(ii),cj.residues(jj));
            val d = shortestDistance(ri,rj)
            if(d < threshold) )
          yield new NGP(ci.sequence.slice(ii-n2,ii+nn2), cj.sequence.slice(jj-n2,jj+nn2))
      )) filter (_.nonEmpty)      
  }

             
  def main(args: Array[String]) {
    println("running ...")
    //multiChains()

//    for(pdbid <- pdbids(pdbidsMultiPath).take(3)) {
//      println(pdbid)
//      val structure = PDBReader.read(pdbDir+pdbid+".PDB")
//      sites(structure, 7.5, 4).foreach(println)
//    }

    println("finished")
  }
}