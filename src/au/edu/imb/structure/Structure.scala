package au.edu.imb.structure

/**
 * A simple structural protein model consisting of molecules, chains,
 * residues and atoms with their spatial coordinates.
 * Version 1.00
 * Author: Stefan Maetschke
 */

import scala.math.sqrt
import scala.collection.mutable.ArrayBuffer
import java.lang.StringBuilder
import au.edu.imb.structure.maps.AA3to1


/** Euclidean distance between two coordinates */
trait Distance {
  val coords:Seq[Float]
  
  def distance(that:Distance) = {
    def sqr(x:Float) = x*x
    val xs = this.coords
    val ys = that.coords
    sqrt( sqr(xs(0)-ys(0))+sqr(xs(1)-ys(1))+sqr(xs(2)-ys(2)) ) // not elegant but fast
  }
}

/**
 * Atom. Note that pos are atom positions/indices taken from PDB file that 
 * can contain gaps.
 */
case class Atom(residue:Residue, name:String, pos:String, coords:Seq[Float]) extends Distance {
  /** zero-based position within residue */
  val index = residue.atoms.length

  residue.chain.molecule.structure.atoms += this
  residue.atoms += this

  override def toString = 
    "Atom(%d): %-3s (%.1f,%.1f,%.1f)" format (index, name,coords(0),coords(1),coords(2))
}

/**
 * Residue. Contains collection of atoms of that residue.
 * @param chain Chain the residue belongs to
 * @param name Three letter amino acid code
 */
case class Residue(chain:Chain, name:String) extends Distance {
  /** zero-based position within chain */
  val index = chain.residues.length
  /** Single letter code */
  val code:Char = AA3to1(name)
  /** Atoms of this residue. */
  val atoms = new ArrayBuffer[Atom]()
  /** first c-alpha atom or first atom if c-alpha is missing */ 
  lazy val cAlpha = atoms.find(_.name =="CA").getOrElse(atoms.head)
  /** residue coordinates = coordinates of first alpha carbon */
  lazy val coords = cAlpha.coords 

  chain.molecule.structure.residues += this
  chain.residues += this

  /** Shortest atomic distance between two residues */
  def shortestDistance(that:Residue) = {
    var minDist = 1e10
    for(i <- 0 until this.atoms.length;
        j <- 0 until that.atoms.length;
      val dist = this.atoms(i).distance(that.atoms(j)) ) 
      if(dist < minDist) minDist = dist
    minDist 
  }

  override def toString = "Residue(%d): %s (%s)" format (index, name,code)
}

/**
 * Contains collection of residues of that chain.
 * @param molecule Molecule the chain belongs to.
 * @param name Name of chain, usually on letter.
 */
case class Chain(molecule:Molecule, name:String) {
  /** zero-based position within molecule */
  val index = molecule.chains.length
  /** Residues of this chain */
  val residues = new ArrayBuffer[Residue]()

  molecule.structure.chains += this
  molecule.chains += this

  /** Amino acid sequence of the chain */
  def sequence = residues.map(_.code).mkString

  /** Returns amino acid sequence of chain as string in FASTA format */
  def toFasta = ">%s_%s\n%s\n" format (molecule.structure.name, name, sequence)

  override def toString = "Chain(%d): %s" format (index,name)
}

/**
 * Molecule. Contains collection of chains of that molecule.
 * @param structure Structure the molecule belongs to.
 * @param name Molecule id
 */
case class Molecule(structure:Structure, name:String) {
  /** zero-based position within structure */
  val index = structure.molecules.length
  /** Chains of this molecule */
  var chains = new ArrayBuffer[Chain]()

  structure.molecules += this

  override def toString = "Molecule(%d): %s" format (index,name)
}

/**
 * Structure. Contains collection of molecules of that structure.
 * @param name Name of the structure, typically the PDB identifier.
 */
case class Structure(name:String) {
  /** All atoms of the structure */
  var atoms = new ArrayBuffer[Atom]()
  /** All residues of the structure */
  var residues = new ArrayBuffer[Residue]()
  /** All chains of the structure */
  var chains = new ArrayBuffer[Chain]()
  /** All molecules of the structure */
  var molecules = new ArrayBuffer[Molecule]()

  /** n nearest residues to the given one */
  def nearest(residue:Residue, n:Int) =
    residues.map(r => (r,residue.distance(r))).sortBy(_._2).take(n)

  /** returns list of tuples (i,j) indicating contact between the i-th and the j-th
      residue according to the given inContact predicate.  */
  def contacts(inContact:(Residue,Residue) => Boolean) = {
    val n = residues.length
    for(i <- 0 until n; j <- i+1 until n if inContact(residues(i),residues(j)))
      yield (i,j)
  }

  /** String with structure residues in Fasta format */
  def toFasta = ">"+name+"\n"+residues.map(_.code).mkString

  override def toString = {
    // problem with scala.StringBuilder in beta-release 2.8. Use java instead
    val builder = new StringBuilder("Structure: "+name+"\n")
    for(molecule <- molecules) {
      builder.append(" "*2 + molecule + "\n")
      for(chain <- molecule.chains) {
        builder.append(" "*4 + chain + "\n")
        for(residue <- chain.residues) {
          builder.append(" "*6 + residue + "\n")
          for(atom <- residue.atoms)
            builder.append(" "*8 + atom + "\n")
        }
      }
    }
    builder.toString
  }
}

/** Usage example */
object ExampleStructure extends App {
  val structure = PDBReader.read("data/PDB/1A0R.pdb")

  println(structure)          // print structure
  //println(structure.toFasta)  // print sequence in Fasta format

  //structure.molecules.foreach(println)   // print all molecules
  //structure.chains.foreach(println)      // print all chains
  //structure.residues.foreach(println)    // print all residues
  //structure.atoms.foreach(println)       // print all atoms

  //Print chains for all molecules
  //structure.molecules.foreach(m => println(m.chains.mkString(m.name+":\n  ",",","")))

  //Print molecules for chains
  //structure.chains.foreach(c => println(c.name+" -> "+c.molecule.name))

  // Print residue information with back references
  //structure.residues.foreach(r =>
  //  println("%s %s %s" format (r.name, r.chain.name, r.chain.molecule.name)))

  //structure.chains.head.residues.foreach(println) // all residues of first chain
  //println(structure.nearest(structure.residues.head, 5)) // print 5 nearest

  // print distance matrix
  //println(structure.residues.map(r1 => structure.residues.map(
  //  r2 => "%6.2f" format r1.distance(r2)).mkString(" ")).mkString("\n"))
}
