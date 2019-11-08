package au.edu.imb.ppii.network

import collection.mutable.HashSet
import io.NetworkReaderMel
import scala.util.Random
import scala.util.Random.shuffle
import scala.io.Source
import java.io._
import javax.management.remote.rmi._RMIConnection_Stub
import com.sun.xml.internal.ws.developer.MemberSubmissionAddressing.Validation

/**
 * Describes an interaction between two proteins. Interactions are undirected
 * edges between nodes (proteins).
 * Version: 1.00
 * Author : Stefan Maetschke
 */
class Interaction(val protein1:Protein, val protein2:Protein, var weight:Double) {

  /** Tests if the given protein is a partner in this interaction */
  def contains(protein:Protein) = protein == protein1 || protein == protein2 
  
  /** Two interaction are equal when their protein partners are equal */
  override def equals(other:Any) = other match {
    case that:Interaction => (this eq that) || 
      (that.protein1 == this.protein1 && that.protein2 == this.protein2) ||
      (that.protein2 == this.protein1 && that.protein1 == this.protein2)
    case _ => false
  }
    
  /** Hash code based on hash codes of interaction partners */
  override def hashCode = protein1.hashCode+protein2.hashCode
  
  /** Returns string with the protein ids and the interaction weight */
  override def toString = "%s <--[%.1f]--> %s" format (protein1.id,weight,protein2.id)

}

/** Factory for an interaction */
object Interaction {
  /** new interaction with the given weight. */
  def apply(protein1:Protein, protein2:Protein, weight:Double = 1.0) =
    new Interaction(protein1, protein2, weight)

  /** new interaction with weight is +1 (interacting) or -1 (non-interacting) */
  def apply(protein1:Protein, protein2:Protein, isInteracting:Boolean) =
    new Interaction(protein1, protein2, if(isInteracting) +1.0 else -1.0)

  def unapply(interaction:Interaction) =
    Some((interaction.protein1,interaction.protein2,interaction.weight))  
}




/**
 * Factory for interactions mostly by sampling a protein-protein interaction
 * network.
 */
object InteractionFactory {

  /** creates a collection of positive and negative interactions randomly sampled from
   * a protein protein interaction network.
   * nPos, nNeg are the number of positives and negative interactions to create.
   * if nPos and nNeg are zero all positive interactions of the network
   * are taken and an equal number of negatives is generated */
  def apply(network:Network, nPos:Int, nNeg:Int):Seq[Interaction] =
    if(nPos == 0 && nNeg == 0) apply(network) else positives(network,nPos)++negatives(network,nNeg)

  /**
   * creates a collection of positive and negative interactions by taking all interactions within the network
   * with a weight below the given threshold as negatives and all above the threshold as positives.
   * There is no random element in this process. The same set interactions for a network is returned for each call.
   */
  def apply(network:Network, threshold:Double):Seq[Interaction] =
    network.interactions.map(i => Interaction(i.protein1, i.protein2, if(i.weight > threshold) +1.0 else -1.0))


  /** Returns all positive interactions within the given network and
   * a randomly sampled, equal number of negative interactions. */
  def apply(network:Network):Seq[Interaction] = {
    val n = network.interactions.size
    positives(network,n)++negatives(network,n)
  }

  /** collection of positive interactions randomly sampled from a PPI network */
  def positives(network:Network, n:Int):Seq[Interaction] =
    shuffle(network.interactions).take(n).map(i => Interaction(i.protein1, i.protein2, +1.0))

  /** collection of negative interactions randomly sampled from a PPI network */
  def negatives(network:Network, n:Int):Seq[Interaction] = {
    val rand = new Random(0)
    var interactions = HashSet[Interaction]()
    val proteins = network.toSeq
    val nProteins = proteins.length

    while(interactions.size < n) {
      val p1 = proteins(rand.nextInt(nProteins))
      val p2 = proteins(rand.nextInt(nProteins))
      if(!p1.isInteracting(p2))
        interactions += new Interaction(p1,p2,-1.0)
    }
    interactions.toSeq
  }

  /** collection of positive interactions where one interactor (but not the other)
   * is associated with a membrane, randomly sampled from a PPI network */
  def positivesMembrane(network:Network, n:Int):Seq[Interaction] = {
    def xor(x: Boolean, y: Boolean) = (x || y) && !(x && y)    
    def isMembraneIntrinsic(protein:Protein) =  protein.goids.contains("GO:0016021")
    def isMembrane(interaction:Interaction) =
      xor(isMembraneIntrinsic(interaction.protein1), isMembraneIntrinsic(interaction.protein2))
    shuffle(network.interactions).filter(isMembrane).take(n).
            map(i => Interaction(i.protein1, i.protein2, +1.0))
  }


  /** Reads node pairs and their geodesic distances from a file,
   *  sorts according to decreasing distance, takes the first n pairs
   *  and creates negative interactions for it.  */
  def negativesGeodesic(network:Network, n:Int, filepath:String):Seq[Interaction] = {
    var list = List[(Protein,Protein,Int)]()
    for(line <- Source.fromFile(filepath).getLines();
      e = line.trim.split(' '); d = e(2).toInt; if d > 3)
         list = (network(e(0)),network(e(1)),d)::list
    list.sortWith(_._3 > _._3).take(n).map(e=>Interaction(e._1,e._2,-1.0))
  }

  /** writes all pairs of network nodes and their geodesic distances to a file */
  def writeGeodesic(filepath:String, network:Network) = {
    val nodes = network.toSeq
    val len = nodes.length
    val f = new BufferedWriter(new FileWriter(new File(filepath)))
    val found = new HashSet[(Protein,Protein)]()

    for(i <- 0 until len; val n1 = nodes(i);
        j <- i+1 until len; val n2 = nodes(j);
        if !found.contains((n1,n2)) )  {
        println("%d %d %d".format(i,j,len))
        for( (node,dist) <- Dijkstra.shortestPath(n1,n2)) {
            found += ((n1,node))
            f.write("%s %s %.0f\n" format (n1.id, node.id, dist))
          }
    }
    f.close()
  }


  def main(args: Array[String]) {
    println("Running...")
    val network = NetworkReaderMel.read("data/mips_with_GOA_cc.txt")
    for(interaction <- InteractionFactory(network,10,10))
      println(interaction)
  }
}