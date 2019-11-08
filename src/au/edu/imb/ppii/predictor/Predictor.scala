package au.edu.imb.ppii.predictor

import scala.Math.exp
import au.edu.imb.ppii.go._
import au.edu.imb.ppii.network._
import au.edu.imb.ppii.network.io.NetworkReaderMel
import au.edu.imb.logreg.LogReg
import cern.colt.matrix.{DoubleFactory2D,DoubleFactory1D}



class Predictor(dag:GoDAG) {
  
  /** Lowest common ancestors for two proteins and their GO Ids */
  private def LCAs(protein1:Protein, protein2:Protein) = 
      dag.LCAs(protein1.goids.map(dag(_)), protein2.goids.map(dag(_)))

  /** All Common ancestors for two proteins and their GO Ids */
  private def ACAs(protein1:Protein, protein2:Protein) = 
    dag.ancestors(protein1.goids.map(dag(_))) & dag.ancestors(protein2.goids.map(dag(_)))
            
  /** trains the predictor with the given interactions */
  def train(interactions:Iterable[Interaction]) = {
    /** ancestral samples as collection of tuples (node,protein1,protein2,weight) */
    val ancestralSamples = interactions.flatMap{case Interaction(p1,p2,w) => ACAs(p1,p2).map((_,p1,p2,w))}
          
    /** returns interactions to train the given node */
    def nodeInteractions(node:Node) =
      ancestralSamples.filter(_._1 == node).map{case (n,p1,p2,w) => Interaction(p1,p2,w)}
      
    def trainNode(node:Node):Unit = {
      val nis = nodeInteractions(node)
      val pos = nis.count(_.weight > 0.5)
      val neg = nis.count(_.weight < 0.5)
      
      if(pos==0 || neg == 0) {
        printf("*****: %s l:%d p:%d n:%d\n",node.term.id,node.level,pos,neg)
        return
      }
      printf("train: %s l:%d p:%d n:%d\n",node.term.id,node.level,pos,neg)
      
      val rows = nis.size
      val cols = node.children.size+2 // + bias_weight + activation_weight
      val X = DoubleFactory2D.dense.make(rows,cols)
      val y = DoubleFactory1D.dense.make(rows)
      for((interaction,i) <- nis.zipWithIndex) {
        y.setQuick(i, if(interaction.weight>0) +1.0 else -1.0)
        X.setQuick(i,0,1.0) // bias
        X.setQuick(i,1,node.activation(interaction)) // activation
        for((output,j)<-node.children.map(_.output(interaction)).zipWithIndex)
          X.setQuick(i,j+2,output)
      }
      val (loglik, w) = LogReg.estimate(X,y,false)
      node.bweight = w.getQuick(0)    // bias weight 
      node.aweight = w.getQuick(1)    // activation weight
      node.childLinks.zipWithIndex.foreach{case (l,i) => l.weight=w.getQuick(i+2)}
    }
    
    def trainDAG(node:Node):Unit = {
      node.children.foreach(trainDAG)
      //println("  %s %3d %3d".format(node.id,node.level,node.children.length))
      trainNode(node)
    }
    
    trainDAG(dag.head)
    //trainNode(dag.last)
    
    //val nss = nodeSampleSet(sampleSet, dag("GO:0000322"))
       
    /*
    var good = 0
    var posSum = 0
    for( node <- dag ) {
      val nss = nodeSampleSet(node)
      val pos = nss.count(_.weight > 0)
      posSum += pos
      if(pos > 0) {
        good += 1
        printf("  %s ", node.id)
        printf("  %3d", node.level)
        printf("  %3d", node.parents.length)
        printf("  %3d", node.children.length)
        printf("  %4d", pos)
        printf("  %s ", node.name)
        println()
      }      
    }
    println("DAG size: "+dag.size)
    println("pos sum : "+posSum)
    println("good    : "+good)
    */
    
    //Problems:
    // 1) training data only for 10% of nodes 
    //    => default weights for other nodes
    //    => shrink DAG / extrapolate to other nodes, eg. all ancestors
    // 2) extremely unbalanced data
    //    => find best negatives

    
  }
  
  
  def predict(interaction:Interaction) = {
    // calc LAC for the two proteins/GOID-lists in sample
    // calc activation for LCA and return it
  }
}


object Predictor {
  
  def main(args: Array[String]) {
    println("loading ppi data...")
    val net = NetworkReaderMel.read("data/mips_with_GOA_cc.txt")
    println("create sample set...")
    val interactions = net.interactions  
    val pos = interactions.count(_.weight > 0.5)
    val neg = interactions.count(_.weight < 0.5)
    printf("interactions: p:%d n:%d  sum:%d\n",pos,neg,pos+neg)
    
    // goids for reduced DAG that covers PPI sample set
    val goids = Set()++net.flatMap(_.goids)
    // create only nodes where terms are existing for in sample set 
    val termFilter =  new TermFilter("go") {
      def apply(term:Term) = goids.contains(term.id)
    }
    println("loading ontology...")
    val ontology = new Ontology(OboParser("data/go.obo"),termFilter)
    println("create dag...")
    val dag = new GoDAG(ontology)
    val predictor = new Predictor(dag)
    println("training...")
    predictor.train(interactions)
    println("finished.")
  }
}
