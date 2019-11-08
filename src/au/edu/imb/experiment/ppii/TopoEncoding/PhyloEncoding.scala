package au.edu.imb.experiment.ppii.TopoEncoding

import au.edu.imb.ml.result.CResult
import au.edu.imb.ml.classifier.{Weka, Mean => CMean}
import au.edu.imb.ml.classifier.Weka._
import au.edu.imb.ml.Classifier
import au.edu.imb.ppii.network.Protein._
import au.edu.imb.ppii.network.io.NetworkReaderEdgeList
import au.edu.imb.ml.sampleset.{ASample, Nominal, SampleSet}
import au.edu.imb.ppii.network.annotation.{AnnotatorPhyloProfile}
import au.edu.imb.ppii.fgraph.io.{FGraphReaderPhyloXML}
import au.edu.imb.ppii.fgraph.{AcceptLabelsFilter, FNode, FGenerator, FGraph}
import au.edu.imb.ppii.network.{Protein, Interaction, InteractionFactory}
import au.edu.imb.ml.xperiment._

/**
 * Explores different methods to encode the information within a phylogenetic tree
 * as a feature vector to infer protein protein interaction.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 16/07/2010
 */

object PhyloEncoding extends XPerimentBinaryClassifier {
  val name = "PhyloEncoding"
  type Encoding = (FGraph, Set[String], Set[String]) => Set[FNode]
                                              
  val folds = 10
  val runs  = 1
  val (interactions, nodeLabels, network) = init(0,0)

  lazy val phylo = createFGraph()


  add(XVariable("encoding", Seq[(String,Encoding)](
    ("AC  ", FGenerator.ac),
    ("SPS ", FGenerator.sps),
//    ("SPA ", FGenerator.spa),
    ("AL  ", FGenerator.al),
//    ("AA  ", FGenerator.aa),
//      ("OLCA", FGenerator.olca),
//    ("LCA ", FGenerator.lca),
//    ("WLCA", FGenerator.wlca),
    ("ULCA", FGenerator.ulca)
    ):_*
  ))
  add(XVariable("ontology",
    ("PHYLO ", phylo)
  ))
  add(XVariable("classifier",
    ("NB   ", Weka(NaiveBayes, "", labels))
//    ("RF3  ", Weka(RandomForest, "-I 400", labels))
  ))


  def evaluate = {
    val classifier = value[Classifier]("classifier")
    val enc = value[Encoding]("encoding")
    val fgraph = value[FGraph]("ontology")
    val sampleSet = createSampleSet(enc, fgraph)
    XValidation(sampleSet,folds,runs).map(classifier.evaluate)
  }                                  

  def createSampleSet(enc:Encoding,fgraph:FGraph) = {
    def toSample(interaction:Interaction):ASample = {
      def labels(protein:Protein) = protein.valuesOrElse(P_PROFILE).toSet
      val classID = if(interaction.weight > 0.5) 1 else 0
      val nodes = enc(fgraph, labels(interaction.protein1), labels(interaction.protein2))
      FGenerator.toSample(fgraph, nodes, classID)
    }
    val fvalues = List("0", "1")
    val features = fgraph.map(node => Nominal(node.label,fvalues)).toSeq
    new SampleSet(features, interactions.map(toSample))
  }

  def createFGraph() = {
    val filepath = "f:/PPII/Data/PhyloProfiles/taxomonicTreeNCBI.xml"
    val graph = (new FGraphReaderPhyloXML(AcceptLabelsFilter(nodeLabels))).read(filepath)
    printf("Phylogentic tree,  size: %s\n", graph.size)
    graph
  }

  def init(nPos:Int, nNeg:Int) = {
    println("Loading network...")
    val network = NetworkReaderEdgeList.read("data/SC_park.txt")
    //val network = NetworkReaderEdgeList.read("data/SC_string.txt")
    //val network = NetworkReaderEdgeList.read("data/SC_biogrid.txt")
    //val network = NetworkReaderEdgeList.read("data/EC_string.txt")
    printf("  Network: %s\n", network.name)
    printf("  proteins: %d  interaction %d\n", network.size, network.interactions.size)
    val filepath = "data/yeast.profile"
    printf("Annotate phylogenetic profiles ...\n  %s\n",filepath)
    (new AnnotatorPhyloProfile(filepath)).annotate(network)
    println("Create interaction set...")
    val interactions = InteractionFactory(network, nPos, nNeg)
    printf("  interactions: %d\n", interactions.size)
    val nodeLabels = Set()++network.flatMap(_.valuesOrElse(P_PROFILE))
    (interactions, nodeLabels, network)
  }



  def main(args: Array[String]) {
    println("running...")
    run()
    summary("encoding","ontology","classifier")
    summary("classifier")
    summary("encoding")
    summary("ontology")
    println("finished.")
  }
}