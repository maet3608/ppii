package au.edu.imb.experiment.ppii.TopoEncoding

import au.edu.imb.ml.result.CResult
import au.edu.imb.ml.classifier.{Weka, Mean => CMean}
import au.edu.imb.ml.classifier.Weka._
import au.edu.imb.ml.Classifier
import au.edu.imb.ppii.network.Protein._
import au.edu.imb.ppii.network.io.NetworkReaderEdgeList
import au.edu.imb.ml.sampleset.{ASample, Nominal, SampleSet}
import au.edu.imb.ppii.fgraph.{AcceptLabelsFilter, FNode, FGenerator, FGraph}
import au.edu.imb.ppii.network.{Protein, Interaction, InteractionFactory}
import au.edu.imb.ppii.network.annotation.{AnnotatorInterpro}
import au.edu.imb.ppii.fgraph.io.{FGraphReaderInterpro}
import au.edu.imb.ml.xperiment._

/**
 * Utilize protein domain information infer protein protein interaction.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 16/07/2010
 */

object DomainEncoding extends XPerimentBinaryClassifier {
  val name = "DomainEncoding"
  type Encoding = (FGraph, Set[String], Set[String]) => Set[FNode]
                                              
  val folds = 10
  val runs  = 1
  val (interactions, nodeLabels, network) = init(1731,1731)

  lazy val domains = createFGraph()


  add(XVariable("encoding", Seq[(String,Encoding)](
    ("AC  ", FGenerator.ac),
//    ("SPS ", FGenerator.sps),
//    ("SPA ", FGenerator.spa),
    ("AL  ", FGenerator.al)
//    ("AA  ", FGenerator.aa),
//      ("OLCA", FGenerator.olca),
//    ("LCA ", FGenerator.lca),
//    ("WLCA", FGenerator.wlca),
//    ("ULCA", FGenerator.ulca)
    ):_*
  ))
  add(XVariable("ontology",
    ("Domain ", domains)
  ))
  add(XVariable("classifier",
    ("NB   ", Weka(NaiveBayes, "", labels)),
    ("RF3  ", Weka(RandomForest, "-I 400", labels))
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
      def labels(protein:Protein) = protein.valuesOrElse(P_INTERPRO).toSet
      val classID = if(interaction.weight > 0.5) 1 else 0
      val nodes = enc(fgraph, labels(interaction.protein1), labels(interaction.protein2))
      FGenerator.toSample(fgraph, nodes, classID)
    }
    val fvalues = List("0", "1")
    val features = fgraph.map(node => Nominal(node.label,fvalues)).toSeq
    new SampleSet(features, interactions.map(toSample))
  }

  def createFGraph() = {
    val filepath = "f:/PPII/Data/Domain/InterPro/ParentChildTreeFile.txt"
    val graph = (new FGraphReaderInterpro(AcceptLabelsFilter(nodeLabels))).read(filepath)
    display("Interpro tree,  size: %s\n", graph.size)
    graph
  }

  def init(nPos:Int, nNeg:Int) = {
    println("Loading network...")
    //val network = NetworkReaderEdgeList.read("data/SC_park.txt")
    //val network = NetworkReaderEdgeList.read("data/SC_string.txt")
    //val network = NetworkReaderEdgeList.read("data/SC_biogrid.txt")
    val network = NetworkReaderEdgeList.read("data/EC_string.txt")
    display("  Network: %s\n", network.name)
    display("  proteins: %d  interaction %d\n", network.size, network.interactions.size)
    display("Annotate interpro domains")
    (new AnnotatorInterpro()).annotate(network)
    println("Create interaction set...")
    val interactions = InteractionFactory(network, nPos, nNeg)
    display("  interactions: %d\n", interactions.size)
    val nodeLabels = Set()++network.flatMap(_.valuesOrElse(P_INTERPRO))
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