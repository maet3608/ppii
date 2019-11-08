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
import au.edu.imb.ppii.fgraph.io.{FGraphReaderLoci, FGraphReaderPhyloXML}
import au.edu.imb.ppii.network.annotation.{AnnotatorLoci, AnnotatorPhyloProfile}
import au.edu.imb.ml.xperiment._

/**
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 16/07/2010
 */

object LociEncoding extends XPerimentBinaryClassifier {
  val name = "LociEncoding"
  type Encoding = (FGraph, Set[String], Set[String]) => Set[FNode]
                                              
  val folds = 10
  val runs  = 1
  val (interactions, nodeLabels, network) = init(0,0)

  lazy val loci = createFGraph()


  add(XVariable("encoding", Seq[(String,Encoding)](
    ("AC  ", FGenerator.ac),
    ("AL  ", FGenerator.al),
    ("SPS ", FGenerator.sps)
//    ("SPA ", FGenerator.spa)
    ):_*
  ))
  add(XVariable("ontology",
    ("LOCI ", loci)
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
      def labels(protein:Protein) = protein.valuesOrElse(P_LOCUS).toSet
      val classID = if(interaction.weight > 0.5) 1 else 0
      val nodes = enc(fgraph, labels(interaction.protein1), labels(interaction.protein2))
      FGenerator.toSample(fgraph, nodes, classID)
    }
    val fvalues = List("0", "1")
    val features = fgraph.map(node => Nominal(node.label,fvalues)).toSeq
    new SampleSet(features, interactions.map(toSample))
  }

  def createFGraph() = {
    val (filepath,isCircular) = ("data/EC.loci", true)
    //val (filepath,isCircular) = ("data/SC.loci", false)
    val graph = (new FGraphReaderLoci(isCircular)).read(filepath)
    if(verbose) display("Locus graph,  size: %s\n", graph.size)
    graph
  }

  def init(nPos:Int, nNeg:Int) = {
    display("Loading network...")
    //val network = NetworkReaderEdgeList.read("data/SC_park.txt")
    //val network = NetworkReaderEdgeList.read("data/SC_string.txt")
    val network = NetworkReaderEdgeList.read("data/EC_string.txt")
    //val network = NetworkReaderEdgeList.read("data/SC_biogrid.txt")
    display("  Network: %s\n", network.name)
    display("  proteins: %d  interaction %d\n", network.size, network.interactions.size)
    val filepath = "data/ecoli.loci"
    display("Annotate loci ...\n  %s\n",filepath)
    (new AnnotatorLoci(filepath)).annotate(network)
    display("Create interaction set...")
    val interactions = InteractionFactory(network, nPos, nNeg)
    display("  interactions: %d\n", interactions.size)
    val nodeLabels = Set()++network.flatMap(_.valuesOrElse(P_LOCUS))
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

