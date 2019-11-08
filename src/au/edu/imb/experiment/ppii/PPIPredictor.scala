package au.edu.imb.experiment.ppii

import au.edu.imb.ml.{Classifier, CertaintyFactors}
import au.edu.imb.ml.classifier.{Weka, Mean => CMean}
import au.edu.imb.ml.classifier.Weka._
import au.edu.imb.ppii.go.{TermFilter, Term, Ontology}
import au.edu.imb.ppii.network.io.NetworkReaderEdgeList
import au.edu.imb.ppii.fgraph.io.FGraphReaderGO
import au.edu.imb.ml.sampleset.{ASample, Nominal, SampleSet}
import au.edu.imb.ppii.fgraph.{FNode, FGenerator, FGraph}
import io.Source
import au.edu.imb.ppii.network.{Protein, Network, Interaction, InteractionFactory}
import java.io.{BufferedWriter, FileWriter, File}
import au.edu.imb.ppii.network.annotation.{AnnotatorGOWeb,AnnotatorGO}

/**
 * Trains a classifier on a training set and predicts protein-protein interactions
 * on a given set.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 25.11.2010
 */

object PPIPredictor {
  val labels = List("n","y")
  val annotator = new AnnotatorGOWeb()
  type Encoding = (FGraph, Set[String], Set[String]) => Set[FNode]


  def createSampleSet(network:Network, fgraph:FGraph, enc:Encoding) = {
    val n = network.interactions.size
    val interactions = InteractionFactory(network, n, n)

    def toSample(interaction:Interaction):ASample = {
      val classID = if(interaction.weight > 0.5) 1 else 0
      val nodes = enc(fgraph, interaction.protein1.goids.toSet, interaction.protein2.goids.toSet)
      FGenerator.toSample(fgraph, nodes, classID)
    }
    val fvalues = List("0", "1")
    val features = fgraph.map(node => Nominal(node.label,fvalues)).toSeq
    new SampleSet(features, interactions.map(toSample))
  }

  def predict(proteins:Seq[Protein], classifier:Classifier with CertaintyFactors, fgraph:FGraph, enc:Encoding) {
    val writer = new BufferedWriter(new FileWriter(new File("prediction.txt")))
    for(i <- 0 until proteins.size; j <- i until proteins.size) {
      val (protein1, protein2) = (proteins(i), proteins(j))
      val nodes = enc(fgraph, protein1.goids.toSet, protein2.goids.toSet)
      val cfs = classifier.cfs(FGenerator.toSample(fgraph, nodes, 0))
      val classID = cfs.indexOf(cfs.max)
      if(classID > 0)
        writer.write("%s\t%s\t%.2f\n" format (protein1.id, protein2.id, cfs(classID)))
    }
    writer.close()
  }

  def createFGraph(termFilter:TermFilter, goids:Set[String]) = {
    class TermFilterUsed extends TermFilter(termFilter.name) {
      def apply(term:Term) = goids.contains(term.id) && termFilter(term)
    }
    val graph = (new FGraphReaderGO(new TermFilterUsed)).read("data/go.obo")
    graph
  }

  def loadNetwork(filepath:String) = {
    val network = NetworkReaderEdgeList.read(filepath)
    annotator.annotate(network)
    network
  }

  def loadProteins(filepath:String) =
    Source.fromFile(filepath).getLines().toSeq.map(id => annotator.annotate(id.trim))


  def run(trainData:String, testData:String) = {
    //val encoding = FGenerator.ac
    val encoding:Encoding = FGenerator.ulca

    //val classifier  = Weka(NaiveBayes, "", labels)
    val classifier  = Weka(RandomForest, "-I 400", labels)

    val ontology = Ontology.biologicalProcess

    println("loading network...")
    val trainNet = loadNetwork("data/"+trainData+".txt")
    val testProteins = loadProteins("data/"+testData+".txt")
    println("train network: "+trainNet.name+"  size="+trainNet.size)
    trainNet.foreach(p => println(p.annotations))
    println("test proteins: size="+testProteins.size)
    testProteins.foreach(p => println(p.annotations))

    print("creating ontology...")
    val goids = Set()++trainNet.flatMap(_.goids)++testProteins.flatMap(_.goids)
    val fgraph = createFGraph(ontology, goids)
    println("goids="+goids.size+" DAG size="+fgraph.size)

    println("creating samplesets...")
    val trainSet = createSampleSet(trainNet, fgraph, encoding)

    println("training...")
    classifier.train(trainSet)
    println("predicting...")
    predict(testProteins, classifier, fgraph, encoding)
  }
  

  def main(args: Array[String]) {
    run("AT_string","AT_predict")
  }
}