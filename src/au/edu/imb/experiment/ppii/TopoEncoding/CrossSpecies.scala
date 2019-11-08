package au.edu.imb.experiment.ppii.TopoEncoding

import au.edu.imb.ml.result.CResult
import au.edu.imb.ml.xperiment.{XVariable, Mean,Std, XValidation, XPeriment}
import au.edu.imb.ml.classifier.{Weka, Mean => CMean}
import au.edu.imb.ml.classifier.Weka._
import au.edu.imb.ml.Classifier
import au.edu.imb.ppii.go.{TermFilter, Term, Ontology}
import au.edu.imb.ppii.fgraph.io.FGraphReaderGO
import au.edu.imb.ml.sampleset.{ASample, Nominal, SampleSet}
import au.edu.imb.ppii.fgraph.{FNode, FGenerator, FGraph}
import au.edu.imb.ppii.network.{Network, Interaction, InteractionFactory}
import au.edu.imb.ppii.network.annotation.{EvidenceFilterNotIFI, AnnotatorGO}
import au.edu.imb.ppii.network.io.{NetworkReaderWeightedEdgeList, NetworkReaderEdgeList}


/**
 * Trains a classifier on one data set and then measures the performance
 * on a different data set.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 20.10.2010
 */

object CrossSpecies {
  val labels = List("n","y")
  val annotator = new AnnotatorGO(EvidenceFilterNotIFI)
  type Encoding = (FGraph, Set[String], Set[String]) => Set[FNode]


  def createSampleSet(network:Network, fgraph:FGraph, enc:Encoding) = {
    val n = network.size
    val interactions = InteractionFactory(network, 0.0)
    def toSample(interaction:Interaction):ASample = {
      val classID = if(interaction.weight > 0.0) 1 else 0
      val nodes = enc(fgraph, interaction.protein1.goids.toSet, interaction.protein2.goids.toSet)
      FGenerator.toSample(fgraph, nodes, classID)
    }
    val fvalues = List("0", "1")
    val features = fgraph.map(node => Nominal(node.label,fvalues)).toSeq
    new SampleSet(features, interactions.map(toSample))
  }


  def createFGraph(termFilter:TermFilter, goids:Set[String]) = {
    class TermFilterUsed extends TermFilter(termFilter.name) {
      def apply(term:Term) = goids.contains(term.id) && termFilter(term)
    }
    val graph = (new FGraphReaderGO(new TermFilterUsed)).read("data/go.obo")
    graph
  }


  def loadNetwork(filepath:String) = {
    val network = NetworkReaderWeightedEdgeList.read(filepath)
    annotator.annotate(network)
    network
  }

  def evaluate(trainData:String, testData:String) = {
    //val encoding = FGenerator.ac
    val encoding:Encoding = FGenerator.ulca

    val classifier  = Weka(NaiveBayes, "", labels)
    //val classifier  = Weka(RandomForest, "-2 100 -K 200", labels)

    //val ontology = Ontology.biologicalProcess
    val ontology = Ontology.molecularFunction
    //val ontology = Ontology.cellularComponent

    println("Loading networks...")
    val trainNet = loadNetwork("data/"+trainData+".txt")
    val testNet = loadNetwork("data/"+testData+".txt")
    println("train network: "+trainNet.name+"  size="+trainNet.size)
    println("test network: "+testNet.name+"  size="+testNet.size)

    println("creating ontology...")
    val goids = Set()++trainNet.flatMap(_.goids)++testNet.flatMap(_.goids)
    val fgraph = createFGraph(ontology, goids)
    println("goids="+goids.size+" DAG size="+fgraph.size)

    println("creating samplesets...")
    val trainSet = createSampleSet(trainNet, fgraph, encoding)
    val testSet = createSampleSet(testNet, fgraph, encoding)
    println("train="+trainSet.size+" test="+testSet.size)

    println("training...")
    classifier.train(trainSet)
    println("testing...")
    classifier.test(testSet)
  }
  

  def main(args: Array[String]) {
    val data = Array("EC_string","SP_string","HS_string","SC_string",
                     "DM_string","AT_string","MM_string")
    //val data = Array("SC_string","HS_string")
    val n = data.length
    val results = Array.ofDim[Double](n,n)
    for(i <- 0 until n; j <- 0 until n)
       results(i)(j) = evaluate(data(i),data(j)).auc(1)

    println(" "*20+data.mkString(" "))
    for(i <- 0 until n) {
      printf("%20s ",data(i))
      for(j <- 0 until n)
        printf("%.2f ",results(i)(j))
      println()
    }
  }
}