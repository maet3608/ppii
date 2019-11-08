package au.edu.imb.experiment.ppii.TopoEncoding

import scala.math.log
import scala.collection.mutable.Map
import au.edu.imb.ppii.network.annotation.{AnnotatorKeyword, AnnotatorGO}
import au.edu.imb.ml.classifier.{Weka, Mean => CMean}
import au.edu.imb.ml.classifier.Weka._
import au.edu.imb.ppii.network.io.NetworkReaderEdgeList
import au.edu.imb.ppii.fgraph.io.FGraphReaderGO
import au.edu.imb.ml.sampleset.{ASample, Nominal, SampleSet}
import au.edu.imb.ppii.fgraph.{FNode, FGenerator, FGraph}
import au.edu.imb.ml.xperiment.XValidation
import au.edu.imb.ppii.go.{OboParser, TermFilter, Term, Ontology}
import au.edu.imb.ppii.network.{Protein, Network, Interaction, InteractionFactory}
import au.edu.imb.ppii.network.Protein._
import au.edu.imb.ppii.go.Ontology._

/**
 * Performs GO term enrichment analysis for predicted interactions.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 21.10.2010
 */


/**
 * Wraps a sample to add the interaction the sample is created from.
 */
class ISample(sample:ASample, val interaction:Interaction) extends ASample {
  def classID = sample.classID
  def apply(idx:Int) = sample(idx)
  def iterator = sample.iterator
  def length = sample.length
}


object EnrichmentAnalysis {
  val labels = List("n","y")
  val annotatorGO = new AnnotatorGO()
  val annotatorKW = new AnnotatorKeyword()
  type Encoding = (FGraph, Set[String], Set[String]) => Set[FNode]

  def createSampleSet(network:Network, fgraph:FGraph, enc:Encoding) = {
    val n = network.size
    val interactions = InteractionFactory(network, n, n)

    def toSample(interaction:Interaction):ISample = {
      val classID = if(interaction.weight > 0.5) 1 else 0
      val nodes = enc(fgraph, interaction.protein1.goids.toSet, interaction.protein2.goids.toSet)
      new ISample(FGenerator.toSample(fgraph, nodes, classID), interaction)
    }
    def features = {
      val values = List("0", "1")
      val l = fgraph.size
      (0 until l).map(i => Nominal("F"+i, values))
    }
    new SampleSet(features, interactions.map(toSample))
  }

  def createFGraph(termFilter:TermFilter, goids:Set[String]) = {
    class TermFilterUsed extends TermFilter(termFilter.name) {
      def apply(term:Term) = goids.contains(term.id) && termFilter(term)
    }
    val graph = (new FGraphReaderGO(new TermFilterUsed)).read("data/go.obo")
    graph
  }


  def resultsFilter(results:Iterable[(Interaction,Int,Int)],tar:Int, pred:Int) =
     results.filter{case (i,t,p) => t==tar && p==pred}.map(t => t._1)

  def goCounterMap(ontology:Ontology, interactions:Iterable[Interaction]) = {
    var map = Map[Term,Int]() ++= ontology.map(term => (term, 0))
    def inc(term:Term) = map(term) += 1
    def count(protein:Protein) =
      protein.goids.filter(ontology.contains).foreach(id => ontology.ancestors(id).foreach(inc))
    interactions.foreach{i => count(i.protein1); count(i.protein2)}
    map.filter{case (term,n) => n > 0}
  }

  def showRanking(tcm:Map[Term,Int], all:Map[Term,Int], termFilter:TermFilter, n:Int) = {
    val termICs = tcm.filter{case (term,n) => termFilter(term)}.
        map{case (term,n) => (term, n, all(term), -log(n.toDouble/all(term))) }.toSeq.sortBy(-_._4).take(n)
    for((term,n,an,ic) <- termICs)
      printf("%5.2f  (%d, %d)\t %s(%d)\t %s\n", ic, n, an, term.id, term.level, term("name"))
  }

  def kwCounterMap(interactions:Iterable[Interaction]) = {
    var map = Map[String,Int]()
    def inc(kw:String) = map(kw) = map.getOrElse(kw,0) + 1
    def count(protein:Protein) =  protein.valuesOrElse(P_KEYWORDS).foreach(inc)
    interactions.foreach{i => count(i.protein1); count(i.protein2)}
    map.filter{case (kw,n) => n > 0}
  }

  def showRanking(kcm:Map[String,Int], all:Map[String,Int], n:Int) = {
    val kwICs = kcm.
        map{case (kw,n) => (kw, n, all(kw), -log(n.toDouble/all(kw)))}.toSeq.sortBy(-_._4).take(n)
    for((kw,n,an,ic) <- kwICs)
      printf("%5.2f  (%d, %d)\t %s\n", ic, n, an, kw)
  }

  def evaluate(filepath:String, termFilter:TermFilter) = {
    val encoding:Encoding = FGenerator.ulca
    val classifier = Weka(NaiveBayes, "", List("n","y"))
    println("loading network...")
    val network = NetworkReaderEdgeList.read(filepath)
    println("annotating...")
    annotatorKW.annotate(network)
    annotatorGO.annotate(network)

    println("loading ontology...")
    val goids = Set()++network.flatMap(_.goids)
    val fgraph = createFGraph(termFilter, goids)

    println("creating sampleset...")
    val sampleSet = createSampleSet(network, fgraph, encoding)

    println("training...")
    val results = XValidation(sampleSet,10,1).flatMap{case (train,test) =>
      classifier.train(train); test.map(s => (s.interaction, s.classID, classifier.predict(s)))}

    (results, sampleSet)
  }

  def analyseGO(filepath:String, termFilter:TermFilter) = {
    val (results,sampleSet) = evaluate(filepath, termFilter)

    println("analysing...")
    val ontology = new Ontology(OboParser("data/go.obo"),Ontology.notObsolete)
    val fps = goCounterMap(ontology, resultsFilter(results, 0,1))
    val fns = goCounterMap(ontology, resultsFilter(results, 1,0))
    val tps = goCounterMap(ontology, resultsFilter(results, 1,1))
    val tns = goCounterMap(ontology, resultsFilter(results, 0,0))
    val all = goCounterMap(ontology, sampleSet.map(s => s.interaction))

    val tcms = List(("FP",fps),("FN",fns),("TP",tps),("TN",tns))
    println("-------"+termFilter.name.toUpperCase+"-"*40)
    for((name,tcm) <-tcms) {
      println("--"+name+"-"*30)
      showRanking(tcm,all,termFilter,30)
    }
    println()
  }

  def analyseKW(filepath:String, termFilter:TermFilter) = {
    val (results,sampleSet) = evaluate(filepath, termFilter)

    println("analysing...")
    val fps = kwCounterMap(resultsFilter(results, 0,1))
    val fns = kwCounterMap(resultsFilter(results, 1,0))
    val tps = kwCounterMap(resultsFilter(results, 1,1))
    val tns = kwCounterMap(resultsFilter(results, 0,0))
    val all = kwCounterMap(sampleSet.map(s => s.interaction))

    val kcms = List(("FP",fps),("FN",fns),("TP",tps),("TN",tns))
    for((name,kcm) <-kcms) {
      println("--"+name+"-"*30)
      showRanking(kcm,all,30)
    }
    println()
  }

  
  def main(args: Array[String]) {
    val filepath  = "data/SC_string.txt"

    for(termFilter <- List(cellularComponent, biologicalProcess, molecularFunction)) {
      println("-------"+termFilter.name.toUpperCase+"-"*40)
      //analyseGO(filepath, termFilter)
      analyseKW(filepath, termFilter)
    }
  }
}