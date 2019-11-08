package au.edu.imb.experiment.ppii

import au.edu.imb.ppii.go.{TermFilter, Term, Ontology}
import au.edu.imb.ppii.network.io.NetworkReaderEdgeList
import au.edu.imb.ppii.fgraph.io.FGraphReaderGO
import au.edu.imb.ppii.fgraph.{FNode, FGenerator, FGraph}
import io.Source
import au.edu.imb.ppii.network.{Protein, Network, Interaction, InteractionFactory}
import java.io.{BufferedWriter, FileWriter, File}
import au.edu.imb.ppii.network.annotation.{AnnotatorGO, AnnotatorGOWeb}
import au.edu.imb.ml.sampleset.{ASample, Nominal, SampleSet}

/**
 * Creates a sample set using a given ontology and encoding and writes it to
 * an output file.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 06.12.2010
 */

object SampleSetWriter {
  val labels = List("n","y")
  val annotator = new AnnotatorGO()
  type Encoding = (FGraph, Set[String], Set[String]) => Set[FNode]


  def createSampleSet(interactions:Seq[Interaction], fgraph:FGraph, enc:Encoding) = {
    def toSample(interaction:Interaction):ASample = {
      val classID = if(interaction.weight > 0.5) 1 else 0
      val nodes = enc(fgraph, interaction.protein1.goids.toSet, interaction.protein2.goids.toSet)
      FGenerator.toSample(fgraph, nodes, classID)
    }
    val fvalues = List("0", "1")
    val features = fgraph.map(node => Nominal(node.label,fvalues)).toSeq
    new SampleSet(features, interactions.map(toSample))
  }

  def writeSampleSet(sampleSet:SampleSet[ASample], filepath:String) {
    val writer = new BufferedWriter(new FileWriter(new File(filepath)))
    writer.write(sampleSet.features.map(_.name).mkString("class "," ", "\n"))
    for(sample <- sampleSet)
      writer.write(sample.map(_.toString).mkString(sample.classID+" ", " ", "\n"))    
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


  def run(inpath:String, outpath:String) {
    //val encoding = FGenerator.ac
    val encoding:Encoding = FGenerator.ulca

    //val ontology = Ontology.cellularComponent
    //val ontology = Ontology.biologicalProcess
    val ontology = Ontology.molecularFunction

    println("loading network..."+inpath)
    val network = loadNetwork(inpath)
    println("network: "+network.name+"  size="+network.size)

    print("creating ontology...")
    val goids = Set()++network.flatMap(_.goids)
    val fgraph = createFGraph(ontology, goids)
    println("goids="+goids.size+" DAG size="+fgraph.size)

    val n = network.interactions.size
    println("creating interactions... pos="+n)
    val interactions = InteractionFactory(network, n, n)

    println("creating sampleset...")
    val sampleSet = createSampleSet(interactions, fgraph, encoding)

    println("writing sampleset..."+outpath)
    writeSampleSet(sampleSet, outpath)

    println("finished")
  }
  

  def main(args: Array[String]) {
    run("data/SC_string.txt", "c:/Maet/Temp/SC_string_MF.dat")
  }
}