package au.edu.imb.go2ppi

import scala.io.Source
import au.edu.imb.ml.xperiment.{XValidation}
import au.edu.imb.ml.classifier.{Weka }
import au.edu.imb.ml.classifier.Weka._
import au.edu.imb.ppii.go.{TermFilter, Term,  Ontology}
import au.edu.imb.ppii.network.{Network, Protein, Interaction, InteractionFactory}
import au.edu.imb.ppii.network.io.NetworkReaderEdgeList
import au.edu.imb.ppii.fgraph.io.FGraphReaderGO
import au.edu.imb.ml.sampleset.{ASample, Nominal, SampleSet}
import au.edu.imb.ppii.fgraph.{FNode, FGenerator, FGraph}
import au.edu.imb.ml.{Inducer}
import au.edu.imb.ppii.network.annotation.{AnnotatorGOWeb, AnnotatorGOfromFile}
import java.io.{FileInputStream, BufferedWriter, FileWriter, File}
import java.util.Properties

/**
 * Command line application to predict protein-protein interactions from Gene Ontology annotation using a
 * machine learning classifier.
 *
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 13/04/11
 */


/** Returns GO annotations for protein identifiers (accession numbers) */
object Id2go {
  def main(args:Array[String]) {
    println("id2go")
    if(args.length != 2) {
      println("USAGE:\nid2go id_file go_term_file")
      sys.exit(0)
    }
    println("annotating...")
    val (infile,outfile) = (args(0),args(1))
    val annotator = new AnnotatorGOWeb()
    val writer = new BufferedWriter(new FileWriter(outfile))
    val ids = Source.fromFile(infile).getLines().toList.flatMap(_.split("\\s+").toList).toSet
    for(protein <- ids.map(id => new Protein(id.trim))) {
      println(protein.id)
      annotator.annotate(protein)
      writer.write(protein.id+protein.goids.mkString("\t",",","\n"))
    }
    writer.close()
    println("finished.")
  }
}

object GenerateFilters {
  def main(args:Array[String]) {
    println("genFilters")
    if(args.length != 3) {
      println("USAGE:\ngen_filters anno_file nr_groups output_path\n")
      println("EXAMPLE:\ngen_filters test/test.anno 5 .")
      sys.exit(0)
    }
    println("creating...")
    val (infile,n,outpath) = (args(0), args(1).toInt, args(2))
    val ids = Source.fromFile(infile).getLines().map(_.split('\t')(0)).toList
    println("  annotation loaded")
    val pairs = (for( (id1,i) <- ids.iterator.zipWithIndex;
                      (id2,j) <- ids.iterator.zipWithIndex if i<j ) yield (id1,id2))
    val size = ids.size*(ids.size-1)/(2*n)+1
    for( (group,i) <- pairs.grouped(size).zipWithIndex ) {
        println("  writing filter file %d" format (i+1))
        val writer = new BufferedWriter(new FileWriter("%s/filter%d.fil" format (outpath,i+1)))
        group.foreach{case (id1,id2) => writer.write(id1+"\t"+id2+"\n")}
        writer.close()
    }
    println("finished.")
  }
}


/** Implements the command line interface for the predictor */
object Go2ppi {
  type Encoding = (FGraph, Set[String], Set[String]) => Set[FNode]
  var otime = System.currentTimeMillis()
  val properties = new Properties()
  val version = "1.06"
  val usage =
    """
    |USAGE:   go2ppi <configuration_file> {parameter_overrides}
    |EXAMPLE: go2ppi go2ppi.cfg self-test=NO sub-ontology=BP,CC
    |See configuration example file for details.
    """.stripMargin('|')


  def evalCommandLine(args:Array[String]) {
    output("config file: "+args.mkString(" "))
    if(args.length < 1)
      sys.error("Path to configuration file is missing!")
    val fis = new FileInputStream(args(0))
    properties.load(fis)
    fis.close()
    args.drop(1).map(_.split('=').map(_.trim)).foreach{ case Array(k,v) => properties.setProperty(k,v) }
    output("SETTINGS")
    properties.keySet.toArray.foreach{ k => output("  %-15s = '%s'", k, cfg(k.toString)) }
  }

  def cfg(key:String):String = properties.getProperty(key).trim
  def hasCfg(key:String):Boolean = properties.containsKey(key)

  def createFGraphs(ontologyFile:String, goids:Set[String]) = {
    var idx = 0
    class TermFilterUsed(termFilter:TermFilter) extends TermFilter(termFilter.name) {
      def apply(term:Term) = goids.contains(term.id) && termFilter(term)
    }
    def fgraph(termFilter:TermFilter) = {
      val graph = (new FGraphReaderGO(new TermFilterUsed(termFilter))).read(ontologyFile)
      graph.foreach{node => node.index = idx; idx += 1}
      graph
    }
    output("  extracting sub-ontologies: "+cfg("sub-ontology"))
    cfg("sub-ontology").split(',').map(s => fgraph(Ontology(s.trim))).toList
  }

  def createSampleSet(network:Network, fgraphs:List[FGraph], enc:Encoding) = {
    def toSample(interaction:Interaction):ASample = {
      val classID = if(interaction.weight > 0.5) 1 else 0
      val (ids1,ids2) = (interaction.protein1.goids.toSet, interaction.protein2.goids.toSet)
      val nodes = fgraphs.map(fg => enc(fg,ids1,ids2)).reduceLeft(_|_)
      FGenerator.toSample(fgraphs, nodes, classID)
    }
    val fvalues = List("0", "1")
    val features = fgraphs.map(fg => fg.map(node => Nominal(node.label,fvalues))).flatten.toSeq
    val interactions = InteractionFactory(network)
    new SampleSet(features, interactions.map(toSample))
  }

  def createSample(goids1:Set[String], goids2:Set[String], fgraphs:List[FGraph], enc:Encoding) = {
    val nodes = fgraphs.map(fg => enc(fg,goids1,goids2)).reduceLeft(_|_)
    FGenerator.toSample(fgraphs, nodes, 0)
  }

  def loadInputs(filepath:String) = {
    var inputs = List[(String, Set[String])]()
    for(line <- Source.fromFile(new File(filepath)).getLines()) {
      line.split('\t') match {
        case Array(accnr, goids) => inputs ::= (accnr, goids.split(',').map(_.trim).toSet)
        case _ => /* no goids */
      }
    }
    inputs
  }

  def loadPairsFilter(filepath:String) = {
    val pairs = Source.fromFile(new File(filepath)).getLines().map(_.trim.split('\t').toSet).toSet
    (accnr1:String, accnr2:String) => pairs contains Set(accnr1,accnr2)
  }



  def train() {
    output("TRAINING")
    val labels = List("n","y")
    output("  predictor: "+cfg("predictor"))
    val predictor = cfg("predictor") match {
      case "NB" => Weka(NaiveBayes, "", labels)
      case "RF" => Weka(RandomForest, "-I 10", labels)
      case "PEG" => Weka(Pegasos, "-F 1 -L 1e-4", labels)
      case "KNN" => Weka(IBk, "-K 5", labels)
      case _ => sys.error("Invalid predictor name: "+cfg("predictor"))
    }
    output("  loading network: "+cfg("network"))
    val network = NetworkReaderEdgeList.read(cfg("network"))
    output("  annotating network: "+cfg("annotations"))
    val  annotator = new AnnotatorGOfromFile(cfg("annotations"))
    annotator.annotate(network)
    output("  loading ontology: "+cfg("ontology"))
    val fgraphs = createFGraphs(cfg("ontology"), annotator.goids)
    output("  creating training data")
    val sampleSet = createSampleSet(network, fgraphs, FGenerator.ulca)
    output("  training predictor")
    predictor.train(sampleSet)
    output("  saving predictor: "+cfg("model"))
    Inducer.save(predictor, cfg("model"))
    output("  performing self-test: "+cfg("self-test"))
    if(cfg("self-test") == "YES") {
      val result = predictor.test(sampleSet)
      output("  self-test AUC = %.3f" format result.auc(0))
    }
    output("  finished.")
  }

  def predict() {
    output("PREDICTING")
    val writer = new BufferedWriter(new FileWriter(cfg("predictions")))
    output("  loading predictor: "+cfg("model"))
    val predictor = Inducer.load(cfg("model")).asInstanceOf[Weka]
    output("  loading ontology: "+cfg("ontology"))
    val  annotator = new AnnotatorGOfromFile(cfg("annotations"))
    val fgraphs = createFGraphs(cfg("ontology"), annotator.goids)
    output("  loading inputs: "+cfg("inputs"))
    val inputs = loadInputs(cfg("inputs"))
    val pairsFilter = if(hasCfg("filter")) {
      output("  loading filter: "+cfg("filter"))
      loadPairsFilter(cfg("filter"))
    } else (accnr1:String, accnr2:String) => true
    output("  predicting...")
    val threshold = cfg("threshold").toFloat
    val n = inputs.size
    for( (in1,i) <- inputs.zipWithIndex; (in2,j) <- inputs.zipWithIndex if i<j && pairsFilter(in1._1,in2._1) ) {
      val sample = createSample(in1._2, in2._2, fgraphs, FGenerator.ulca)
      val cfs = predictor.cfs(sample)
      if(cfs(1) >= threshold)
        writer.write("%s\t%s\t%.3f\n" format (in1._1, in2._1, cfs(1)))
      showStatus(i*n+j, n*n/2)
    }
    writer.close()
    output("  100.0 %%\nfinished.")
  }

  def test() {
    output("TESTING")
    output("  loading predictor: "+cfg("model"))
    val predictor = Inducer.load(cfg("model")).asInstanceOf[Weka]
    output("  loading network: "+cfg("network"))
    val network = NetworkReaderEdgeList.read(cfg("network"))
    output("  annotating network: "+cfg("annotations"))
    val  annotator = new AnnotatorGOfromFile(cfg("annotations"))
    annotator.annotate(network)
    output("  loading ontology: "+cfg("ontology"))
    val fgraphs = createFGraphs(cfg("ontology"), annotator.goids)
    output("  creating test data")
    val sampleSet = createSampleSet(network, fgraphs, FGenerator.ulca)
    output("  testing predictor")
    val (folds,runs) = (cfg("folds").toInt,cfg("runs").toInt)
    val results = XValidation(sampleSet,folds,runs).map(predictor.evaluate)
    val auc = results.map(_.auc(1)).sum / results.size
    output("  "+cfg("folds")+" fold x-validation AUC = %.3f" format auc)
    output("  finished.")
  }

  def output(formatstr:String, args:Any*) {
    println(formatstr.format(args:_*))
  }

  def showStatus(i:Int, n:Int) {
    val time = System.currentTimeMillis()
    if((time-otime)/1000/60 >= 15) {    // every 15 mins
      otime = time
      output("  %.1f %%", 100.0*i/n)    // progress percentage
    }
  }

  def main(args:Array[String]) {
    output("go2ppi")
    output("Version "+version)

    evalCommandLine(args)

    for(mode <- cfg("mode").split(',').map(_.trim))
      mode match {
        case "TRAIN"   => train()
        case "TEST"    => test()
        case "PREDICT" => predict()
        case _ => sys.error("mode parameter is invalid: "+mode)
      }
  }
}