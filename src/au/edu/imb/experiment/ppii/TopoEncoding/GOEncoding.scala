package au.edu.imb.experiment.ppii.TopoEncoding

import au.edu.imb.ml.result.CResult
import au.edu.imb.ml.classifier.{Weka, Majority, Mean => CMean}
import au.edu.imb.ml.classifier.Weka._
import au.edu.imb.ml.{Classifier}
import au.edu.imb.ppii.network.annotation.AnnotatorGO
import au.edu.imb.ppii.go.{TermFilter, Term, OboParser, Ontology}
import au.edu.imb.ppii.network.{Network, Protein, Interaction, InteractionFactory}
import au.edu.imb.ppii.network.io.{NetworkReaderPSI25XMLBDB, NetworkReaderEdgeList, NetworkReaderMel}
import au.edu.imb.ppii.fgraph.io.FGraphReaderGO
import au.edu.imb.ml.sampleset.{ASample, Nominal, SparseSample, SampleSet}
import au.edu.imb.ppii.fgraph.{FNode, FGenerator, FGraph}
import au.edu.imb.ml.xperiment._


/**
 * Explores different methods to encode the information within the gene ontology as a feature vector
 * to infer protein protein interaction. For instance, GO annotations of two proteins are mapped
 * to an indicator vector, stating which GO terms are shared.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 16/07/2010
 */

object GOEncoding extends XPerimentBinaryClassifier {
  val name = "Inducers_NB_MM_biogrid"
  type Encoding = (FGraph, Set[String], Set[String]) => Set[FNode]
                                              
  val folds = 5
  val runs  = 1
  val (interactions, goids, network) = init(0,0)

  lazy val cc = createFGraph(Ontology.cellularComponent)
  lazy val mf = createFGraph(Ontology.molecularFunction)
  lazy val bp = createFGraph(Ontology.biologicalProcess)
  //lazy val no = createFGraph(Ontology.notObsolete)


  add(XVariable("indcucer", Seq[(String,Encoding)](
//      ("AC  ", FGenerator.ac),
//      ("AL  ", FGenerator.al),
//      ("AA  ", FGenerator.aa),
//      ("ACA  ", FGenerator.aca),
//      ("OLCA", FGenerator.olca),
//      ("LCA ", FGenerator.lca),
//      ("WLCA", FGenerator.wlca),
      ("ULCA", FGenerator.ulca)
//      ("SPS ", FGenerator.sps),
//      ("SPA ", FGenerator.spa)
    ):_*
  ))
  add(XVariable("ontology",
    ("BP   ", bp),
    ("CC   ", cc),
    ("MF   ", mf)
//    ("NO   ", no)
  ))
  add(XVariable("classifier",
    //("MJ   ", new Majority(labels)),
    //("MEAN ", new CMean(labels))
    ("NB   ", Weka(NaiveBayes, "", labels)),
    ("LR   ", Weka(Logistic, "", labels)),
    //("PEGF1L-5 ", Weka(Pegasos, "-F 1 -L 0.00001", labels)),
    //("SMO  ", Weka(SMO, "", labels)),
   // ("RBF  ", Weka(Weka.SMO, Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.01"),labels)),
//    ("LR-9   ", Weka(Logistic, "-R 1e-9", labels)),
//    ("LR-8   ", Weka(Logistic, "-R 1e-8", labels)),
    ("LR-7   ", Weka(Logistic, "-R 1e-7", labels)),
//    ("LR-6   ", Weka(Logistic, "-R 1e-6", labels))
    //("PEG1 ", Weka(Pegasos, "-F 1", labels)),
    ("PEGF1L-4", Weka(Pegasos, "-F 1 -L 1e-4", labels)),
   // ("PEG0N", Weka(Pegasos, "-F 0 -N", labels)),
//    ("LinSVMZ   ", Weka(LibSVM, "-K 0 -C 1.0 -Z", labels)),
//    ("LinSVMC-2 ", Weka(LibSVM, "-K 0 -C 0.01", labels)),
//    ("LinSVMC-1 ", Weka(LibSVM, "-K 0 -C 0.1", labels)),
//    ("LinSVMC+1 ", Weka(LibSVM, "-K 0 -C 1.0", labels)),
    ("LinSVMC+2 ", Weka(LibSVM, "-K 0 -C 10.0", labels)),
//    ("LinSVMC+3 ", Weka(LibSVM, "-K 0 -C 100.0", labels))
    //("SMO  ", Weka(SMO, "", labels)),
//    ("RBFC-3  ", Weka(Weka.SMO, Array("-C","0.001","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.01"),labels)),
//    ("RBFC-2  ", Weka(Weka.SMO, Array("-C","0.01","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.01"),labels)),
    ("RBFC-1  ", Weka(Weka.SMO, Array("-C","0.1","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.01"),labels)),
//    ("RBFC+1  ", Weka(Weka.SMO, Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.01"),labels)),
//    ("RBFC+2  ", Weka(Weka.SMO, Array("-C","10.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.1"),labels))
//    ("RBFG002  ", Weka(Weka.SMO, Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.02"),labels)),
//    ("RBFG005  ", Weka(Weka.SMO, Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.05"),labels)),
//    ("RBFG010  ", Weka(Weka.SMO, Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.10"),labels)),
//    ("RBFG012  ", Weka(Weka.SMO, Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.12"),labels)),
//    ("RBFG015  ", Weka(Weka.SMO, Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.15"),labels))
//    ("RBFC-1G005  ", Weka(Weka.SMO, Array("-C","0.1","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.05"),labels)),
//    ("RBFC-1G007  ", Weka(Weka.SMO, Array("-C","0.1","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.07"),labels)),
//    ("RBFC-1G010  ", Weka(Weka.SMO, Array("-C","0.1","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.10"),labels)),
//    ("RBFC-1G013  ", Weka(Weka.SMO, Array("-C","0.1","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.13"),labels)),
//    ("RBFC-1G015  ", Weka(Weka.SMO, Array("-C","0.1","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.15"),labels))
//    ("RF    ", Weka(RandomForest, "", labels)),
//    ("RFK1  ", Weka(RandomForest, "-K 1", labels)),
//    ("RFK2  ", Weka(RandomForest, "-K 2", labels)),
//    ("RFK5  ", Weka(RandomForest, "-K 5", labels)),
//    ("RFK10 ", Weka(RandomForest, "-K 10", labels)),
//    ("RFK20 ", Weka(RandomForest, "-K 20", labels)),
//    ("RFK50 ", Weka(RandomForest, "-K 50", labels)),
//    ("RFK100", Weka(RandomForest, "-K 100", labels))
//    ("RFI5   ", Weka(RandomForest, "-I 5", labels)),
//    ("RFI10  ", Weka(RandomForest, "-I 10", labels)),
    ("RFI100 ", Weka(RandomForest, "-I 100", labels)),
//    ("RFI200 ", Weka(RandomForest, "-I 200", labels)),
//    ("RFI400 ", Weka(RandomForest, "-I 400", labels)),
//    ("RFI800 ", Weka(RandomForest, "-I 800", labels))
//    ("J48  ", Weka(J48, "", labels)),
//    ("J48U  ", Weka(J48, "-U", labels)),
//    ("J48B  ", Weka(J48, "-B", labels)),
//    ("J48R  ", Weka(J48, "-R", labels)),
//    ("J48L  ", Weka(J48, "-L", labels)),
//    ("J48C0.1  ", Weka(J48, "-C 0.1", labels)),
//    ("J48C0.2  ", Weka(J48, "-C 0.2", labels)),
    ("J48C0.3  ", Weka(J48, "-C 0.3", labels)),
//    ("J48C0.4  ", Weka(J48, "-C 0.4", labels)),
//    ("J48C0.5  ", Weka(J48, "-C 0.5", labels)),
//    ("J48C0.6  ", Weka(J48, "-C 0.6", labels)),
//    ("J48C0.7  ", Weka(J48, "-C 0.7", labels)),
//    ("J48C0.8  ", Weka(J48, "-C 0.8", labels)),
//    ("J48C0.9  ", Weka(J48, "-C 0.9", labels))
    //  ("NBT  ", Weka(NBTree, "", labels))
    ("KNN5  ", Weka(IBk, "-K 5", labels))
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
      val classID = if(interaction.weight > 0.5) 1 else 0
      val nodes = enc(fgraph, interaction.protein1.goids.toSet, interaction.protein2.goids.toSet)
      FGenerator.toSample(fgraph, nodes, classID)
    }
    val fvalues = List("0", "1")
    val features = fgraph.map(node => Nominal(node.label,fvalues)).toSeq
    new SampleSet(features, interactions.map(toSample))
  }

  def createFGraph(termFilter:TermFilter) = {
    class TermFilterUsed extends TermFilter(termFilter.name) {
      def apply(term:Term) = goids.contains(term.id) && termFilter(term)
    }
    val graph = (new FGraphReaderGO(new TermFilterUsed)).read("data/go.obo")
    display("Ontology %s,  size: %s\n",termFilter.name, graph.size)
    graph
  }

  def init(nPos:Int, nNeg:Int) = {
    //val filepath = "data/SC_park.txt"
    val filepath = "data/SC_string.txt"
    display("Loading network: "+filepath+" ...")
    val network = NetworkReaderEdgeList.read(filepath)
    display("  Network: %s\n", network.name)
    display("  proteins: %d  interactions pos:%d\n", network.size, network.interactions.size)
    display("Annotate GO terms ...")
    (new AnnotatorGO()).annotate(network)
    display("Create interaction set...")
    val interactions = InteractionFactory(network, nPos, nNeg)
    display("  interactions: pos:%d neg:%d\n", nPos,nNeg)
    val goids = Set()++network.flatMap(_.goids)
    (interactions, goids, network)
  }



  def main(args: Array[String]) {
    println("running...")
    run()
    summary("inducer","ontology","classifier")
    summary("classifier")
    summary("inducer")
    summary("ontology")
    println("finished.")
  }
}
