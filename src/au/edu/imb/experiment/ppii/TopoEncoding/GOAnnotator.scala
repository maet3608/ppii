package au.edu.imb.experiment.ppii.TopoEncoding

import au.edu.imb.ml.result.CResult
import au.edu.imb.ml.classifier.{Weka}
import au.edu.imb.ml.classifier.Weka._
import au.edu.imb.ml.Classifier
import au.edu.imb.ppii.go.{TermFilter, Term, Ontology}
import au.edu.imb.ppii.network.Protein._
import au.edu.imb.ppii.network.{Interaction, InteractionFactory}
import au.edu.imb.ppii.fgraph.io.FGraphReaderGO
import au.edu.imb.ml.sampleset.{ASample, Nominal, SampleSet}
import au.edu.imb.ppii.fgraph.{FNode, FGenerator, FGraph}
import au.edu.imb.ppii.network.io.{NetworkReaderWeightedEdgeList, NetworkReaderEdgeList}
import au.edu.imb.ppii.network.annotation._
import au.edu.imb.ml.xperiment._


/**
 * Explores different version of GO annotation when predicting protein-protein interactions.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 02/12/2010
 */

object GOAnnotator extends XPerimentBinaryClassifier {
  val name = "InducerComp_10x_RF_NoK_HS_-IFI"
  type Inducer = (FGraph, Set[String], Set[String]) => Set[FNode]
                                              
  val folds = 10
  val runs  = 1
  val (interactions, network) = init(0,0)

  lazy val slim_gen   = new AnnotatorGOSlim("data/goslim_generic.obo","data/go.obo")
  lazy val slim_pir   = new AnnotatorGOSlim("data/goslim_pir.obo","data/go.obo")
  lazy val slim_yeast = new AnnotatorGOSlim("data/goslim_yeast.obo","data/go.obo")
  lazy val slim_goa   = new AnnotatorGOSlim("data/goslim_goa.obo","data/go.obo")
  lazy val slim_PPI   = new AnnotatorGOSlim("data/goslim_ppi_yeast.obo","data/go.obo")


  add(XVariable("annotator",
//    ("SLIM_GEN", slim_gen),
//    ("SLIM_GOA", slim_goa),
//    ("SLIM_PIR", slim_pir),
//    ("SLIM_YEA", slim_yeast),
//    ("SLIM_PPI ", slim_PPI),

//    ("-EXP", new AnnotatorGO(new EvidenceFilterExclude("EXP"))),
//    ("-IDA", new AnnotatorGO(new EvidenceFilterExclude("IDA"))),
//    ("-IPI", new AnnotatorGO(new EvidenceFilterExclude("IPI"))),
//    ("-IMP", new AnnotatorGO(new EvidenceFilterExclude("IMP"))),
//    ("-IGI", new AnnotatorGO(new EvidenceFilterExclude("IGI"))),
//    ("-IEP", new AnnotatorGO(new EvidenceFilterExclude("IEP"))),
//    ("-ISS", new AnnotatorGO(new EvidenceFilterExclude("ISS"))),
//    ("-ISO", new AnnotatorGO(new EvidenceFilterExclude("ISO"))),
//    ("-ISA", new AnnotatorGO(new EvidenceFilterExclude("ISA"))),
//    ("-ISM", new AnnotatorGO(new EvidenceFilterExclude("ISM"))),
//    ("-IGC", new AnnotatorGO(new EvidenceFilterExclude("IGC"))),
//    ("-RCA", new AnnotatorGO(new EvidenceFilterExclude("RCA"))),
//    ("-TAS", new AnnotatorGO(new EvidenceFilterExclude("TAS"))),
//    ("-NAS", new AnnotatorGO(new EvidenceFilterExclude("NAS"))),
//    ("-IC ", new AnnotatorGO(new EvidenceFilterExclude("IC"))),
//    ("-ND ", new AnnotatorGO(new EvidenceFilterExclude("ND"))),
//    ("-IEA", new AnnotatorGO(new EvidenceFilterExclude("IEA")))

//    ("+EXP  ", new AnnotatorGO(EvidenceFilterExperimental)),
//    ("+COM  ", new AnnotatorGO(EvidenceFilterComputational)),
//    ("+CUR  ", new AnnotatorGO(EvidenceFilterCurated)),
//    ("+IEA  ", new AnnotatorGO(EvidenceFilterIEA)),
      ("-IFI  ", new AnnotatorGO(EvidenceFilterNotIFI))
//    ("+IFI  ", new AnnotatorGO(EvidenceFilterAll))
//    ("-IF   ", new AnnotatorGO(EvidenceFilterNotInferred)),

    //("+ALL  ", new AnnotatorGO(EvidenceFilterAll))
  ))
  add(XVariable("inducer", Seq[(String,Inducer)](
      ("AC  ", FGenerator.ac),
      ("AL  ", FGenerator.al),
      ("AA  ", FGenerator.aa),
      ("ACA ", FGenerator.aca),
      ("LCA ", FGenerator.lca),
      ("OLCA", FGenerator.olca),
      ("WLCA", FGenerator.wlca),
      ("ULCA", FGenerator.ulca),
      ("SPS ", FGenerator.sps),
      ("SPA ", FGenerator.spa)
    ):_*
  ))
  add(XVariable("ontology",
    ("CC   ", Ontology.cellularComponent),
    ("MF   ", Ontology.molecularFunction),
    ("BP   ", Ontology.biologicalProcess)
  ))
  add(XVariable("classifier",
  //  ("NB   ", Weka(NaiveBayes, "", labels))
  //  ("RF   ", Weka(RandomForest, "-I 200 -K 200", labels))
      ("RF   ", Weka(RandomForest, "-I 200", labels))
  ))


  def evaluate = {
    val classifier = value[Classifier]("classifier")
    val inducer = value[Inducer]("inducer")
    val termFilter = value[TermFilter]("ontology")
    val annotator = value[Annotator]("annotator")
    val sampleSet = createSampleSet(inducer, termFilter, annotator)
    XValidation(sampleSet,folds,runs).map(classifier.evaluate)
  }                                  

  def createSampleSet(inducer:Inducer,termFilter:TermFilter,annotator:Annotator) = {
    (new AnnotatorRemove(P_GOIDS)).annotate(network)
    annotator.annotate(network)
    val goids = Set()++network.flatMap(_.goids)
    val fgraph = createFGraph(termFilter, goids)
    def toSample(interaction:Interaction):ASample = {
      val classID = if(interaction.weight > 0.0) 1 else 0
      val nodes = inducer(fgraph, interaction.protein1.goids.toSet, interaction.protein2.goids.toSet)
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
    display("Ontology %s,  size: %s\n",termFilter.name, graph.size)
    graph
  }

  def init(nPos:Int, nNeg:Int) = {
    display("Loading network...")
    //val network = NetworkReaderWeightedEdgeList.read("data/SC_park.txt")
    //val network = NetworkReaderWeightedEdgeList.read("data/SC_string.txt")
    val network = NetworkReaderWeightedEdgeList.read("data/HS_string.txt")
    //val network = NetworkReaderWeightedEdgeList.read("data/MM_string.txt")
    //val network = NetworkReaderWeightedEdgeList.read("data/EC_string.txt")
    //val network = NetworkReaderWeightedEdgeList.read("data/SP_string.txt")
    //val network = NetworkReaderWeightedEdgeList.read("data/AT_string.txt")
    //val network = NetworkReaderWeightedEdgeList.read("data/DM_string.txt")
    //val network = NetworkReaderPSI25XMLBDB.read("f:/PPII/Data/PPI/BioGrid/xml/Saccharomyces_cerevisiae.dbxml", "net:sf:psidev:mi")
    display("  Network: %s\n", network.name)
    display("  proteins: %d  interactions: %d\n", network.size, network.interactions.size)
    display("Create interaction set...")
    //val interactions = InteractionFactory(network, nPos, nNeg)
    val interactions = InteractionFactory(network, 0.0)
    display("  interactions: pos:%d neg:%d\n", interactions.filter(_.weight>0).size, interactions.filter(_.weight<0).size)
    (interactions, network)
  }



  def main(args: Array[String]) {
    println("running...")
    run()
    //summary("annotator","inducer","ontology","classifier")
    //summary("annotator","inducer","ontology")
    //summary("annotator","inducer")
    summary("inducer","ontology")
    //summary("inducer","classifier")
    //summary("classifier")
    summary("ontology")
    summary("inducer")
    summary("annotator")
    println("finished.")
  }
}