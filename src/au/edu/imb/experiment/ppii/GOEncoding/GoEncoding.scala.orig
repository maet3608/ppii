package au.edu.imb.experiment.ppii.GOEncoding

import scala.math.log
import au.edu.imb.ml.result.CResult
import au.edu.imb.ml.xperiment.{XVariable, Mean,Std, XValidation, XPeriment}
import au.edu.imb.ml.classifier.{Weka, Majority, Mean => CMean}
import au.edu.imb.ml.classifier.Weka._
import au.edu.imb.ml.{Classifier}
import au.edu.imb.ml.sampleset.{Nominal, SparseSample, SampleSet}
import au.edu.imb.ppii.go.{TermFilter, Term, OboParser, Ontology}
import au.edu.imb.ppii.predictor.{GoDAG, DAG, Node}
import au.edu.imb.ppii.network.{Network, Protein, Interaction, InteractionFactory}
import au.edu.imb.ppii.network.io.{NetworkReaderPSI25XMLBDB, NetworkReaderEdgeList, NetworkReaderMel}
import au.edu.imb.ppii.network.annotation.{EvidenceFilterAll, EvidenceFilter, EvidenceFilterNI, AnnotatorGO}

/**
 * Explores different methods to exploit the information within the Gene Ontology
 * to infer protein protein interaction. For instance, GO annotations can
 * be represented as indicator vectors, stating which GO terms are shared by
 * two proteins.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 16/07/2010
 */

object GoEncoding extends XPeriment[CResult] {
  val name = "GoEncoding"
  val labels = List("n","y")
  type Encoding = (Interaction)=>Iterable[Node]

  val folds = 4
  val runs  = 1
  val evidenceFilter = EvidenceFilterAll
  //val evidenceFilter = EvidenceFilterNI
  val (interactions, goids, network) = init(0,0, evidenceFilter)
  
  lazy val cc = createDag(Ontology.cellularComponent)
  lazy val mf = createDag(Ontology.molecularFunction)
  lazy val bp = createDag(Ontology.biologicalProcess)
  lazy val no = createDag(Ontology.notObsolete)

  var dag:GoDAG = null

  add(XVariable("binary",
    ("BIN", true)
    //("TRI", false)
  ))
  add(XVariable("encoding", Seq[(String,Encoding)](
  //  ("AL  ", AL),
  //  ("AC  ", AC),
    //("AA  ", AA),
    //("LCA ", LCA),
    //("ACA ", ACA)
    //("LLCA ", ACA),
    ("ULCA", ULCA)
    ):_*
  ))
  add(XVariable("ontology",
 //   ("CC     ", List(cc))
 //   ("MF     ", List(mf))
 //   ("BP     ", List(bp))
  //  ("NO     ", List(no))
//    ("CC|MF  ", List(cc,mf)),
//    ("BP|CC   ", List(cc,bp))
    ("BP|CC|MF", List(bp,cc,mf))
  ))
  add(XVariable("classifier",
    //("MJ   ", new Majority(labels)),
    //("MEAN ", new CMean(labels))
    //("NB   ", Weka(NaiveBayes, "", labels))
    //("LR   ", Weka(Logistic, "", labels)),
    //("PEG1 ", Weka(Pegasos, "-F 1", labels)),
   // ("PEG0 ", Weka(Pegasos, "-F 0", labels)),
   // ("PEG0N", Weka(Pegasos, "-F 0 -N", labels)),
    //("SMO  ", Weka(SMO, "", labels)),
   // ("RBF  ", Weka(Weka.SMO, Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.01"),labels)),
   // ("RFK2 ", Weka(RandomForest, "-K 2", labels)),
   // ("RFK  ", Weka(RandomForest, "-K "+3*(log(ni)+1).toInt, labels)),
    //("RF   ", Weka(RandomForest, "", labels))
    ("RF1  ", Weka(RandomForest, "-I 100", labels))
  //  ("RF2  ", Weka(RandomForest, "-I 200", labels)),
  //  ("RF3  ", Weka(RandomForest, "-I 200", labels))
  //  ("RF4  ", Weka(RandomForest, "-I 800", labels))
    //("RF5  ", Weka(RandomForest, "-I 1200", labels)),
    //("RF6  ", Weka(RandomForest, "-I 2400", labels))
   // ("FRF ", Weka(FastRandomForest, "-I 400", labels))
    //("LMT  ", Weka(LMT, "", labels)),
    //("J48  ", Weka(J48, "", labels))
    //  ("NN  ", Weka(IB1, "", labels))
    //("KNN  ", Weka(IBk, "-K 15 -X", labels))
  ))

  add("AUC", Mean,Std)
  //add("MCC", Mean)
  //add("ACC", Mean)


  def result(results:CResult, name:String) = name match {
      case "AUC" => results.auc(1)
      case "MCC" => results.mcc(1)
      case "ACC" => results.acc(1)
      case _ => sys.error("Unknown result requested: "+name)
    }

  def evaluate = {
    val classifier = value("classifier").asInstanceOf[Classifier]
    val bin = value("binary").asInstanceOf[Boolean]
    val enc = value("encoding").asInstanceOf[Encoding]
    val dags = value("ontology").asInstanceOf[List[GoDAG]]
    val sampleSet = createSampleSet(bin,enc,dags)
    XValidation(sampleSet,folds,runs).map(classifier.evaluate)
  }

  def createSampleSet(bin:Boolean,enc:Encoding,dags:List[GoDAG]) = {
    def toVector(interaction:Interaction) = {
      var (v,l) = (Seq[(Int,Double)](), 0)
      def weight(node:Node) = if(bin) 1.0 else
        if(interaction.protein1.goids.contains(node.id) && interaction.protein2.goids.contains(node.id)) 2.0 else 1.0
      def encode(interaction:Interaction) =
        enc(interaction).toSeq.sortBy(_.index).map(n => (l+n.index,weight(n)))
      for(d <- dags) {
        dag = d                    // global variable
        v ++= encode(interaction)  // encode(),enc() uses dag !
        l += dag.length
      }
      (v,l)
    }
    def toSample(interaction:Interaction):SparseSample = {
      val classID = if(interaction.weight > 0.5) 1 else 0
      var (v,l) = toVector(interaction)
      SparseSample(l,v,classID)
    }
    val values = if(bin) List("0", "1") else List("0", "1", "2")
    val l = dags.map(_.length).sum
    val features = (0 until l).map(i => Nominal("F"+i, values))
    new SampleSet(features, interactions.map(toSample))
  }

  def createDag(termFilter:TermFilter) = {
    /** Only GO terms that are used within the network and pass the given term filter */
    class TermFilterUsed extends TermFilter(termFilter.name) {
      def apply(term:Term) = goids.contains(term.id) && termFilter(term)
    }
    val ontology = new Ontology(OboParser("data/go.obo"), new TermFilterUsed)
    val dag  = new GoDAG(ontology)
    dag.zipWithIndex.foreach{ case(node,i) => node.index = i }
    println("Dag size : "+dag.size)
    dag
  }
  
  def init(nPos:Int, nNeg:Int, evidenceFilter:EvidenceFilter) = {
    println("Loading network...")
    val network = NetworkReaderEdgeList.read("data/SC_park.txt")
    //val network = NetworkReaderEdgeList.read("data/SC_string.txt")
    //val network = NetworkReaderEdgeList.read("data/EC_string.txt")
    //val network = NetworkReaderEdgeList.read("data/SP_string.txt")
    //val network = NetworkReaderEdgeList.read("data/HS_string.txt")
    //val network = NetworkReaderEdgeList.read("data/MM_string.txt")
    //val network = NetworkReaderEdgeList.read("data/AT_string.txt")
    //val network = NetworkReaderEdgeList.read("data/DM_string.txt")
    //val network = NetworkReaderEdgeList.read("data/CT_string.txt")
    printf("proteins: %d  interaction %d\n", network.size, network.interactions.size)
    println("Annotate GO terms ...")
    (new AnnotatorGO(evidenceFilter)).annotate(network)
    println("Create interaction set...")
    val interactions = InteractionFactory(network, nPos, nNeg)
    printf("interactions: %d\n", interactions.size)
    println("Loading ontology...")
    val goids = Set()++network.flatMap(_.goids)
    (interactions, goids, network)
  }


  /** All dag nodes for a protein and their go ids */
  def nodes(protein:Protein) = protein.goids.flatMap(dag.get(_)) match {
      case List() => dag.roots  // no go annotation => use roots
      case nodes  => nodes
    }

  /** All Labeled nodes for an interaction */
  def AL(interaction:Interaction) =
    Set()++(nodes(interaction.protein1)++nodes(interaction.protein2))

  /** All Common nodes for an interaction */
  def AC(interaction:Interaction) =
    (Set()++(nodes(interaction.protein1)) & (Set()++nodes(interaction.protein2)))

    /** All Ancestor nodes for an interaction */
  def AA(interaction:Interaction) =
    dag.ancestors(nodes(interaction.protein1)) | dag.ancestors(nodes(interaction.protein2))

  /** All Common Ancestor nodes for an interaction */
  def ACA(interaction:Interaction) =
    dag.ancestors(nodes(interaction.protein1)) & dag.ancestors(nodes(interaction.protein2))

  /** All Lowest Common ancestor nodes for an interaction */
  def LCA(interaction:Interaction) =
    dag.LCAs(nodes(interaction.protein1), nodes(interaction.protein2))

  /** Labeled and Lowest Common ancestor nodes for an interaction */
  def LLCA(interaction:Interaction) =
    LCA(interaction) | AL(interaction)

  /** Up to Lowest Common Ancestor */
  def ULCA(interaction:Interaction):Set[Node] = {
    val ancestors1 = dag.ancestors(nodes(interaction.protein1))
    val ancestors2 = dag.ancestors(nodes(interaction.protein2))
    val commonAncestors = ancestors1 & ancestors2
    if(commonAncestors.isEmpty) return AL(interaction)
    val maxLevel = commonAncestors.map(_.level).max
    (ancestors1 | ancestors2).filter(_.level >= maxLevel)
  }



  def main(args: Array[String]) {
    println("running...")
    run()
    printSummary("binary","encoding","ontology","classifier")
    printSummary("classifier")
    printSummary("encoding")
    printSummary("ontology")
    printSummary("binary")
    println("finished.")
  }
}
