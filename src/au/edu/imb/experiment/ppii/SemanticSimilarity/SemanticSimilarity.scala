package au.edu.imb.experiment.ppii.SemanticSimilarity

import scala.math.log
import scala.collection.mutable.Map

import au.edu.imb.ppii.network.annotation.AnnotatorGO
import au.edu.imb.ppii.go.{TermFilter, Term, Ontology}
import au.edu.imb.ppii.network.io.NetworkReaderEdgeList
import au.edu.imb.ppii.fgraph.io.FGraphReaderGO
import au.edu.imb.ml.result.{COutput, CResult}
import au.edu.imb.ppii.network.{Protein, Interaction, InteractionFactory}
import au.edu.imb.ppii.fgraph.{Utils, FNode, FGenerator, FGraph}




/**
 * Measures the similarity between GO terms with different semantic similarity methods
 * to infer protein-protein interactions.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 25/10/2010
 */

object SemanticSimilarity  {
  type Measure = (Set[FNode], Set[FNode]) => Double

  val (interactions, goids) = init(0,0)

  //val graph = createFGraph(Ontology.cellularComponent)
  //val graph = createFGraph(Ontology.biologicalProcess)
  val graph = createFGraph(Ontology.molecularFunction)

  val probs = calcProbabilities(interactions, goids)


  /**Prob of a GO term within the data set (set of interactions) taking the ancestors of a
   * term into account. See Resnik's paper. Root term has prob of 1.0.
   * Returns map of GO terms to their probabilities. */
  def calcProbabilities(interactions:Iterable[Interaction], goids:Set[String]) = {
    var probs = Map[String,Double]() ++= graph.map(node => (node.label, 0.0))
    def inc(label:String) { probs(label) += 1.0 }
    goids.filter(graph.contains).foreach(goid => graph.ancestors(goid).foreach(n => inc(n.label)))
    var n = probs(graph.roots.head.label)
    probs.foreach{case (label,c) => probs(label) = c/n}
    probs
  }

  def nodes(protein:Protein) = protein.goids.toSet[String].flatMap(graph.get(_)) match {
      case nodes if nodes.isEmpty => graph.roots  // no labels => use roots
      case nodes => nodes
    }

  /** Resnik's similarity measure: sim(c1,c2) = max {c in subsumers(c1,c2) | -log(p(c))} */
  def ssmResnik(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    val ancestors1 = graph.ancestors(nodes1)
    val ancestors2 = graph.ancestors(nodes2)
    val commonAncestors = ancestors1 & ancestors2
    if(commonAncestors.isEmpty) return 0.0
    commonAncestors.map(n => -log(probs(n.label))).toSeq.max
  }


  def ssmPairwise(measure:Measure, mode:String, nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    if(mode == "bma") return ssmPairwiseBMA(measure, nodes1, nodes2)
    val ms = for((n1,i) <- nodes1.toSeq.zipWithIndex; (n2,j) <- nodes2.toSeq.zipWithIndex if j < i ) yield measure(Set(n1),Set(n2))
    if(ms.isEmpty) return 0.0
    mode match {
      case "avg" => ms.sum/ms.size
      case "max" => ms.max
      case _ => sys.error("Unknown mode: "+mode)
    }
  }

  def ssmPairwiseBMA(measure:Measure, nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def mean(x:Seq[Double]) = x.sum/x.length
    def avg(nodes1:Set[FNode], nodes2:Set[FNode]) =
      mean(for(n1 <- nodes1.toSeq) yield (for(n2 <- nodes2.toSeq) yield measure(Set(n1),Set(n2))).max)
    (avg(nodes1,nodes2)+avg(nodes2,nodes1))/2.0
  }



  /** Lin's similarity measure */
  def ssmLin(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def ICs(nodes:Set[FNode]) = nodes.map(n => -log(probs(n.label))).toSeq
    val ancestors1 = graph.ancestors(nodes1)
    val ancestors2 = graph.ancestors(nodes2)
    val commonAncestors = ancestors1 & ancestors2
    if(commonAncestors.isEmpty) return 0.0
    val num = ICs(commonAncestors).max *(ancestors1.size+ancestors2.size)
    val den = ICs(ancestors1).sum + ICs(ancestors2).sum
    if(den > 0) num/den else 0.0
  }

  /** Jiangs's similarity measure */
  def ssmJiang(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def ICs(nodes:Set[FNode]) = nodes.map(n => -log(probs(n.label))).toSeq
    val ancestors1 = graph.ancestors(nodes1)
    val ancestors2 = graph.ancestors(nodes2)
    val commonAncestors = ancestors1 & ancestors2
    if(commonAncestors.isEmpty) return 0.0
    val num = ICs(commonAncestors).max *(ancestors1.size+ancestors2.size)
    val den = ICs(ancestors1).sum + ICs(ancestors2).sum
    1.0-den-num
  }

  /** Schlicker's similarity measure */
  def ssmSchlicker(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    val ancestors1 = graph.ancestors(nodes1)
    val ancestors2 = graph.ancestors(nodes2)
    val commonAncestors = ancestors1 & ancestors2
    if(commonAncestors.isEmpty) return 0.0
    val weight = commonAncestors.map(n => 1-probs(n.label)).toSeq.max
    ssmLin(nodes1,nodes2) * weight
  }

  /** simUI: Ratio between common ancestor graph and union of ancestors graphs. See Gentleman's paper */
  def ssmGentleman(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    val ancestors1 = graph.ancestors(nodes1)
    val ancestors2 = graph.ancestors(nodes2)
    val nIntersection = (ancestors1 & ancestors2).size.toDouble
    val nUnion = (ancestors1 | ancestors2).size
    if(nUnion > 0) nIntersection/nUnion else 0.0
  }

  /** simGIC See Pesquita's paper */
  def ssmPesquita(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def ICsum(nodes:Set[FNode]) = nodes.map(n => -log(probs(n.label))).toSeq.sum
    val ancestors1 = graph.ancestors(nodes1)
    val ancestors2 = graph.ancestors(nodes2)
    val icIntersection = ICsum(ancestors1 & ancestors2)
    val icUnion = ICsum(ancestors1 | ancestors2)
    if(icUnion != 0.0) icIntersection/icUnion else 0.0
  }

  /** shortest path between nodes */
  def ssmShortestPath(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    val all = nodes1 ++ nodes2
    val nMin = all.size.toDouble
    var path = all
    for(node <- all)
      path = path ++ Utils.shortestPathSingle(graph, node, path - node)
    if(path.size > 0) nMin/path.size else 0.0
  }

  /** average information content over ULCA */
  def ssmULCA(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def ICs(nodes:Set[FNode]) = nodes.map(n => -log(probs(n.label))).toSeq
    val nodes = FGenerator.ulca(graph, nodes1.map(_.label), nodes2.map(_.label))
    if(nodes.size == 0) 0.0 else ICs(nodes).sum/nodes.size
  }

  /** max information content of LCAs */
  def ssmOLCA(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def ICs(nodes:Set[FNode]) = nodes.map(n => -log(probs(n.label))).toSeq
    val nodes = FGenerator.olca(graph, nodes1.map(_.label), nodes2.map(_.label))
    if(nodes.size == 0) 0.0 else ICs(nodes).max
  }

  /** avg information content of LCAs */
  def ssmLCA(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def ICs(nodes:Set[FNode]) = nodes.map(n => -log(probs(n.label))).toSeq
    val nodes = FGenerator.lca(graph, nodes1.map(_.label), nodes2.map(_.label))
    if(nodes.size == 0) 0.0 else ICs(nodes).sum/nodes.size
  }


  def evaluate(measure: Measure) = {
    def target(interaction:Interaction) = if(interaction.weight > 0.5) 1 else 0
    def cfs(interaction:Interaction) = {
      val s = measure(nodes(interaction.protein1), nodes(interaction.protein2)); Seq(1.0-s,s)
    }
    CResult(2, interactions.map(i => COutput(target(i),cfs(i))))
  }

  def createFGraph(termFilter:TermFilter) = {
    class TermFilterUsed extends TermFilter(termFilter.name) {
      def apply(term:Term) = goids.contains(term.id) && termFilter(term)
    }
    val graph = (new FGraphReaderGO(new TermFilterUsed)).read("data/go.obo")
    printf("Ontology %s,  size: %s\n",termFilter.name, graph.size)
    graph
  }

  def init(nPos:Int, nNeg:Int) = {
    println("Loading network...")
    //val network = NetworkReaderEdgeList.read("data/SC_string.txt")
    //val network = NetworkReaderEdgeList.read("data/HS_string.txt")
    val network = NetworkReaderEdgeList.read("data/SC_park.txt")
    printf("  Network: %s\n", network.name)
    printf("  proteins: %d  interactions pos:%d\n", network.size, network.interactions.size)
    println("Annotate GO terms ...")
    (new AnnotatorGO()).annotate(network)
    println("Create interaction set...")
    val interactions = InteractionFactory(network, nPos, nNeg)
    printf("  interactions: pos:%d neg:%d\n", nPos,nNeg)
    val goids = Set()++network.flatMap(_.goids)
    (interactions, goids)
  }



  def main(args: Array[String]) {
    println("running...")
    val measures = List[(String,Measure)](
     // ("SP",ssmShortestPath),
     // ("Jiang",ssmJiang),
      ("Resnik",ssmResnik),
      //("ULCA",ssmULCA),
      //("OLCA",ssmOLCA),
      //("LCA",ssmLCA),
   //   ("Lin",ssmLin),
      ("Pesquita",ssmPesquita),
   //   ("Gentleman",ssmGentleman),
      ("Schlicker",ssmSchlicker))

    for((name,measure) <- measures) {
      for(mode <- List("max","avg","bma"))
        printf("%-15s %.3f\n",name+"-"+mode, evaluate(ssmPairwise(measure,mode,_,_)).auc(1))
      printf("%-15s %.3f\n",name+"-gen",  evaluate(measure).auc(1))
    }

    println("finished.")
  }
}