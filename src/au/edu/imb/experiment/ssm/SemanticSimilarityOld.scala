package au.edu.imb.experiment.ssm

import scala.math.log
import scala.collection.mutable.Map

import au.edu.imb.ppii.go.{TermFilter, Term, Ontology}
import au.edu.imb.ppii.fgraph.io.FGraphReaderGO
import au.edu.imb.ml.result.{COutput, CResult}
import au.edu.imb.ppii.network.{Protein, Interaction, InteractionFactory}
import au.edu.imb.ppii.fgraph.{Utils, FNode, FGenerator, FGraph}
import au.edu.imb.ml.xperiment.{XVal,Mean,Std}
import au.edu.imb.ppii.network.annotation.{EvidenceFilterNotIFI, AnnotatorGO}
import au.edu.imb.ppii.network.io.{NetworkReaderWeightedEdgeList, NetworkReaderEdgeList}

/**
 * Evaluation of the performance of semantic similarity methods on the Gene Ontology.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 25/10/2010
 */

object SemanticSimilarityOld  {
  type Measure = (Set[FNode], Set[FNode]) => Double
  type Interactions = Iterable[Interaction]

  val (interactions, goids) = init(0,0)

  //val graph = createFGraph(Ontology.biologicalProcess)
  val graph = createFGraph(Ontology.cellularComponent)
  //val graph = createFGraph(Ontology.molecularFunction)

  case class Odds(var p: Int, var n:Int) {
    var P = 0.0
    var N = 0.0
    def setPN(p:Int, n:Int) { P=p; N=n }
   // def odds = if(n>0) log(1.0*p/n) else 0.0
    //def odds = if(n>0) 1.0*p/n else 0.0
    //def odds = if(n*P>0) 1.0*p*N/(n*P) else 0.0
    def odds = {
      val (pp, pn) = ((1.0+p)/P,  (1.0+n)/N)
      -log((pp/(1.0-pp))/(pn/(1.0-pn)))
    }
  }
  case class PTable() {
    var N:Double = 0.0
    val p = Map[Set[String], Double]()
    def inc(goids:Set[String]) { if(p.contains(goids)) p(goids) += 1 else p(goids) = 1 }
    def setN(n:Double) {N = n}
    def apply(goids:Set[String]) = -log(p.getOrElse(goids, 0.0)/N)
  }

  var probs = Map[String,Double]()
  var probs2 = Map[String,Double]()
  var probs3 = Map[String,Odds]()
  var probs4 = Map[String,PTable]()



  def calcProbabilities(interactions:Interactions) = {
    var n = 0.0
    val probs = Map[String,Double]() ++= graph.map(node => (node.label, 0.0))
    val positives = interactions.filter(_.weight > 0.0).toList
    val proteins = (positives.map(_.protein1):::positives.map(_.protein2)).toSet.toList
    val goids = proteins.flatMap(_.goids.toList)
    def inc(label:String) { probs(label) += 1.0; n +=1 }
    goids.filter(graph.contains).foreach(goid => graph.ancestors(goid).foreach(n => inc(n.label)))
    probs.foreach{case (label,c) => probs(label) = c/n}
    probs
  }
  /** Resnik's similarity measure: sim(c1,c2) = max {c in subsumers(c1,c2) | -log(p(c))} */
  def ssmResnik(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def ICs(nodes:Set[FNode]) = nodes.map(n => -log(probs(n.label))).toSeq
    val commonAncestors = FGenerator.aca(graph, nodes1.map(_.label), nodes2.map(_.label))
    if(commonAncestors.isEmpty) 0.0 else ICs(commonAncestors).max
  }


  def calcProbabilities2(interactions:Interactions) = {
    var n = 0.0
    val probs2 = Map[String,Double]() ++= graph.map(node => (node.label, 0.0))
    def inc(label:String) { probs2(label) += 1.0; n +=1 }
    for(inter <- interactions.filter(_.weight > 0.0)) {
      val (ids1,ids2) = (inter.protein1.goids.toSet,inter.protein2.goids.toSet)
      val pairs = for(id1 <- ids1; id2 <- ids2) yield Set(id1,id2)
      for(pair <- pairs) {
        val nodes = FGenerator.aca(graph, Set(pair.head), Set(pair.last))
        nodes.foreach(n => inc(n.label))
      }
    }
    probs2.foreach{case (label,c) => probs(label) = c/n}
    probs2
  }
  def ssmResnik2(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def ICs(nodes:Set[FNode]) = nodes.map(n => -log(probs2(n.label))).toSeq
    val commonAncestors = FGenerator.aca(graph, nodes1.map(_.label), nodes2.map(_.label))
    if(commonAncestors.isEmpty) 0.0 else ICs(commonAncestors).max
  }


  def calcProbabilities3(interactions:Interactions) = {
    var P = 0
    var N = 0
    val probs3 = Map[String,Odds]() ++= graph.map(node => (node.label,Odds(0,0)))
    def inc(label:String, pos:Boolean) { if(pos) {probs3(label).p+=1; P+=1} else {probs3(label).n+=1; N+=1} }
    for(inter <- interactions) {
      val nodes = FGenerator.aca(graph, inter.protein1.goids.toSet, inter.protein2.goids.toSet)
      nodes.foreach(n => inc(n.label, inter.weight>0))
    }
    probs3.foreach{case (label,odds) => odds.setPN(P,N)}
    probs3
  }
  /** Resnik's similarity measure: sim(c1,c2) = max {c in subsumers(c1,c2) | -log(p(c))} */
  def ssmResnik3(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    def odds(nodes:Set[FNode]) = nodes.map(n => -log(probs3(n.label).odds)).toSeq
    val commonAncestors = FGenerator.aca(graph, nodes1.map(_.label), nodes2.map(_.label))
    if(commonAncestors.isEmpty) 0.0 else odds(commonAncestors).max
  }


  def calcProbabilities4(interactions:Interactions) = {
    var n = 0
    val probs4 = Map[String,PTable]() ++= graph.map(node => (node.label,PTable()))
    def inc(label:String, ids1:Set[String],ids2:Set[String], pos:Boolean) {
      val pairs = for(id1 <- ids1; id2 <- ids2) yield Set(id1,id2)
      if(pos) { pairs.foreach(probs4(label).inc); n += 1 }
    }
    for(inter <- interactions) {
      val (ids1,ids2) = (inter.protein1.goids.toSet,inter.protein2.goids.toSet)
      val nodes = FGenerator.aca(graph, ids1, ids2)
      nodes.foreach(node => inc(node.label, ids1, ids2, inter.weight>0))
    }
    probs4.foreach{case (label,ptable) => ptable.setN(n)}
    probs4
  }
  def ssmResnik4(nodes1:Set[FNode], nodes2:Set[FNode]):Double = {
    val (ids1,ids2) = (nodes1.map(_.label),nodes2.map(_.label))
    def ICmax(nodes:Set[FNode]) = {
      val pairs = for(id1 <- ids1; id2 <- ids2) yield Set(id1,id2)
      val ics = for(n <- nodes; pair <- pairs) yield probs4(n.label)(pair)
      ics.max
    }
    val commonAncestors = FGenerator.aca(graph, ids1,ids2)
    if(commonAncestors.isEmpty) 0.0 else ICmax(commonAncestors)
  }






  def nodes(protein:Protein) = protein.goids.toSet[String].flatMap(graph.get(_)) match {
      case nodes if nodes.isEmpty => graph.roots  // no labels => use roots
      case nodes => nodes
    }

  def ancestors(labels:Iterable[String]):Set[FNode] = {
    val goids = labels.filter(graph.contains)
    if(goids.isEmpty) Set[FNode]() else graph.ancestors(goids.map(graph(_)))
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



    /** evaluate with cross-validation */
  def evaluate(measure: Measure) = {
    def time = System.currentTimeMillis
    def target(interaction: Interaction) = if(interaction.weight > 0.0) 1 else 0
    def cfs(interaction: Interaction) = {
      val s = measure(nodes(interaction.protein1), nodes(interaction.protein2))
      Seq(1.0-s,s)
    }
    def train(interactions: Interactions) {
      probs = calcProbabilities(interactions)
      probs2 = calcProbabilities2(interactions)
      probs3 = calcProbabilities3(interactions)
      probs4 = calcProbabilities4(interactions)
    }
    def test(interactions:Interactions) =
      CResult(2, interactions.map(i => COutput(target(i),cfs(i)))).auc(1)
    def evaluate(interactions: (Interactions,Interactions)) = {
      val st = time
      train(interactions._1)
      val auc = test(interactions._2)
      (auc, (time-st)/1000.0)
    }
    val results = XVal(interactions, 3, 1).map(evaluate).toSeq
    val (aucs, durations) = (results.map(_._1),results.map(_._2))
    (Mean(aucs), Std(aucs), Mean(durations), Std(durations))
  }

  /** evaluate without cross-validation */
//  def evaluate(measure: Measure) = {
//    def time = System.currentTimeMillis
//    def target(interaction: Interaction) = if(interaction.weight > 0.0) 1 else 0
//    def cfs(interaction: Interaction) = {
//      val s = measure(nodes(interaction.protein1), nodes(interaction.protein2))
//      Seq(1.0-s,s)
//    }
//    val st = time
//    probs = calcProbabilities(interactions)
//    val dur = (time-st)/1000.0
//    val auc = CResult(2, interactions.map(i => COutput(target(i),cfs(i)))).auc(1)
//    (auc, 0.0, dur, 0.0)
//  }

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
    //val network = NetworkReaderWeightedEdgeList.read("data/SC_park.txt")
    val network = NetworkReaderWeightedEdgeList.read("data/SC_string.txt")
    //val network = NetworkReaderEdgeList.read("data/old/SC_string.txt")
    printf("  Network: %s\n", network.name)
    printf("  proteins: %d  interactions pos:%d\n", network.size, network.interactions.size)
    println("Annotate GO terms ...")
    (new AnnotatorGO(EvidenceFilterNotIFI)).annotate(network)
    //(new AnnotatorGO()).annotate(network)
    println("Create interaction set...")
    //val interactions = InteractionFactory(network, nPos, nNeg)
    val interactions = InteractionFactory(network, 0.0)
    printf("  interactions: pos:%d neg:%d\n", nPos,nNeg)
    val goids = Set()++network.flatMap(_.goids)
    (interactions, goids)
  }



  def main(args: Array[String]) {
    println("running...")
    val measures = List[(String,Measure)](
    //  ("SP",ssmShortestPath),
    //  ("Jiang",ssmJiang),
    //  ("Resnik",ssmResnik),
    //  ("Resnik2",ssmResnik2),
      ("Resnik3",ssmResnik3),
      ("Resnik4",ssmResnik4),
    //  ("ULCA",ssmULCA),
    //  ("OLCA",ssmOLCA),
    //  ("LCA",ssmLCA),
    //  ("Lin",ssmLin),
    //  ("Pesquita",ssmPesquita),
    //  ("Schlicker",ssmSchlicker),
      ("Gentleman",ssmGentleman))

    for((name,measure) <- measures) {
//      for(mode <- List("max","avg","bma")) {
//        val (meanAUC, stdAUC, meanDUR, stdDUR) = evaluate(ssmPairwise(measure,mode,_,_))
//        printf("%-15s %.2f  %.2f  \t  %.1f  %.2f\n", name+"-"+mode, meanAUC, stdAUC, meanDUR, stdDUR)
//        //printf("%-15s\t%.2f\n", name+"-"+mode, meanAUC)
//      }
      val (meanAUC, stdAUC, meanDUR, stdDUR) = evaluate(measure)
      printf("%-15s %.2f  %.2f \t  %.1f  %.2f\n", name+"-gen", meanAUC, stdAUC, meanDUR, stdDUR)
      //printf("%-15s\t%.2f\n", name+"-gen", meanAUC)
    }

    println("finished.")
  }
}