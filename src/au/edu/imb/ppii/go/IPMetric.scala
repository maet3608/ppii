package au.edu.imb.ppii.go

/**
 * Calculation of the interaction potential of two proteins based on their
 * subcellular localization annotations (Go ID).
 */

import scala.collection.mutable.Map

/**
 * A map of unique go terms is compiled by traversing the GO DAG from the given
 * start term up to the root. A metric is applied to score the terms and the map
 * stores the term and the highest score.
 */
case class TermMap(private val start:Term, private val metric:IPMetric) 
     extends Iterable[Term] {
  /** Stores terms and their corresponding scores */
  private val map = Map[Term, Double]()
  
  traverse(start, metric.startValue(start))
  
  /** Traverses the go DAG up to the root and collect terms with their scores */
  private def traverse(term:Term, score:Double):Unit = {
    map(term) = score max map.getOrElse(term, score)
    term.parentRelations.foreach(r => traverse(r.parent, metric.calculate(r, score)))
  }
  
  /** Getter for the score of the given term */
  def apply(term:Term) = map(term)
  /** Tests if the term is within the map */
  def contains(term:Term) = map.contains(term)
  /** Iterable over the terms of the map*/
  def iterator = map.keysIterator
  
  /** Returns string with term and its score*/
  override def toString = map.formatted("%s:%.1f").mkString("\n")
}


/**
 * Abstract base class to measure interaction potentials.
 */     
abstract class IPMetric(private val ontology:Ontology) {
  
  /** abstract method that provides name of metric*/
  def name():String
  /** abstract method that provides description of metric*/
  def description():String  
  /** abstract method to calculate metric */
  def calculate(relation:Relation, score:Double):Double
  /** abstract method to provide start value for metric */
  def startValue(start:Term):Double 
  
  /** returns best common ancestor (BAC) and its score */
  protected def BCA(map1:TermMap, map2:TermMap) = {
    def best(a:(Term,Double), b:(Term,Double)) = (a._1, a._2 + b._2)
    map1.filter(map2 contains _).
      map(term => (term, map1(term)+map2(term))).
      reduceLeft(best(_,_))
   } 
    
  /** returns interaction potential (ip) between two proteins
   * based on their go term ids. */
  def ip(id1:String, id2:String):Double = 
    BCA(TermMap(ontology(id1), this),  TermMap(ontology(id2), this))._2
  
}

/*
class IPMetric1a(ontology:Ontology) extends IPMetric(ontology) {
  def name = "M1A"
  def description = "score - relation.weight, start.level"
  def calculate(relation:Relation, score:Double) =  score - relation.weight
  def startValue(start:Term) = start.level
}
class IPMetric1b(ontology:Ontology) extends IPMetric(ontology) {
  def name = "M1B"
  def description = "score - relation.weight, 0.0"
  def calculate(relation:Relation, score:Double) =  score - relation.weight
  def startValue(start:Term) = 0.0
}
class IPMetric2a(ontology:Ontology) extends IPMetric(ontology) {
  def name = "M2A"
  def description = "score - 1.0,start.level"
  def calculate(relation:Relation, score:Double) = score - 1.0
  def startValue(start:Term) = start.level
}
class IPMetric2b(ontology:Ontology) extends IPMetric(ontology) {
  def name = "M2B"
  def description = "score - 1.0, 0.0"
  def calculate(relation:Relation, score:Double) = score - 1.0
  def startValue(start:Term) = 0.0
}
class IPMetric3a(ontology:Ontology) extends IPMetric(ontology) {
  def name = "M3A"
  def description = "relation.child.level,start.level"
  def calculate(relation:Relation, score:Double) = relation.child.level
  def startValue(start:Term) = start.level
}
class IPMetric3b(ontology:Ontology) extends IPMetric(ontology) {
  def name = "M3B"
  def description = "relation.child.level,0.0"
  def calculate(relation:Relation, score:Double) = relation.child.level
  def startValue(start:Term) = 0.0
}
class IPMetric4a(ontology:Ontology) extends IPMetric(ontology) {
  def name = "M4A"
  def description = "0.0,start.level  "
  def calculate(relation:Relation, score:Double) = 0.0
  def startValue(start:Term) = start.level
}
class IPMetric4b(ontology:Ontology) extends IPMetric(ontology) {
  def name = "M4B"
  def description = "0.0,0.0 CONTROL"
  def calculate(relation:Relation, score:Double) =  0.0
  def startValue(start:Term) = 0.0
}
*/

/**
 * Null model where interaction potential is 1 if the two proteins
 * share the same go term annotation or 0 otherwise.
 */
class IPMetricNull(ontology:Ontology) extends IPMetric(ontology) {
  def name = "MNull"
  def description = "Null model, only exact matches score"
  def calculate(relation:Relation, score:Double) = 0.0; // not used
  def startValue(start:Term) = 0.0 
  override def ip(id1:String, id2:String) = if(id1==id2) 1.0 else 0.0
}


/**
 * Just a usage example
 */
object ExampleIPMetric {

  def main(args: Array[String]) {
  }

}