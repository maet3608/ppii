package au.edu.imb.ppii.go

import au.edu.imb.ppii.go.Ontology._


/** Abstract base class to filter terms when loading an ontology */
abstract class TermFilter(val name:String) {

  def apply(term:Term):Boolean

  /** Logical OR of two term filters */
  def | (that:TermFilter) = {
    class TermFilterOr(f1:TermFilter, f2:TermFilter) extends TermFilter(f1.name+"|"+f2.name) {
      def apply(term:Term) = f1(term) || f2(term)
    }
    new TermFilterOr(this,that) 
  }

  override def toString = name
}


/** Acceptance filter for non-obsolete terms within the given namespace */
class TermFilterNamespace(namespace:String) extends TermFilter(namespace) {
  def apply(term:Term) = term(GO_NAMESPACE)==namespace && !term.contains(GO_IS_OBSOLETE)
}

/** Acceptance filter for non-obsolete terms */
class TermFilterNonObsolete extends TermFilter("non_obsolete") {
  def apply(term:Term) = !term.contains(GO_IS_OBSOLETE)
}

/** Acceptance filter for all terms */
class TermFilterAll extends TermFilter("all") {
  def apply(term:Term) = true
}


/**
 * Describes a direct acyclic graph (DAG) of GO terms. It takes an iterable over
 * GO terms and creates the edges (relations) of the DAG from that.
 * The class also maintains a map that maps Go ids to the corresponding terms 
 * within the DAG.
 * The ontology is restricted to terms that pass the given term filter.
 * The companion object defines some acceptance filters, e.g. cellularComponent
 * http://www.geneontology.org/index.shtml
 * Regardless of the chosen term filter the constructor ensures that fully connected
 * DAGs are created (with root terms) and that the DAGs for different name spaces
 * are separate (e.g. links between cellular component and molecular function
 * are ignored). 
 * @param terms Iterable over GO terms the ontology is constructed from.
 * @param termFilter Acceptance filter for terms, eg. Ontology.cellularComponent
 */
class Ontology(private val terms:Iterable[Term], val termFilter:TermFilter)
                       extends Iterable[Term] {
  
  private var _termMap = Map[String,Term]()
  private var _relations = List[Relation]()
   
  createDAG(terms)  // Derives a DAG from GO terms and their relationships
  /** root terms of ontology, eg. cellular component and molecular function */
  val roots:List[Term] = this.filter(_.parents.length == 0).toList
  // Set term levels recursively, starting from roots
  roots.foreach(setLevel(_,0))

  
  /** Auxiliary constructor with predefined filter for cellular components */
  def this(terms:Iterable[Term]) = this(terms, Ontology.cellularComponent)


  /** Recursively determines and sets hierarchy levels of terms */
  private def setLevel(term:Term, level:Int):Unit = {
    term.level = term.level max level
    term.children.foreach(setLevel(_, level+1))
  }

  private def createDAG(terms:Iterable[Term]) {
    val allTerms = Map[String,Term]() ++ terms.map(term => (term.id,term))

    // Derives a DAG from GO terms and their relationships
    allTerms.valuesIterator.filter(termFilter(_)).foreach(addTerm)

    /** Adds a term and all its parents with links, which ensures a fully connected DAG */
    def addTerm(term:Term):Unit = {    
      if(!_termMap.contains(term.id)) {
        _termMap += ((term.id, term))
        getAllParents(term).foreach{parent => addTerm(parent); term.addIsA(parent)}
        _relations ++= term.parentRelations
      }
    }
    /** Extracts set of all parent terms for the given term */
    def getAllParents(term:Term):Set[Term] =
      (getParents(term,GO_IS_A,""):::getParents(term,GO_RELATIONSHIP,"part_of ")).toSet
    /** Extracts parent terms from part_of or is_a relations within same namespace */
    def getParents(term:Term, kind:String, prefix:String) =
        term.valuesOrElse(kind).flatMap(getTerm(prefix,_)).filter(sameNamespace(term,_))
    /** Tests whether two terms are in the same name space */
    def sameNamespace(term1:Term,term2:Term) =
      term1(GO_NAMESPACE) == term2(GO_NAMESPACE)
    /** Extracts GO ID from value string and returns the term if it exists, otherwise None */
    def getTerm(prefix:String, value:String):Option[Term] =
      (prefix+"""GO:\d{7}""").r.findFirstIn(value).flatMap(allTerms.get)
  }

  /** Returns the set of ancestors of the given term including the given term */  
  def ancestors(term:Term):Set[Term] =
    Set()+term++term.parents.flatMap(ancestors)

  /** Returns the set of ancestors of the given term id including the given term */
  def ancestors(id:String):Set[Term] =
    ancestors(_termMap(id))

  /** Returns the union of ancestors common to all given terms including the terms */
  def ancestors(terms:Iterable[Term]):Set[Term] = 
    terms.map(ancestors).reduceLeft(_ | _)

  /** Returns a set of slimmed GO terms using the given set of slim terms
   *  see http://search.cpan.org/~cmungall/go-perl/scripts/map2slim */
  def slim(terms:Set[Term], slimTerms:Set[Term]) = {
    def find(term:Term):Set[Term] =    // find slim terms for the given term
      if(slimTerms.contains(term)) Set(term) else Set()++term.parents.flatMap(find)
    val slims = terms.map(find).foldLeft(Set[Term]())(_ | _)
    val subsumers = slims.map(t => ancestors(t)-t).foldLeft(Set[Term]())(_ | _)
    slims -- subsumers // keep most specific slim terms = remove subsumers
  }

  /** Returns a sequence of all relations/edges of the DAG */
  def relations:Seq[Relation] = _relations
  
  /** Returns the term with the given GO id. Throws an exception if the id does not exist */
  def apply(id:String) = _termMap(id)

  /** Returns an option with the term for the given GO id. */
  def get(id:String) = _termMap.get(id)

  /** Returns the annotation for the given id and tag or "" if that annotation or id does not exist */
  def get(id:String,tag:String) = try{ _termMap(id)(tag) } catch { case _ => "" }
   
  /** Tests whether Ontology contains term with the given GO ID */
  def contains(id:String) = _termMap.contains(id)
    
  /** Returns the number of GO terms within the ontology */
  override def size = _termMap.size
   
  /** Returns an iterable over all terms of the ontology */
  def iterator = _termMap.valuesIterator

  /** Prints out some statistical information of the ontology */
  def printStatistics():Unit = {
    println("filter:     "+this.termFilter)
    println("roots:      "+this.roots.mkString(", "))
    println("terms:      "+this.size)
    println("edges:      "+this.relations.size)
    val levels = this.map(_.level)
    println("max depth:  "+levels.max)
    println("mean depth: "+levels.sum/levels.size)
    val kidLens = this.map(_.children.length)
    println("max kids:   "+kidLens.max)
    println("mean kids:  "+kidLens.sum/kidLens.size)
    val dadLens = this.map(_.parents.length)
    println("max dads:   "+dadLens.max)
    println("mean dads:  "+dadLens.sum/dadLens.size)
  }
}


/**
 * Defines term filters and constants
 */
object Ontology {
  val GO_ID            = "id"
  val GO_NAME          = "name"
  val GO_IS_A          = "is_a"
  val GO_RELATIONSHIP  = "relationship"
  val GO_NAMESPACE     = "namespace"
  val GO_SUBSET        = "subset"
  val GO_IS_OBSOLETE   = "is_obsolete"

  def apply(name:String) = name match {
    case "BP" => biologicalProcess
    case "CC" => cellularComponent
    case "MF" => molecularFunction
    case "NO" => notObsolete
    case _ => sys.error("Unknown ontology name: "+name)
  }

  /** Acceptance filter for non-obsolete, cellular components */
  def cellularComponent = new TermFilterNamespace("cellular_component")

  /** Acceptance filter for non-obsolete, biological process */
  def biologicalProcess = new TermFilterNamespace("biological_process")

  /** Acceptance filter for non-obsolete, molecular function */
  def molecularFunction = new TermFilterNamespace("molecular_function")

  /** Acceptance filter for non-obsolete terms */
  def notObsolete = new TermFilterNonObsolete

  /** Acceptance filter for all terms */
  def all = new TermFilterAll
}


/**
 * Usage example.
 */
object ExampleOntology {
  def main(args: Array[String]) {
    println("loading Ontologies...")

    //val ontology = new Ontology(OboParser("data/go.obo"), Ontology.cellularComponent)
    //ontology.foreach(println)

    // list of term filters
    val filters = List(
      Ontology.cellularComponent,
      Ontology.biologicalProcess,
      Ontology.molecularFunction,
      Ontology.notObsolete)

    filters.foreach(f => (new Ontology(OboParser("data/go.obo"), f)).printStatistics)

    println("finished.")
  }
}