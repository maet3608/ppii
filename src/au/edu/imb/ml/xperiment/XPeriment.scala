package au.edu.imb.ml.xperiment

import java.io.{File, FileWriter, BufferedWriter}
import scala.collection.immutable.ListMap
import au.edu.imb.ml.sampleset.{SampleSet, DenseSample}
import au.edu.imb.ml.classifier.{Majority, SimpleKNN}
import collection.mutable.ArrayBuffer
import au.edu.imb.ml.result.{Result, CResult}
import au.edu.imb.ml.Classifier



/**
 * Describes some kind of experiment, which has some variables (e.g. different
 * classifiers), computes some outputs (e.g. AUC, accuracy) and calculates some
 * statistics (e.g. mean, std) over those outputs.
 * Author : Stefan Maetschke
 * Version: 1.00
 */

abstract class XPeriment[R <: Result] {
  /** Number of variable iterations the xperiment has performed so far */
  private var step = 0
  /** List of variables the experiment runs over */
  private val variables = new ArrayBuffer[XVariable]()
  /** Outputs and the statistics calculated over */
  private var statistics = new ListMap[String, Seq[XStatistic]]()
  /** Log file writer */
  private var writer:FileWriter = _

  /** if true prints status and progress messages */
  var verbose = true
  /** Writes results to output files if true */
  var logging:Boolean = true
  /** Name of the experiment. That is also the name of the  output file */
  val name:String

  /** Displays on screen if verbose is true */
  def display(fmt:String, args:Any*)  { if(verbose) printf(fmt, args:_*) }

  /** Adds a variable and its possible values to the experiment */
  def add(variable:XVariable) { variables += variable }
  /** Adds an output and the computed statistics to the experiment */
  def add(output:String, stats:XStatistic*)  { statistics += output -> stats}

  /** Override: Performs one evaluation for the current variable values and returns some results */
  def evaluate:Iterable[R]
  /** Override: Returns the result with the given name from results, e.g. AUC */
  def result(results:R, name:String):Double

  /** List of outputs created during evaluation */
  private var outputs = Seq[(Seq[String],Seq[Double])]()
  /** Collects evaluation outputs and prints out progress */
  private def collect() {
    if(verbose) printf("%6.1f\n", progress)
    step += 1
    val results = evaluate.toSeq
    val varvals = variables.map(_.value.name)
    outputs ++= results.map(r => (varvals, output(r)))
    if(logging) writer.write(varvals.mkString("VAL\t","\t","\n"))
    if(logging) results.foreach(r => r.write(writer))
  }
  /** Create output vectors from a result */
  private def output(results:R) = statistics.keysIterator.map(result(results,_)).toSeq

  /** Iterates over all possible combinations of variable values */
  private def iterate(i:Int = 0) {
    while(variables(i).hasNext) {
      if(i < variables.length-1) iterate(i+1) else collect()
      variables.map(_.hasChanged = false)
      variables(i).increment()
    }
    variables(i).reset()
  }

  /** Current value content of the variable with the given name */
  protected def value[T](name:String):T = variable(name).value.content.asInstanceOf[T]
  /** Indicates of the specified variable has changed */
  protected def hasChanged(name:String):Any = variable(name).hasChanged
  /** Last value of the variable range for the variable with the given name */
  protected def last(name:String):Any = variable(name).last.content
  /** First value of the variable range for the variable with the given name */
  protected def fist(name:String):Any = variable(name).first.content
  /** Variable with the given name */
  protected def variable(name:String):XVariable =
    try {variables.find(_.name == name).get}
    catch {case _ => sys.error("Unknown variable: "+name) }


  /** Summarizes the evaluation outputs for the given variable names */
  def summarize(varnames:Iterable[String]) = {
    def nameidx(varname:String):Int = variables.indexWhere(v => v.name==varname)
    varnames.filter(varname => nameidx(varname) == -1).foreach(varname => sys.error("Unknown variable in summary: "+varname))
    val varidxs = varnames.map(nameidx)
    val statidxs = statistics.map(_._2).zipWithIndex
    // while very compact, I think this is not exactly readable code. Have to work on that
    outputs.groupBy(t => varidxs.map(i => t._1(i))).
            mapValues(es => statidxs.map{case(s,i) => s.map(f => f(es.map(e => e._2(i))))})
  }

  /** Shows or stores a summary of the evaluation outputs for the given variable names.
   * If verbose == true the summary is printed to the screen.
   * If logging == true the summary is written to a file <name_varnames>.sum
   * @param varnames Collection of variable names, e.g. "folds","Classifier" */
  def summary(varnames:String*) {
    if(verbose) {
      printf("SUMMARY: %s\n",varnames.mkString(", "))
      println(statistics.map{case(o,ss) => "  %s|%s" format (o,ss.mkString(":"))}.mkString)
      for((es,os) <- summarize(varnames).toSeq.sortBy(_._1.mkString))
        printf("%s :\t %s\n", os.map(_.map("%6.2f" format _).mkString).mkString("   "), es.mkString(" "))
    }
    if(logging) {
      val writer = new FileWriter("log/"+name+"_"+varnames.mkString("_")+".sum")
      writer.write("SUMMARY\t%s\n" format name)
      writer.write(varnames.mkString("VAR\t","\t","\n"))
      writer.write(statistics.map{case(o,ss) => ss.map(_+" "+o).mkString("\t")}.mkString("COL\t","\t","\t"))
      writer.write(varnames.mkString("","\t","\n"))
      for((es,os) <- summarize(varnames).toSeq.sortBy(_._1.mkString))
        writer.write("DAT\t%s\t%s\n" format (os.map(_.map("%.2f" format _).mkString("\t")).mkString("\t"), es.mkString("\t")))
      writer.close()
    }
  }

  /** Current progress of the experiment in percent */
  protected def progress = 100.0*step/variables.map(_.length).reduceLeft(_*_)

  /** Runs the experiment. */
  protected def run() {
    if(logging) writer = new FileWriter("log/"+name+".out")
    if(logging) writer.write("NAME\t%s\n" format name)
    if(logging) writer.write(variables.map(_.name).mkString("VAR\t","\t","\n"))
    if(verbose) display("%s:\nrunning...\n",name)
    step = 0
    iterate()
    if(verbose) display("finished.\n\n")
    if(logging) writer.close()
  }

}


/**
 * Base class for experiments on binary classifiers.
 * Defines a set of performance metrics to assess the prediction power of the classifier.
 */
abstract class XPerimentBinaryClassifier extends XPeriment[CResult] {
  val labels = List("n","y")    // symbolic class labels for binary classifier

  /** Performance metrics that are reported */
  add("AUC", Mean,Std)
  add("ROC20", Mean,Std)
  add("ROC50", Mean,Std)
  add("MCC", Mean)
  add("F1", Mean)
  add("ACC", Mean)
  add("SN", Mean)
  add("SP", Mean)
  add("PRE", Mean)
  add("REC", Mean)

  /** Definition of performance metrics that can be used above */
  def result(results:CResult, name:String) = name match {
      case "AUC"   => results.auc(1)
      case "ROC20" => results.auc(1,20)
      case "ROC50" => results.auc(1,50)
      case "MCC"   => results.mcc(1)
      case "F1"    => results.F1(1)
      case "ACC"   => results.acc
      case "SN"    => results.sensitivity(1)
      case "SP"    => results.specificity(1)
      case "PRE"   => results.precision(1)
      case "REC"   => results.recall(1)
      case _ => sys.error("Unknown result requested: "+name)
    }
}


/**
 * A usage example.
 */
object XperimentExample extends XPeriment[CResult]  {
  val name = "ExampleExperiment"
  val sampleSet = SampleSet(
     DenseSample("1 1", 1),
     DenseSample("2 2", 1),
     DenseSample("3 3", 1),
     DenseSample("4 4", 0),
     DenseSample("5 5", 0),
     DenseSample("6 6", 0)
    )
  val labels = List("no","yes")    // class labels

  add(XVariable("folds", 3,5,1))
  add(XVariable("classifier",
                ("Majority", new Majority(labels)),
                ("KNN(1)", new SimpleKNN(labels, 1))))

  add("AUC", Mean,Std)
  add("MCC", Mean)
  add("ACC", Mean)

  def result(results:CResult, name:String) = name match {
      case "AUC"   => results.auc(1)
      case "MCC"   => results.mcc(1)
      case "ACC"   => results.acc
      case _ => sys.error("Unknown result requested: "+name)
    }
  
  def evaluate = {
    val folds = value[Int]("folds")
    val classifier = value[Classifier]("classifier")
    XValidation(sampleSet,folds,1).map(classifier.evaluate)
  }

  def main(args:Array[String]) = {
    run()
    summary("folds","classifier")
    summary("classifier")
  }  
}