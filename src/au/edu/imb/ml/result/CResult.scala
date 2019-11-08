package au.edu.imb.ml.result
/**
 * Stores the outputs of a classifier and evaluates its performance
 * using various metrices such as Accuracy, MCC, AUC, ...
 * Author:  Stefan Maetschke
 * Version: 1.00
 * Date :   15/06/2010
 */

import scala.math.{exp,log,sqrt,abs}
import java.io.{Writer, File, FileWriter, BufferedWriter}
import com.sun.jmx.snmp.agent.SnmpMibSubRequest

/**
 * Describes a classifier output with its target value.
 * @param target ID of target class
 * @param predicted ID of predicted class
 * @param cfs Option vector with confidence values for each class
 */
class COutput(val target:Int, val predicted:Int, cfs:Option[Seq[Double]]) {

  /** Getter for a confidence value for the given class */
  def cf(classID:Int) = cfs match {
    case Some(cfs) => cfs(classID)
    case None =>  if(classID==predicted) 1.0 else 0.0
  }

  /** Writes target, predicted values and confidence factors to a writer, eg. a FileWriter */
  def write(writer:Writer) {
    writer.write("OUT\t%d\t%d" format (target, predicted))
    cfs.foreach{cs  => writer.write(cs.map("%.4f" format _).mkString("\t","\t",""))}
    writer.write("\n")
  }

}

/** Classifier output factory */
object COutput {
  /** Constructor.
   * @param target ID of target class
   * @param predicted ID of predicted class
   * @param cfs Option vector with confidence values for each class
   */
  def apply(target:Int, predicted:Int, cfs:Option[Seq[Double]]) =
    new COutput(target, predicted, cfs)

  /**
   * @param target ID of target class
   * @param cfs Option vector with confidence values for each class
   */ 
  def apply(target:Int, cfs:Seq[Double]) =
    new COutput(target, cfs.indexOf(cfs.max), Some(cfs))

  /**Constructor.
   * @param target ID of target class
   * @param predicted ID of predicted class
   * @param cfs Vector with certainty factors values for each class.
   */
  def apply(target:Int, predicted:Int, cfs:Seq[Double]) =
    new COutput(target, predicted, Some(cfs))

  /** Constructor.
   * @param target ID of target class
   * @param predicted ID of predicted class
   */
  def apply(target:Int, predicted:Int) =
    new COutput(target, predicted, None)
}


/**
 * Stores the outputs of a classifier and evaluates its performance.
 * @param labels Class labels
 */
class CResult(val labels:Seq[String], outputs:Iterable[COutput]) extends Result
        with Iterable[COutput] {
  /** Number of classes */
  val nClasses = labels.length
  /** class indices and therefore indices of the confusion matrix  */
  val cindices = labels.indices
  /** Confusion matrix */
  val cmatrix = Array.ofDim[Int](nClasses, nClasses)

  /** Fill confusion matrix */
  outputs.foreach(o => cmatrix(o.target)(o.predicted) += 1)


  /** Getter for value in confusion matrix */
  def apply(target:Int, predicted:Int):Int = cmatrix(target)(predicted)

  /** Results is iterable over classifier outputs */
  def iterator = outputs.iterator

  /** Safe division x/y */
  private def div(x:Double, y:Double) = if(y != 0) x/y else 0.0

  /** Trace of confusion matrix */
  def trace = cindices.map(i=>cmatrix(i)(i)).sum

  /** Sum over all elements within confusion matrix */
  def sum = (for(i <- cindices; j <- cindices) yield cmatrix(i)(j)).sum

  /** Sums over all elements of the given row */
  def rowSum(row:Int) = cindices.map(col=>cmatrix(row)(col)).sum

  /** Sums over all elements of the given col */
  def colSum(col:Int) = cindices.map(row=>cmatrix(row)(col)).sum

  /** True Positives */
  def TP(classID:Int) = cmatrix(classID)(classID)

  /** True Negatives */
  def TN(classID:Int) = trace - TP(classID)

  /** False Positives */
  def FP(classID:Int) = colSum(classID) - TP(classID)

  /** False Negatives */
  def FN(classID:Int) = rowSum(classID) - TP(classID)

  /** Accuracy over all classes */
  def acc:Double = div(trace,sum)

  /** Accuracy for the given class. This is usually not what you want.
   * Use acc instead */
  def acc(classID:Int) = div(TP(classID),sum)

  /** Balanced accuracy over all classes */
  def bacc:Double = div(cindices.map(bacc).sum, nClasses)

  /** Balanced accuracy for the given class,
   * which is the number of true positives divided by the number of class samples */
  def bacc(classID:Int) = div(TP(classID), rowSum(classID))


  /** Error rate over all classes */
  def error = div(sum-trace,sum)

  /** Geometric mean over all class accuracies */
  def gm = exp(div(cindices.map(i => log(acc(i))).sum, nClasses))

  /**
   * Calculates the sensitivity (=TP/(TP+FN)) for a confusion matrix. The matrix
   * must be square but can contain more than two classes.
   * Recall and sensitivity are identical.
   * Reference:
   * Lu, Z.; Szafron, D.; Greiner, R.; Lu, P.; Wishart, D.; Poulin, B.;
   * Anvik, J.; Macdonell, C. & Eisner, R.
   * Predicting subcellular localization of proteins using machine-learned
   * classifiers.
   * Bioinformatics, 2004 , 20 , 547-556
   * See also www.cs.cornell.edu/Courses/cs578/ 2003fa/performance_measures.pdf
   * @param classID Class ID.
   */
  def sensitivity(classID:Int) = div(TP(classID), rowSum(classID))

  /** Calculates the average sensitivity over all classes. */
  def sensitivity:Double = cindices.map(sensitivity).sum/nClasses

  /**
   * Calculates the specificity for a confusion matrix. There is
   * some confusion about the definition of specificity. Sometimes specificity is
   * used interchangeably with precision, however this appears to be wrong
   * (see reference). True is, that recall and sensitivity are identical.
   * Reference:
   * Lu, Z.; Szafron, D.; Greiner, R.; Lu, P.; Wishart, D.; Poulin, B.;
   * Anvik, J.; Macdonell, C. & Eisner, R.
   * Predicting subcellular localization of proteins using machine-learned
   * classifiers.
   * Bioinformatics, 2004 , 20 , 547-556
   * See also www.cs.cornell.edu/Courses/cs578/ 2003fa/performance_measures.pdf
   * @param classID Class ID.
   * @return Returns the specificity
   */
  def specificity(classID:Int) = {
    val fns:Double = sum - rowSum(classID) // all - TP + FN = all false negatives
    div(TN(classID), fns)
  }

  /** Calculates the average specificity over all classes. */
  def specificity:Double = cindices.map(specificity).sum/nClasses

  /** Calculates the precision for the given class */
  def precision(classID:Int) = div(TP(classID),TP(classID)+FP(classID))

  /** Calculates the recall for the given class */
  def recall(classID:Int) = div(TP(classID),TP(classID)+FN(classID))

  /** Calculates the F1 score */
  def F1(classID:Int) = div(2.0*TP(classID),2.0*TP(classID)+FN(classID)+FP(classID))

  /** Calculates the F score. For beta == 1.0 this becomes the F1 score */
  def F(classID:Int, beta:Double) = {
    val b2 = beta*beta
    div((1+b2)*TP(classID),(1+b2)*TP(classID)+b2*FN(classID)+FP(classID))
  }

  /**
   * Area under the ROC curve (AUC).
   * @param classID is the ID of the positive class; typically 1 for a binary problem.
   *                For multi-class problems samples of all other classes are treated as negatives.
   * @param maxFP can be set to compute the AUC until the first maxFP false positives are encountered,
   *               e.g. the AUC50/ROC50 can be computed by auc(1,50)
   *               maxFP = 0 means maxFP is ignored and the standard AUC is computed.
   */
  def auc(classID:Int, maxFP:Int=0) = {
    def area(x1:Double,x2:Double,y1:Double,y2:Double):Double = abs(x1-x2)*(y1+y2)/2.0
    var n = 0                    // counter for processed outputs
    var tp,  fp  = 0             // true and false positives
    var otp, ofp = 0             // old true and false positives
    var oldcf = Double.NaN
    var auc = 0.0

    for(output <- outputs.toSeq.sortBy(_.cf(classID)) if maxFP == 0 || fp < maxFP) {
      if(oldcf != output.cf(classID)) {
        auc += area(fp,ofp,tp,otp)
        ofp = fp; otp = tp
        oldcf = output.cf(classID)
      }
      if(output.target == classID) tp += 1 else fp += 1
      n += 1
    }

    val (np,nn) = (tp,n-tp)      // number of positives and negatives
    auc = div(auc+area(nn,ofp,np,otp), np*nn)
    if(auc < 0.5) 1.0-auc else auc
  }

  /**
    * Calculates Matthews correlation coefficient between two classes for
    * a confusion matrix. The matrix must be square but can contain more
    * than two classes.
    * see Matthews, B. W. (1975). Comparison of predicted and observed
    * secondary structure of t4 phage lysozyme. Biochim Biophys Acta, 405:
    * 442-451.
    * @param classID1 Id of the first class. Must be < getRowNumber()-1.
    * @param classID2 Id of the second class. Must be < getRowNumber()-1.
    * @return Matthews correlation coefficient [-1,+1].
    */
  def mcc(classID1:Int, classID2:Int) = {
    val maxClass = nClasses-1
    val den = sqrt(1.0*colSum(classID1)*colSum(classID2)*rowSum(classID1)*rowSum(classID2))
    val num = 1.0*TP(classID1)*TP(classID2)-
                 1.0*cmatrix(classID1)(maxClass-classID1)*cmatrix(classID2)(maxClass-classID2)
    div(num,den)
  }

  /** Calculates the average Matthews correlation coefficient over all classes. */
//  def mcc:Double = div(cindices.map(mcc).sum, nClasses)
  /**
    * Calculates Matthews correlation coefficient for multiple class problems.
    * @param classID Class ID.
    */
  def mcc(classID:Int) = {
    val tp:Double = TP(classID)      // true positives
    val tn:Double = TN(classID)      // true negatives
    val fp:Double = FP(classID)      // false positives
    val fn:Double = FN(classID)      // false negatives
    val den = sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
    div(tp*tn-fp*fn, den)
  }


  /** Writes outputs using a writer, eg. a FileWriter */
  def write(writer:Writer) {
    writer.write("RES\n")
    outputs.foreach(o => o.write(writer))
  }

  override def toString = {
    // problem with scala.StringBuilder in beta-release 2.8. Use java instead
    val buf = new StringBuilder
    for(row <- cindices) {
      buf.append(cindices.map(col => " %4d" format cmatrix(row)(col)).mkString)
      buf.append("     |%-5d :%s\n" format(rowSum(row),labels(row)))
    }
    buf.toString()
  }

}


/**
 * Classifier result factory
 */
object CResult {
  
  /** Creates classifier result with the given class labels and classifier outputs */
  def apply(labels:Seq[String], outputs:Iterable[COutput]) =
    new CResult(labels, outputs)

  /** Creates classifier result for n classes and the given outputs */
  def apply(n:Int, outputs:Iterable[COutput]) =
    new CResult((1 to n).map("Class_"+_), outputs)

  /** Creates classifier result from other classifier results */
  def apply(results:Iterable[CResult]) =
    new CResult(results.head.labels, results.flatMap(r => r))

  /** Writes classifier results to a file */
  def write(result:CResult, filepath:String) {
    val writer = new BufferedWriter(new FileWriter(new File(filepath)))
    writer.write("%s\n" format result.labels.mkString("\t"))
    result.foreach(o => writer.write(o.toString))
    writer.close()
  }

}