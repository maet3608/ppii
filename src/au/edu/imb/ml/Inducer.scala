package au.edu.imb.ml

import result.{COutput, Result, RResult, CResult}
import sampleset.{ASample, ASampleSet}
import java.io._

/**
 * Abstract trait describing an inducer, which can be any kind
 * of trainable machine learning algorithm.
 * Author : Stefan Maetschke
 * Version: 1.00
 */
trait Inducer[R <: Result] extends Serializable {
  /** Name of the inducer */
  val name:String

  /** Trains the inducer based on the given set of samples */
  def train[S <: ASample](sampleSet:ASampleSet[S]):Unit

  /** Tests the inducer and returns a performance result */
  def test[S <: ASample](sampleSet:ASampleSet[S]):R

  /** Evaluates the inducer performance by training first and then testing */
  def evaluate[S <: ASample](trainTest:(ASampleSet[S],ASampleSet[S])):R = {
    train(trainTest._1)
    test(trainTest._2)
  }

  /** Better override this with something more useful */
  override def toString  = name
}

/** Static load and save methods for inducers and derived objects */
object Inducer {
  /** Serializes an inducer to the given file */
  def save(inducer:Any, filename:String)  {
    (new ObjectOutputStream(new FileOutputStream(filename))).writeObject(inducer)
  }

  /** Loads a serialized inducer from the given file. Returned object must be casted to proper type */
  def load(filename:String) =
    (new ObjectInputStream(new FileInputStream(filename))).readObject()
}


/**
 * Abstract trait describing a classifier.
 */
trait Classifier extends Inducer[CResult] with Serializable {

  /** Class labels the classifier uses. Override. */
  protected val labels:Seq[String]

  /** Returns the string label for the given class id */
  def label(classID:Int) = labels(classID)

  /** Predicts the class of a sample. Override. */
  def predict[S <: ASample](sample:S):Int

  def test[S <: ASample](sampleSet:ASampleSet[S]) =
    CResult(labels, sampleSet.map(s => COutput(s.classID, predict(s))))

}

/**
 * Trait for classifiers that produce certainty factors.
 * Usage: class MyClassifier extend Classifier with CertaintyFactors
 */
trait CertaintyFactors extends Classifier with Serializable{
  /** Should return a new copy! of certainty factors for a sample. Override. */
  def cfs[S <: ASample](sample:S):Seq[Double]

  /** Predicts class id from largest certainty factor */
  def predict[S <: ASample](sample:S) = {
    val _cfs = cfs(sample)
    _cfs.indexOf(_cfs.max)
  }

  /** Tests and takes certainty factors into account */
  override def test[S <: ASample](sampleSet:ASampleSet[S]) =
    CResult(labels, sampleSet.map(s => COutput(s.classID, cfs(s))))
}



/**
 * Abstract trait describing a regressor.
 */
trait Regressor extends Inducer[RResult] with Serializable {

  /** Output of the regressor for a sample */
  def predict[S <: ASample](sample:S):Double

}