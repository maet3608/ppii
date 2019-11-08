package au.edu.imb.ml.sampleset

import javax.management.remote.rmi._RMIConnection_Stub

/**
 * Collections of samples with attribute description.
 * Author : Stefan Maetschke
 * Version: 1.00
 */

/**
 * The following features are essentially an enumeration of the possible
 * value types the columns of the sample set can take on.
 * ATTENTION: update Weka.convert(feature) when adding new
 * feature types
 */
sealed class Feature(val name:String)
case class Numeric(override val name:String) extends Feature(name)
case class Nominal(override val name:String, values:Seq[String]) extends Feature(name)


/**
 * Trait to describe a set of samples
 * E is the type of the elements within a sample
 * S is type of the sample within the set
 */
trait ASampleSet[S <: ASample] extends Seq[S] {
  /** Describes the column types/features of the sample set */
  def features:Seq[Feature]

  /** Dimension of samples within the sample set. Fails for an empty set. */
  def dimension:Int = this(0).dimension

  /** Creates a new samples set that takes over the attributes of this sample set */
  def create(samples:Seq[S]):ASampleSet[S]

  /** Returns map with class IDs as keys and the corresponding number of samples as values */
  def distribution:Map[Int,Int] = this.groupBy(_.classID).mapValues(_.size)

  /** Returns a balanced sample set according to the size of the smallest class */
  def balance = {
    val n = distribution.values.min
    create(this.groupBy(_.classID).flatMap(_._2.take(n)).toSeq)
  }
  /** Returns a string representation of the sample set */
  override def toString = mkString("\n")
}


/**
 * A sample set for samples with double values.
 * @param features Collection of features describing sample set columns
 * @params samples Samples of the set
 */
class SampleSet[S <: ASample](val features:Seq[Feature], samples:Seq[S]) extends ASampleSet[S] {

  /**
   * Auxillary constructor.
   * The sample set features are generated automatically as
   * Numeric(F_0), ..., Numeric(F_n)
   * @params samples Samples of the set
   */
  def this(samples:Seq[S]) =
    this((0 until samples(0).length).map(i => Numeric("F_"+i)), samples)

  /** Creates a new samples set that takes over the features of this sample set */
  def create(samples:Seq[S]) = new SampleSet(features, samples)

  /** Returns a new shuffled sample set */
  def shuffle = create(scala.util.Random.shuffle(samples))

  /** Getter for the idx-th sample */
  def apply(idx:Int) = samples(idx)

  /** Iterator over the samples */
  def iterator = samples.iterator

  /** Number of samples */
  def length = samples.length
}


/**
 * SampleSet factory
 */
object SampleSet {
  /**
   * Constructor.
   * @param features Collection of features describing sample set columns
   * @params samples Samples of the set
   */
  def apply[S <: ASample](features:Seq[Feature], samples:S*) =
    new SampleSet(features, samples)

  /**
   * Constructor.
   * The sample set features are generated automatically as
   * Numeric(F_0), ..., Numeric(F_n)
   * @params samples Samples of the set
   */
  def apply[S <: ASample](samples:S*) =
    new SampleSet(samples)
}
