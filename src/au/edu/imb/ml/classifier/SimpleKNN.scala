package au.edu.imb.ml.classifier

import au.edu.imb.ml.{CertaintyFactors, Classifier}
import au.edu.imb.ml.sampleset.{ASampleSet, ASample}
import au.edu.imb.ml.sampleset.SampleMath._

/**
 * A simple k-nearest neighbours classifier implementation. Not very efficient.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 02/07/2010
 */

/**
 * k-nearest neighbours classifier
 * @param labels Class labels
 * @param k Number of nearest neighbors
 * @param dist Distance function 
 */
class SimpleKNN(val labels:Seq[String],
                val k:Int = 3,
                val d: (ASample,ASample) => Double = squared)
                extends Classifier  {
  val name = "KNN (%d)" format k

  // stores all training samples
  private var samples:Seq[ASample] = null

  // not very efficient but short
  def predict[S <: ASample](sample:S) = 
    samples.sortBy(s => d(sample,s)).take(k).groupBy(_.classID).
    reduceLeft((a,b) => if(a._2.length > b._2.length) a else b)._1

  // stores all training samples
  def train[S <: ASample](sampleSet:ASampleSet[S]) =
    samples = sampleSet.toSeq
}