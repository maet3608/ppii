package au.edu.imb.ml.classifier

import au.edu.imb.ml.{Classifier, CertaintyFactors}
import au.edu.imb.ml.sampleset.{ASampleSet, ASample}

/**
 * A stupid classifier that simply calculates the mean value of a given input
 * vector. A test classifier that works only for binary problems.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 02/07/2010
 */

class Mean(val labels:Seq[String]) extends Classifier with CertaintyFactors {
  val name = "Mean"

  require(labels.length == 2, "Classifier is binary! Two labels required!")

  def cfs[S <: ASample](sample:S) = {
    val mean = sample.sum/sample.length
    Array(mean, 1.0-mean)
  }

  def train[S <: ASample](sampleSet:ASampleSet[S]) = {
    // no training
  }

}