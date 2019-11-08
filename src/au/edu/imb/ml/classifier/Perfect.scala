package au.edu.imb.ml.classifier

import au.edu.imb.ml.{Classifier, CertaintyFactors}
import au.edu.imb.ml.sampleset.{ASampleSet, ASample}

/**
 * A test classifier that cheats by taking the class label of the sample to always return
 * correct answers (provided the class label is set correctly).
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 21/06/2011
 */

class Perfect(val labels:Seq[String]) extends Classifier with CertaintyFactors {
  val name = "Perfect"

  def cfs[S <: ASample](sample:S) = {
    val ret = Array.fill(labels.length)(0.0)
    ret(sample.classID) = 1.0
    ret
  }

  def train[S <: ASample](sampleSet:ASampleSet[S]) = {
    // no training
  }

}