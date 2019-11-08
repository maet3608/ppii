package au.edu.imb.ml.classifier

import au.edu.imb.ml.{Classifier, CertaintyFactors}
import au.edu.imb.ml.sampleset.{ASampleSet, ASample}

/**
 * A test classifier that always picks the first class.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 21/06/2011
 */

class Zero(val labels:Seq[String]) extends Classifier with CertaintyFactors {
  val name = "Zero"
  private val ret = Array.fill(labels.length)(0.0)
  ret(0) = 1.0

  def cfs[S <: ASample](sample:S) = ret

  def train[S <: ASample](sampleSet:ASampleSet[S]) = {
    // no training
  }

}