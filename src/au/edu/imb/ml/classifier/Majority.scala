package au.edu.imb.ml.classifier

import au.edu.imb.ml.{Classifier, CertaintyFactors}
import au.edu.imb.ml.sampleset.{ASampleSet, ASample}

/**
 * A simple majority classifier (only for test purposes). It classifies all
 * samples according to the most frequent class of the training set.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 02/07/2010
 */

class Majority(val labels:Seq[String]) extends Classifier with CertaintyFactors {
  val name = "Majority"

  // class probabilities
  private val probs = Array.ofDim[Double](labels.length)

  // Should return clone of probs array but since probs are constant this is okay
  def cfs[S <: ASample](sample:S) = probs

  // not very efficient but short
  def train[S <: ASample](sampleSet:ASampleSet[S]) = {
    val n:Double = sampleSet.length
    sampleSet.groupBy(_.classID).foreach{t => probs(t._1) = t._2.length/n}
  }
}