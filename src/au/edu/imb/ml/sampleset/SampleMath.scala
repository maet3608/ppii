package au.edu.imb.ml.sampleset

import scala.math.sqrt

/**
 * Mathematical functions (e.g. euclidean distance) for samples
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 08/07/2010
 */

object SampleMath {

  /** Sum over squared differences between sample components. Ugly but fast implementation */
  def squared(xs:ASample, ys:ASample):Double = {
    var sum = 0.0
    for(i <- 0 until xs.length; val diff = xs(i)-ys(i))
      sum += diff*diff
    sum
  }

  // Sparse implementation squared distances. Ugly but fast.
  def squared(xs:SparseSample, ys:SparseSample):Double = {
    var sum = 0.0
    for((idx,value) <- xs.components; val diff = value - ys(idx))
      sum += diff*diff  // all non-zeros in xs
    for((idx,value) <- ys.components if xs(idx)==0.0; val diff = ys(idx))
      sum += diff*diff   // remaining non-zeros in ys
    sum
  }

  /** Euclidean distance between two samples. */
  def euclidean[S <: ASample](xs:S, ys:S) = sqrt(squared(xs,ys))

  /** dot product */
  def dot(xs:ASample, ys:ASample):Double = {
    var sum = 0.0
    for(i <- 0 until xs.length)
      sum += xs(i)*ys(i)
    sum
  }

  // Sparse dot product
  def dot(xs:SparseSample, ys:SparseSample):Double = {
    var sum = 0.0
    for((idx,value) <- xs.components)
      sum += value*ys(idx)
    sum
  }

}


