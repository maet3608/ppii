package au.edu.imb.ml.xperiment

import scala.math.sqrt

/**
 * Computes various simple statistics (e.g. mean, standard deviation)
 * over results of an XPeriment.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 02/07/2010
 */

abstract class XStatistic(val name:String) {
  def apply(xs:Seq[Double]):Double
  override def toString = name
}

/** Mean value */
object Mean extends XStatistic("mean") {
   def apply(xs:Seq[Double]) = xs.sum/xs.length
}

/** Median value */
object Median extends XStatistic("median") {
   def apply(xs:Seq[Double]) = {
     val (lower, upper) = xs.sortWith(_<_).splitAt(xs.size/2)
     if (xs.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
   }
}

/** Mode value */
object Mode extends XStatistic("mode") {
   def apply(xs:Seq[Double]) = {
     val grouped = xs.groupBy(x => x).mapValues(_.size)
     val max = grouped.map(_._2).max
     grouped.find(_._2 == max).get._1
   }
}

/** Variance */
object Variance extends XStatistic("variance") {
   def apply(xs:Seq[Double]) = {
     val mean = Mean(xs)
     xs.map(x => (mean-x)*(mean-x)).sum
   }
}

/** Standard deviation */
object Std extends XStatistic("std") {
   def apply(xs:Seq[Double]) = sqrt(Variance(xs))
}

