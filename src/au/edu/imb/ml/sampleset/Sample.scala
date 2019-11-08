package au.edu.imb.ml.sampleset

import scala.math.sqrt

/**
 * Traits and classes describing samples.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 17/06/2010
 */

/**
 * Abstract trait to describe a sample
 */
trait ASample extends Seq[Double] {
  /** Getter for the class the sample belongs too */
  def classID:Int
  /** Weight of the sample */
  def weight:Double = 1.0
  /** Number of values within the sample */
  def dimension = length
  /* Sample component values and class id */
  override def toString = mkString(classID+" -> "," ","")
}


/**
 * Abstract trait for dense samples.
 */
trait ADenseSample extends ASample


/**
 * Abstract trait for sparse samples.
 */
trait ASparseSample extends ASample {
  /** Ordered non-zero components of the form tuples (index,value) */
  def components:Seq[(Int,Double)]
}



/**
 * Dense sample that stores double values.
 * @param classID class the sample belongs too
 * First index is zero, max index is length-1.
 */
class DenseSample(values:Seq[Double], val classID:Int) extends ADenseSample {
  private val elems = values.toArray

  /** Getter for the idx-th element. First index is zero. Max index is length-1. */
  def apply(idx:Int) = elems(idx)

  /** Iterator over the sample elements */
  def iterator = elems.iterator

  /** Length of vector */
  def length = elems.length
}

/**
 * DenseSample factory
 */
object DenseSample {
  /** Creates a sample from a Seq of values.  */
  def apply(values:Seq[Double], classID:Int) = new DenseSample(values, classID)

  /** Creates a sample from a string of values separated by whitespace, comma or semicolon */
  def apply(values:String, classID:Int) =
    new DenseSample(values.split("[\\s;,]+").map(_.toDouble), classID)
}


/**
 * Sparse sample that stores only non-zero values
 * @param length Max number of vector elements == dimension of feature vector.
 * @param components Seq of tuples (index,value) describing non-zero elements.
 *        Need to be sorted with ascending index!
 * @param classID class the sample belongs too
 * First index is zero, max index is length-1.
 */
class SparseSample(val length:Int, val components:Seq[(Int,Double)], val classID:Int) extends ASparseSample {

  /** Getter for the idx-th element. First index is zero.  */
  def apply(idx:Int):Double = {
    var start = 0
    var end   = components.length-1
    while(start <= end) {
      val mid = (start + end) / 2
      val current = components(mid)
      if(idx > current._1)      start = mid + 1
      else if(idx < current._1) end   = mid - 1
      else return current._2
    }
    return 0.0   // not found, therefore zero value
  }

  /** Non-sparse, ordered iterator over the sample vector components */
  def iterator = new ValuesIterator(this)

  class ValuesIterator(sample:SparseSample) extends Iterator[Double] {
    var idx = -1
    def hasNext = idx < sample.length-1
    def next() = { idx += 1; sample(idx) }
  }

  override def toString = components.map{case (i,v) => i+":"+v}.mkString(classID+" -> "," ","")
}

/**
 * SparseSample factory
 */
object SparseSample {
  /** Creates sparse sample from sequence of tuples (index, value) with value!=0.
   *  length is maximum index. */
  def apply(length:Int, values:Seq[(Int,Double)], classID:Int) =
    new SparseSample(length, values, classID)

  /** Creates sample from dense vector of values. */
  def apply(values:Seq[Double], classID:Int) = new SparseSample(values.length,
      values.zipWithIndex.filter(_._1 != 0.0).map{case(v,i) => (i,v)}, classID)

  /** Creates sample from string with sparse values of the form 1:2.5 3:0.7 25:1.1
   *  length is maximum index.*/
  def apply(length:Int,values:String, classID:Int) = new SparseSample(length,
    values.split("[\\s,]+").map{_.split(":") match {case Array(i,v) => (i.toInt,v.toDouble)}}, classID)
}

