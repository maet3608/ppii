package au.edu.imb.ml.xperiment


/**
 * A variable of the experiment, e.g. number of cross validation folds or a threshold.
 * (Essentially a resettable counter)
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 16/07/2010 */

/**
 * A variable value.
 */
case class XValue(name:String, content:Any)

/**
 * A variable of the experiment
 * @param name Name of variable
 * @param values Collection of values of the variable to iterate over.
 */
class XVariable(val name:String, values:Seq[XValue]) {
  /** Current index of the current value */
  var index = 0
  /** Current value of the variable */
  var value = values(0)
  /** Set when value has changed. We start with a change. Reset from outside */
  var hasChanged = true
  /** Resets the variable to its first value */
  def reset() = { hasChanged = true; index = 0 }
  /** Increments to next value. Returns nothing! Use value instead */
  def increment() = { index+=1; hasChanged = true; value = values(index % values.length) }
  /** Tests whether there are more values */
  def hasNext = index < values.length
  /** Number of values */  
  def length = values.length
  /** First value of the range of values */
  def first = values.head
  /** Last value of the range of values */
  def last = values.last
}


/** Factory for variables */
object XVariable {
  /* Variable over a sequence of XValues represent as tuples */
  def apply(name:String, values:(String,Any)*) =
    new XVariable(name, values.map(v => XValue(v._1,v._2)))
  
  /** Integer counter running from start to end (inclusive) with the givens stride */
  def apply(name:String, start:Int, end:Int, stride:Int = 1) =
    new XVariable(name, (start to end by stride).map(v => XValue(v.toString,v)))

  /** Double counter running from start to end (inclusive) with the givens stride */
  def apply(name:String, start:Double, end:Double, stride:Double) =
    new XVariable(name, (start to end by stride).map(v => XValue(v.toString,v)))
}