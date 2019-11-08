package au.edu.imb.ml.xperiment

import au.edu.imb.ml.sampleset.{SampleSet, ASampleSet, ASample}

/**
 * Creation of cross-validation sets.
 * Author : Stefan Maetschke
 * Version: 1.00
 * Date   : 29/06/2010
 */


/**
 * Generator for cross-validation sets for any type of sequence.
 * See generator for sample sets below.
 * @param samples Sequence of objects to split into folds
 * @params folds Number of folds
 * @params runs Number of runs. Each run randomly shuffles the samples
 */
class XVal[S](var samples:Seq[S], folds:Int, runs:Int) extends Iterable[(Seq[S], Seq[S])] {

  /** Creates a fold for the given index. index must be [0,folds-1] */
  private def fold(index:Int) = {
    require(index >=0 && index < folds, "Invalid fold index: "+index)

    val start = index*samples.length/folds
    val end = (index+1)*samples.length/folds
    def isTest(i:Int) = i >= start && i < end
    val(testSet, trainSet) = samples.zipWithIndex.partition(t => isTest(t._2))
    (trainSet.map(_._1), testSet.map(_._1))
  }

  /** Shuffles the samples of the sample set. Do not call between folds! */
  private def shuffle() { samples = scala.util.Random.shuffle(samples) }

  /** Iterator over all folds */
  def iterator =  for(run <- Iterator.range(0,runs);
                      val dummy = shuffle();
                      index <- Iterator.range(0,folds)) yield fold(index)
}

/** Factory */
object XVal {
  def apply[S](samples:Seq[S], folds:Int = 10, runs:Int = 1) = new XVal(samples, folds, runs)
}



/**
 * Generator for cross-validation sample sets.
 * @param sampleSet Sample set to split into folds
 * @params folds Number of folds
 * @params runs Number of runs. Each run shuffles the sample set
 */
class XValidation[S <: ASample](sampleSet:ASampleSet[S], val folds:Int, val runs:Int) extends
  Iterable[(ASampleSet[S], ASampleSet[S])] {
  var samples = sampleSet.toSeq

  /** Creates a fold for the given index. index must be [0,folds-1] */
  private def fold(index:Int) = {
    require(index >=0 && index < folds, "Invalid fold index: "+index)

    val start = index*samples.length/folds
    val end = (index+1)*samples.length/folds
    def isTest(i:Int) = i >= start && i < end
    val(testSet, trainSet) = samples.zipWithIndex.partition(t => isTest(t._2))
    
    (sampleSet.create(trainSet.map(_._1)), sampleSet.create(testSet.map(_._1)))
  }

  /** Shuffles the samples of the sample set. Do not call between folds! */
  private def shuffle() { samples = scala.util.Random.shuffle(samples) }

  /** Iterator over all folds */
  def iterator =  for(run <- Iterator.range(0,runs);
                      val dummy = shuffle();
                      index <- Iterator.range(0,folds)) yield fold(index)
}


/** Factory */
object XValidation {
  def apply[S <: ASample](sampleSet:ASampleSet[S], folds:Int = 10, runs:Int = 1) =
    new XValidation(sampleSet, folds, runs)
}