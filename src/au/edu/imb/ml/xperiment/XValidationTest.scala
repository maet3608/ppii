package au.edu.imb.ml.xperiment

import org.scalatest.FunSuite
import au.edu.imb.ml.sampleset.{DenseSample, SampleSet}

/** Tests cross-validation **/
class XValidationTest extends FunSuite  {

  def createSampleSet = SampleSet(
     DenseSample("1 1", 1),
     DenseSample("2 2", 0),
     DenseSample("3 3", 1),
     DenseSample("4 4", 0),
     DenseSample("5 5", 1),
     DenseSample("6 6", 1)
    )

  test("size") {
    val sampleSet = createSampleSet
    expect(3*2)(XValidation(sampleSet, 3,2).size)
    expect(2*1)(XValidation(sampleSet, 2,1).size)
  }

  test("folds") {
    val sampleSet = createSampleSet
    for((train,test) <- XValidation(sampleSet, 3,2)) {
      expect(2)(test.length)
      expect(4)(train.length)
      assert(test.forall(x => !train.exists(y => x==y)))     // test & train are distinctive
      assert(test.forall(x => sampleSet.exists(y => x==y)))  // all test samples in set
      assert(train.forall(x => sampleSet.exists(y => x==y))) // all train samples in set
      assert(test.forall(x => test.count(y => x==y)==1))     // no duplicates in test
      assert(train.forall(x => train.count(y => x==y)==1))   // no duplicates in test
    }
  }

  test("runs") {
    val sampleSet = createSampleSet
    val trainSets = XValidation(sampleSet, 3,1).map(_._1)
    for(set1 <- trainSets; set2 <- trainSets if set1 != set2)
      assert(set1.exists(x => !set2.exists(y => x==y)))     // all train sets are different
    val testSets = XValidation(sampleSet, 3,2).map(_._2)
    for(set1 <- testSets; set2 <- testSets if set1 != set2)
      assert(set1.exists(x => !set2.exists(y => x==y)))     // all test sets are different
  }

}


object XValidationTest extends App {
  (new XValidationTest).execute()
}