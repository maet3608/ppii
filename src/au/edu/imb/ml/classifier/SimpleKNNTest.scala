package au.edu.imb.ml.classifier

import org.scalatest.FunSuite
import au.edu.imb.ml.sampleset.{DenseSample, SampleSet}

/** Tests for k-nearest neighbour classifier */
class SimpleKNNTest extends FunSuite  {

  def createSampleSet = SampleSet(
     DenseSample("1 1", 0),
     DenseSample("2 2", 0),
     DenseSample("3 3", 1),
     DenseSample("4 4", 1)
    )

  test("train&predict, k=1") {
    val sampleSet = createSampleSet
    val classifier = new SimpleKNN(List("true", "false"), 1)

    classifier.train(sampleSet)

    // should always predict correct class for k=1
    for(i <- 0 until sampleSet.size)
      expect(sampleSet(i).classID)(classifier.predict(sampleSet(i)))
  }

  test("train&predict, k=3") {
    val sampleSet = createSampleSet
    val classifier = new SimpleKNN(List("true", "false"), 1)

    classifier.train(sampleSet)

    for(i <- 0 until sampleSet.size)
      expect(sampleSet(i).classID)(classifier.predict(sampleSet(i)))
  }
}


object SimpleKNNTest extends App {
  (new SimpleKNNTest).execute()
}