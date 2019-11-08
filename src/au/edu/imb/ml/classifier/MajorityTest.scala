package au.edu.imb.ml.classifier

import org.scalatest.FunSuite
import au.edu.imb.ml.sampleset.{DenseSample, SampleSet}

/** Tests for majority classifier */
class MajorityTest extends FunSuite  {

  def createSampleSet = SampleSet(
     DenseSample("1", 0),
     DenseSample("2", 1),
     DenseSample("3", 1),
     DenseSample("4", 1)
    )

  test("train&predict") {
    val sampleSet = createSampleSet
    val classifier = new Majority(List("true", "false"))

    classifier.train(sampleSet)

    // always predicts majority class
    for(i <- 0 until sampleSet.size) {
      expect(1)(classifier.predict(sampleSet(i)))
      expect(List(0.25,0.75))(classifier.cfs(sampleSet(i)).toList)
    }  
  }

}


object MajorityTest extends App {
  (new MajorityTest).execute()
}