package au.edu.imb.ml.classifier

import org.scalatest.FunSuite
import au.edu.imb.ml.sampleset._

/** Tests for Weka classifier */
class WekaTest extends FunSuite  {

  def createDenseSampleSet = SampleSet(
     DenseSample("1 0 1 0", 0),
     DenseSample("2 0 2 0", 0),
     DenseSample("3 0 3 0", 1),
     DenseSample("4 0 4 0", 1)
    )

  def createSparseSampleSet = SampleSet(
     SparseSample(4, "0:1 2:1", 0),
     SparseSample(4, "0:2 2:2", 0),
     SparseSample(4, "0:3 2:3", 1),
     SparseSample(4, "0:4 2:4", 1)
    )

  def validate[S <: ASample](sampleSet:ASampleSet[S], classifier:Weka) = {
    classifier.train(sampleSet)
    for(i <- 0 until sampleSet.size)
      expect(sampleSet(i).classID)(classifier.predict(sampleSet(i)))
  }

  test("dense train&predict KNN, k=1") {
    validate(
      createDenseSampleSet,
      Weka("weka.classifiers.lazy.IBk", "-K 1 -I", List("y","n"))
    )
  }

  test("sparse train&predict KNN, k=1") {
    validate(
      createSparseSampleSet,
      Weka("weka.classifiers.lazy.IBk", "-K 1 -I", List("y","n"))
    )
  }

  test("sparse train&predict SVM") {
    validate(
      createSparseSampleSet,
      Weka("weka.classifiers.functions.SMO",
        Array("-C","1.0","-K","weka.classifiers.functions.supportVector.RBFKernel -G 0.01"), 
        List("y","n"))
    )
  }

  test("compare sparse & dense") {
    val sparseSampleSet = createSparseSampleSet
    val denseSampleSet  = createDenseSampleSet
    val cSparse = Weka("weka.classifiers.lazy.IBk", "-K 1", List("y","n"))
    val cDense = Weka("weka.classifiers.lazy.IBk", "-K 1", List("y","n"))
    cSparse.train(sparseSampleSet)
    cDense.train(denseSampleSet)

    for(i <- 0 until denseSampleSet.length)  
      assert(cSparse.cfs(sparseSampleSet(i)).toSeq == cDense.cfs(denseSampleSet(i)).toSeq)
  }
}


object WekaTest extends App {
  (new WekaTest).execute()
}