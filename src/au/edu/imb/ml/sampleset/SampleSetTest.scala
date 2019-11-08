package au.edu.imb.ml.sampleset

import org.scalatest.FunSuite


/** Tests for sample set */
class SampleSetTest extends FunSuite  {

  def createSampleSet = SampleSet(
     DenseSample("1 2 3", 1),
     DenseSample("2 3 4", 0),
     DenseSample("3 4 5", 1),
     DenseSample("4 5 6", 1),
     DenseSample("5 6 7", 0)
  )

  test("create") {
    val sampleSet = createSampleSet
    expect(5)(sampleSet.length)

    expect(1)(sampleSet(0)(0))
    expect(2)(sampleSet(1)(0))
    expect(3)(sampleSet(2)(0))
    expect(4)(sampleSet(3)(0))
    expect(5)(sampleSet(4)(0))

    expect(1)(sampleSet(0).classID)
    expect(0)(sampleSet(1).classID)
    expect(1)(sampleSet(2).classID)
    expect(1)(sampleSet(3).classID)
    expect(0)(sampleSet(4).classID)
  }

  test("dimension") {
    val sampleSet = createSampleSet
    expect(3)(sampleSet.dimension)
  }

  test("distribution") {
    val sampleSet = createSampleSet
    val distribution = sampleSet.distribution
    expect(2)(distribution(0))
    expect(3)(distribution(1))
  }

  test("balance") {
    val sampleSet = createSampleSet.balance
    println(sampleSet)
    val distribution = sampleSet.distribution
    expect(2)(distribution(0))
    expect(2)(distribution(1))
  }

  test("features") {
    val sampleSet = createSampleSet
    expect(List(Numeric("F_0"),Numeric("F_1"),Numeric("F_2")))(sampleSet.features.toList)
  }
  
  test("shuffle") {
    val sampleSet = createSampleSet
    val shuffledSet = sampleSet.shuffle
    //can fail! That's random for you
    assert((sampleSet zip shuffledSet).forall(t => t._1 == t._2) == false)
  }

} 


object SampleSetTest extends App {
  (new SampleSetTest).execute()
}





