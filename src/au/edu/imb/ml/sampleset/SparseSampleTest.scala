package au.edu.imb.ml.sampleset

import org.scalatest.FunSuite


/** Tests for sparse sample */
class SparseSampleTest extends FunSuite  {

  def createDenseSample  = SparseSample(4, "0:1 1:2 2:3 3:4", 1)
  def createSparseSample = SparseSample(9, "0:1 4:2 6:3 7:4", 1)

  test("create dense") {
    val sample = createDenseSample
    expect(1)(sample(0))
    expect(2)(sample(1))
    expect(3)(sample(2))
    expect(4)(sample(3))
    expect(1)(sample.classID)
    expect(4)(sample.length)
  }

  test("create sparse") {
    val sample = createSparseSample
    expect(1)(sample(0))
    expect(0)(sample(1))
    expect(0)(sample(2))
    expect(0)(sample(3))
    expect(2)(sample(4))
    expect(0)(sample(5))
    expect(0)(sample(5))
    expect(3)(sample(6))
    expect(4)(sample(7))
    expect(0)(sample(8))
    expect(1)(sample.classID)
    expect(9)(sample.length)
  }


  test("dense iterator") {
    val sample = createSparseSample
    expect(List(1.0,0.0,0.0,0.0,2.0,0.0,3.0,4.0,0.0))(sample.iterator.toList)
  }

  test("sparse components") {
    val sample = createSparseSample
    expect(Seq((0,1.0),(4,2.0),(6,3.0),(7,4.0)))(sample.components)
  }

} 


object SparseSampleTest extends App {
  (new SparseSampleTest).execute()
}





