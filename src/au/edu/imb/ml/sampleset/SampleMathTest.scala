package au.edu.imb.ml.sampleset

import org.scalatest.FunSuite

import SampleMath.{squared,euclidean}

/** Tests for sparse sample */
class SampleMathTest extends FunSuite  {

  test("squared dense") {
    val sample1 = DenseSample("1 2 3", 1)
    val sample2 = DenseSample("3 1 5", 1)
    val sample3 = DenseSample("1 2 3", 1)
    expect(0.0)(squared(sample1,sample1))
    expect(0.0)(squared(sample1,sample3))
    expect(9.0)(squared(sample1,sample2))
  }

  test("squared sparse") {
    val sample1 = SparseSample(5, "0:2 2:1 3:4", 1)
    val sample2 = SparseSample(5, "0:4 1:2 3:4", 1)
    val sample3 = SparseSample(5, "0:2 2:1 3:4", 1)
    expect(0.0)(squared(sample1,sample1))
    expect(0.0)(squared(sample1,sample3))
    expect(9.0)(squared(sample1,sample2))
  }

  test("euclidean dense") {
    val sample1 = DenseSample("1 2 3", 1)
    val sample2 = DenseSample("3 1 5", 1)
    expect(3.0)(euclidean(sample1,sample2))
  }

  test("euclidean sparse") {
    val sample1 = SparseSample(5, "0:2 2:1 3:4", 1)
    val sample2 = SparseSample(5, "0:4 1:2 3:4", 1)
    expect(3.0)(euclidean(sample1,sample2))
  }

}


object SampleMathTest extends App {
  (new SampleMathTest).execute()
}