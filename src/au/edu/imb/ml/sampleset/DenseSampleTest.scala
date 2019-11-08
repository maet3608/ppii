package au.edu.imb.ml.sampleset

import org.scalatest.FunSuite


/** Tests for dense sample */
class DenseSampleTest extends FunSuite  {

  test("create") {
    val sample = DenseSample("1 2,3;4", 1)
    expect(1)(sample(0))
    expect(2)(sample(1))
    expect(3)(sample(2))
    expect(4)(sample(3))
    expect(1)(sample.classID)
    expect(4)(sample.length)
  }

} 


object DenseSampleTest extends App {
  (new DenseSampleTest).execute()
}





