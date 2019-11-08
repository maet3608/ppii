package au.edu.imb.ml.sampleset

import org.scalatest.Suites

class AllTests extends Suites(
  new DenseSampleTest,
  new SparseSampleTest,
  new SampleSetTest,
  new SampleMathTest
)

object AllTests extends App {
  (new AllTests).execute()
}