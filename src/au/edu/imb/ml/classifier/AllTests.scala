package au.edu.imb.ml.classifier

import org.scalatest.Suites

class AllTests extends Suites(
    new MajorityTest,
    new SimpleKNNTest,
    new WekaTest
)

object AllTests extends App {
  (new AllTests).execute()
}