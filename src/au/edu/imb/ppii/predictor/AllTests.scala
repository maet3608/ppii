package au.edu.imb.ppii.predictor

import org.scalatest.Suites

class AllTests extends Suites(
    new NodeTest,
    new DAGTest
)

object AllTests extends App {
  (new AllTests).execute()
}