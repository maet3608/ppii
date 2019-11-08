package au.edu.imb.ppii

import org.scalatest.Suites

class AllTests extends Suites(
  new au.edu.imb.ppii.go.AllTests,
  new au.edu.imb.ppii.utils.AllTests,
  new au.edu.imb.ppii.network.AllTests,
  new au.edu.imb.ppii.predictor.AllTests,
  new au.edu.imb.ppii.fgraph.AllTests
)

object AllTests extends App {
  (new AllTests).execute()
}