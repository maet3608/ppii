package au.edu.imb.ml

import org.scalatest.Suites


class AllTests extends Suites(
  new classifier.AllTests,
  new result.AllTests,
  new sampleset.AllTests,
  new xperiment.AllTests
)

object AllTests extends App {
  (new AllTests).execute()
}