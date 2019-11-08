package au.edu.imb.ppii.fgraph

import org.scalatest.Suites

class AllTests extends Suites(
  new FGeneratorTest,
  new au.edu.imb.ppii.fgraph.io.AllTests
)

object AllTests extends App {
  (new AllTests).execute()
}