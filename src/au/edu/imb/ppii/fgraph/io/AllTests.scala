package au.edu.imb.ppii.fgraph.io

import org.scalatest.Suites

class AllTests extends Suites(
  new FGraphReaderGOTest
)

object AllTests extends App {
  (new AllTests).execute()
}