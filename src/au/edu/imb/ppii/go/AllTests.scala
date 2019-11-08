package au.edu.imb.ppii.go

import org.scalatest.Suites

class AllTests extends Suites(
  new TermTest,
  new RelationTest,
  new OboParserTest,
  new OntologyTest
)

object AllTests extends App {
  (new AllTests).execute()
}