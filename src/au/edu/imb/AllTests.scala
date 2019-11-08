package au.edu.imb

import org.scalatest.Suites


class AllTests extends Suites(
    new ddi.AllTests,
    new logreg.AllTests,
    new ml.AllTests,
    new ppii.AllTests,
    new structure.AllTests
)

object AllTests extends App {
  (new AllTests).execute()
}