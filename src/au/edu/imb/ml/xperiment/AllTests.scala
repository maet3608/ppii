package au.edu.imb.ml.xperiment

import org.scalatest.Suites


class AllTests extends Suites(
    new XValidationTest
)

object AllTests extends App {
  (new AllTests).execute()
}