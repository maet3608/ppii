package au.edu.imb.ml.result

import org.scalatest.Suites

class AllTests extends Suites(
  new CResultTest
)

object AllTests extends App {
  (new AllTests).execute()
}