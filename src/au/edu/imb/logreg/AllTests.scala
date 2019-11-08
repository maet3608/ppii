package au.edu.imb.logreg

import org.scalatest.Suites

class AllTests extends Suites (
  new LogRegTest
)

object AllTests extends App {
  (new AllTests).execute()
}