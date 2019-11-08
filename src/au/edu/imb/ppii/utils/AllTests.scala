package au.edu.imb.ppii.utils

import org.scalatest.Suites

class AllTests extends Suites(
    new AnnotationsTest
)

object AllTests extends App {
  (new AllTests).execute()
}