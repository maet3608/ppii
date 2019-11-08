package au.edu.imb.ppii.network

import org.scalatest.Suites


class AllTests extends Suites(
    new ProteinTest,
    new InteractionTest
)

object AllTests extends App {
  (new AllTests).execute()
}