package au.edu.imb.sandbox

import scala.actors.Future
import scala.actors.Futures._

object FuturesExample {
  def main(args: Array[String]) {

    var results = List[Future[Int]]()
    for (i <- 1 to 10) {
      println("Sending " + i + "...")
      val f = future {
        println("Processing " + i + "...")
        Thread.sleep(500)
        println("Processed " + i)
        i
      }
      println("Sent " + i)
      results ::= f
    }

    results.foreach(f => println("result: " + f()))
  }
}