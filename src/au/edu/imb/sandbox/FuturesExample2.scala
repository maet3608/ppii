import actors.Future

import scala.actors.Future
import scala.actors.Futures._

object FuturesExample2 {
  def main(args: Array[String]) {
    println("Sending... ")
    val f = future {
        println("Processing...")
        Thread.sleep(100)
        println("Processed ")
        42
      }
      println("Sent")
      println("waiting result is "+f())
      println("Finished")
  }
}