package au.edu.imb.sandbox

import testing.Benchmark
import Stream.range

object BenchmarkForLoops extends Benchmark {
  multiplier = 100

  val b = 100
  var x = 0

//  override def run = {
//    for(i <- 1 until b)
//      for(j <- 1 until b)
//        for(k <- 1 until b)
//          {x += 1}
//  }

  override def run {
    for(i <- 1 until b; j <- 1 until b; k <- 1 until b) {x += 1}
  }

//  override def run = {
//    for(i <- range(1,b); j <- range(1,b); k <- range(1,b)) {x += 1}
//  }


  override def main(args: Array[String]) {
    println(BenchmarkForLoops.runBenchmark(3))
  }
}