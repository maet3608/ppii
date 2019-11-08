package au.edu.imb.ml.sampleset

import scala.testing.Benchmark

/**
 * Benchmarking of some functions over samples
 * Author : Stefan Maetschke
 * Version: 1.00
 */

/*
Benchmark results:
dot1: 2031
dot2: 3033
dot3: 31873
dot4: 5984
*/

object SampleMathBenchmark extends Benchmark {
  multiplier = 100

  val sample = new DenseSample(Seq.range(1,100000).map(_.toDouble),0)

  def dot1 = {var sum = 0.0; for(i <- 0 until sample.length) sum += sample(i)*sample(i); sum }
  def dot2 = { var sum = 0.0; sample.foreach(e => sum += e*e); sum }
  def dot3 = sample.map(v => v*v).sum
  def dot4 = sample.iterator.map(v => v*v).reduceLeft(_+_)

  def run = dot4

  override def main(args: Array[String]):Unit =  println(runBenchmark(10).sum)
}