package au.edu.imb.ppii.fgraph

import io.FGraphReaderGO
import au.edu.imb.ppii.go.Ontology._
import org.scalatest.FunSuite

/**
 * Tests the feature generation for a graph of the following structure:
 * 0                   1
 *                  /      \
 * 1            2             3 
 *            /   \       /  |  \
 * 2         4     5      6  7  8
 *           |  X  | 
 * 3         9     10
 */
class FGeneratorTest extends FunSuite  {

  val graph = (new FGraphReaderGO(cellularComponent)).read("test/go_test2.obo")
  val labels1 = Set("GO:0000010")
  val labels2 = Set("GO:0000006","GO:0000003")
  implicit def label2node(label:String) = graph(label)


  test("ulca") {
    val nodes = FGenerator.ulca(graph,labels1,labels2)
    println("ucla: "+nodes)
  }   

  test("spa") {
    val nodes = FGenerator.spa(graph,labels1,labels2)
    println("spa:  "+nodes)
  }

} 


object FGeneratorTest extends App {
  (new FGeneratorTest).execute()
}