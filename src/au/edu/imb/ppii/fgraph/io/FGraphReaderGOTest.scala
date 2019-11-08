package au.edu.imb.ppii.fgraph.io

import au.edu.imb.ppii.go.Ontology._
import org.scalatest.FunSuite



/**
 * Tests the reading of a graph of the following structure:
 * 0                   1
 *                  /      \
 * 1            2             3 
 *            /   \       /  |  \
 * 2         4     5      6  7  8
 *           |  X  | 
 * 3         9     10
 */
class FGraphReaderGOTest extends FunSuite  { 

  val graph = (new FGraphReaderGO(cellularComponent)).read("test/go_test2.obo")

  implicit def label2node(label:String) = graph(label)

   test("contains") {
     expect(10)(graph.size)
     assert(graph.contains("GO:0000001"))
     assert(graph.contains("GO:0000002"))
     assert(graph.contains("GO:0000003"))
     assert(graph.contains("GO:0000004"))
     assert(graph.contains("GO:0000005"))
     assert(graph.contains("GO:0000006"))
     assert(graph.contains("GO:0000007"))
     assert(graph.contains("GO:0000008"))
     assert(graph.contains("GO:0000009"))
     assert(graph.contains("GO:0000010"))
   }
   
   test("relations") {
     expect(0)(graph.parents("GO:0000001").size)
     expect(2)(graph.children("GO:0000001").size)
     expect(1)(graph.parents("GO:0000002").size)
     expect(2)(graph.children("GO:0000002").size)
     expect(1)(graph.parents("GO:0000003").size)
     expect(3)(graph.children("GO:0000003").size)
     expect(1)(graph.parents("GO:0000004").size)
     expect(2)(graph.children("GO:0000004").size)
     expect(1)(graph.parents("GO:0000005").size)
     expect(2)(graph.children("GO:0000005").size)
     expect(1)(graph.parents("GO:0000006").size)
     expect(0)(graph.children("GO:0000006").size)
     expect(1)(graph.parents("GO:0000007").size)
     expect(0)(graph.children("GO:0000007").size)
     expect(1)(graph.parents("GO:0000008").size)
     expect(0)(graph.children("GO:0000008").size)
     expect(2)(graph.parents("GO:0000009").size)
     expect(0)(graph.children("GO:0000009").size)
     expect(2)(graph.parents("GO:0000010").size)
     expect(0)(graph.children("GO:0000010").size)
   }
   
   test("ancestors") {
      val term9 = graph("GO:0000009")
      val term5 = graph("GO:0000005")
      val ancestors9 = Set("GO:0000005", "GO:0000002", "GO:0000004", 
          "GO:0000009", "GO:0000001").map(graph(_))
      val ancestors5 = Set("GO:0000005", "GO:0000002", 
          "GO:0000001").map(graph(_))
    
      expect(ancestors5)(graph.ancestors(term5))
      expect(ancestors9)(graph.ancestors(term9))
   }
   
   test("levels") {
     expect(0)(graph("GO:0000001").level)
     expect(1)(graph("GO:0000002").level)
     expect(1)(graph("GO:0000003").level)
     expect(2)(graph("GO:0000004").level)
     expect(2)(graph("GO:0000005").level)
     expect(2)(graph("GO:0000006").level)
     expect(2)(graph("GO:0000007").level)
     expect(2)(graph("GO:0000008").level)
     expect(3)(graph("GO:0000009").level)
     expect(3)(graph("GO:0000010").level)
   }
   
   test("isLinked") {
     assert(graph.isLinked("GO:0000002","GO:0000001"))
     assert(graph.isLinked("GO:0000003","GO:0000001"))
     assert(graph.isLinked("GO:0000004","GO:0000002"))
     assert(graph.isLinked("GO:0000005","GO:0000002"))
     assert(graph.isLinked("GO:0000006","GO:0000003"))
     assert(graph.isLinked("GO:0000007","GO:0000003"))
     assert(graph.isLinked("GO:0000008","GO:0000003"))
     assert(graph.isLinked("GO:0000009","GO:0000004"))
     assert(graph.isLinked("GO:0000009","GO:0000005"))
     assert(graph.isLinked("GO:0000010","GO:0000005"))
   }
   
} 


object FGraphReaderGOTest extends App {
  (new FGraphReaderGOTest).execute()
}