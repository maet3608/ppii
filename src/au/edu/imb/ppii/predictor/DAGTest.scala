package au.edu.imb.ppii.predictor

import au.edu.imb.ppii.go._
import org.scalatest.FunSuite

/**
 * Tests a DAG of the following structure:
 * 0                   1
 *                  /      \
 * 1            2             3 
 *            /   \       /  |  \
 * 2         4     5      6  7  8
 *           |  X  | 
 * 3         9     10
 */
class DAGTest extends FunSuite  { 

   def createDAG = new GoDAG(new Ontology(OboParser("test/go_test2.obo")))
   
   test("contains") {
     val dag = createDAG
     expect(10)(dag.size)
     assert(dag.contains("GO:0000001"))
     assert(dag.contains("GO:0000002"))
     assert(dag.contains("GO:0000003"))
     assert(dag.contains("GO:0000004"))
     assert(dag.contains("GO:0000005"))
     assert(dag.contains("GO:0000006"))
     assert(dag.contains("GO:0000007"))
     assert(dag.contains("GO:0000008"))
     assert(dag.contains("GO:0000009"))
     assert(dag.contains("GO:0000010"))
    }
   
   test("links") {
     val dag = createDAG
     expect(0)(dag("GO:0000001").parents.length)
     expect(2)(dag("GO:0000001").children.length)
     expect(1)(dag("GO:0000002").parents.length)
     expect(2)(dag("GO:0000002").children.length)
     expect(1)(dag("GO:0000003").parents.length)
     expect(3)(dag("GO:0000003").children.length)
     expect(1)(dag("GO:0000004").parents.length)
     expect(2)(dag("GO:0000004").children.length)
     expect(1)(dag("GO:0000005").parents.length)
     expect(2)(dag("GO:0000005").children.length)
     expect(1)(dag("GO:0000006").parents.length)
     expect(0)(dag("GO:0000006").children.length)
     expect(1)(dag("GO:0000007").parents.length)
     expect(0)(dag("GO:0000007").children.length)
     expect(1)(dag("GO:0000008").parents.length)
     expect(0)(dag("GO:0000008").children.length)
     expect(2)(dag("GO:0000009").parents.length)
     expect(0)(dag("GO:0000009").children.length)
     expect(2)(dag("GO:0000010").parents.length)
     expect(0)(dag("GO:0000010").children.length)
   } 
  
   test("levels") {
     val dag = createDAG
     expect(0)(dag("GO:0000001").level)
     expect(1)(dag("GO:0000002").level)
     expect(1)(dag("GO:0000003").level)
     expect(2)(dag("GO:0000004").level)
     expect(2)(dag("GO:0000005").level)
     expect(2)(dag("GO:0000006").level)
     expect(2)(dag("GO:0000007").level)
     expect(2)(dag("GO:0000008").level)
     expect(3)(dag("GO:0000009").level)
     expect(3)(dag("GO:0000010").level)
   }
   
   test("ancestors") {
      val dag = createDAG
      val node9 = dag("GO:0000009")
      val node5 = dag("GO:0000005")
      val ancestors9 = Set("GO:0000005", "GO:0000002", "GO:0000004", 
          "GO:0000009", "GO:0000001").map(dag(_))
      val ancestors5 = Set("GO:0000005", "GO:0000002", 
          "GO:0000001").map(dag(_))
    
      expect(ancestors5)(dag.ancestors(node5))
      expect(ancestors9)(dag.ancestors(node9))
      expect(ancestors9 | ancestors5)(dag.ancestors(List(node9,node5)))
   }
   
   test("LACs") {
      val dag = createDAG
      val node2  = dag("GO:0000002")
      val node4  = dag("GO:0000004")
      val node5  = dag("GO:0000005")
      val node9  = dag("GO:0000009")
      val node10 = dag("GO:0000010")
      
      expect(Set(node2))(dag.LCAs(List(node4),List(node5)))
      expect(Set(node4,node5))(dag.LCAs(List(node9),List(node10)))
      expect(Set(node4,node5))(dag.LCAs(List(node9,node5),List(node10)))
      expect(Set(node4,node5))(dag.LCAs(List(node9,node5),List(node4,node10)))
   }   
} 


object DAGTest extends App {
  (new DAGTest).execute()
}





