package au.edu.imb.ppii.predictor

import au.edu.imb.ppii.go._
import au.edu.imb.ppii.utils.Annotations
import au.edu.imb.ppii.go.Ontology._

import org.scalatest.FunSuite



class NodeTest extends FunSuite  { 

   def createNode1 = new Node(new Term( new Annotations(List(
         GO_ID    -> "GO:0000001",
         GO_NAME  ->"mitochondrion inheritance",
         GO_IS_A  ->"GO:0048308",
         GO_IS_A  -> "GO:0048311",
         GO_NAMESPACE -> "biological_process"
  	 ))))    
   
   def createNode2 = new Node(new Term( new Annotations(List(
         GO_ID  -> "GO:0000003"
     ))))    

   def createNode3 = new Node(new Term( new Annotations(List(
         GO_ID -> "GO:0000003",
         GO_NAME ->"reproduction"
     ))))    
   
      
   test("equals") { 
     val node1 = createNode1
     val node2 = createNode2
     val node3 = createNode3
     
     assert(node1 != node3)
     assert(node2 == node2)
     assert(node2 == node3)
   }
   
   test("add link") { 
     val node1 = createNode1
     val node2 = createNode2
     val node3 = createNode3
     
     val link1 = node1 -> node2
     val link2 = node1 -> node3
     expect(0)(node1.children.length)
     expect(2)(node1.parents.length)
     expect(0)(node2.parents.length)
     expect(1)(node2.children.length)
     expect(0)(node3.parents.length)
     expect(1)(node3.children.length)
   }
} 


object NodeTest extends App {
  (new NodeTest).execute()
}





