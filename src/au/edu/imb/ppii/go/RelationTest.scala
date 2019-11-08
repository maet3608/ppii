package au.edu.imb.ppii.go

import au.edu.imb.ppii.utils.Annotations
import au.edu.imb.ppii.go.Ontology._

import org.scalatest.FunSuite


class RelationTest extends FunSuite  { 

   def createTerm1 = new Term( new Annotations(List(
         GO_ID    -> "GO:0000001",
         GO_NAME  ->"mitochondrion inheritance",
         GO_NAMESPACE -> "biological_process"
     )))    
   
   def createTerm2 = new Term( new Annotations(List(
         GO_ID  -> "GO:0000001",
         GO_NAME ->"mitochondrion inheritance"
     )))    

   def createTerm3 = new Term( new Annotations(List(
         GO_ID -> "GO:0000003",
         GO_NAME ->"reproduction"
     )))   
   
      
   test("equals") { 
     val term1 = createTerm1
     val term2 = createTerm2
     val term3 = createTerm3
     
     val rel1 = new Relation(term1, term3, true)
     val rel2 = new Relation(term2, term3, false)
     val rel3 = new Relation(term2, term1, true)
     
     assert(rel1 == rel1)
     assert(rel1 == rel2)
     assert(rel2 != rel3)
     assert(rel1 != rel3)
  }
   
} 


object RelationTest {
  def main(args : Array[String]):Unit = (new RelationTest).execute()
}





