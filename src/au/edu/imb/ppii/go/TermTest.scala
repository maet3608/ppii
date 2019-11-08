package au.edu.imb.ppii.go

import au.edu.imb.ppii.utils.Annotations
import au.edu.imb.ppii.go.Ontology._

import org.scalatest.FunSuite


class TermTest extends FunSuite  { 

   def createTerm1 = new Term( new Annotations(List(
		  	 GO_ID    -> "GO:0000001",
		  	 GO_NAME  ->"mitochondrion inheritance",
			   GO_IS_A  ->"GO:0048308",
			   GO_IS_A  -> "GO:0048311",
			   GO_NAMESPACE -> "biological_process"
  	 )))    
   
   def createTerm2 = new Term( new Annotations(List(
         GO_ID  -> "GO:0000001"
     )))    

   def createTerm3 = new Term( new Annotations(List(
         GO_ID -> "GO:0000003",
         GO_NAME ->"reproduction"
     )))    
   
      
   test("apply") { 
     val term = createTerm1
     expect("GO:0000001")(term(GO_ID))
     expect("biological_process")(term(GO_NAMESPACE))
   }
   
   test("get") { 
     val term = createTerm1
     expect("GO:0000001")(term.get(GO_ID))
     expect("biological_process")(term.get(GO_NAMESPACE))
     expect("")(term.get("nope"))
   }   
   
   test("values") { 
     val term = createTerm1
     assert(term.values(GO_IS_A).contains("GO:0048308"))
     assert(term.values(GO_IS_A).contains("GO:0048311"))
   }   
   
   test("contains") { 
     val term = createTerm1
     assert(term.contains(GO_ID))
     assert(term.contains(GO_IS_A))
     assert(term.contains(GO_NAMESPACE))
   } 
   
   test("id") { 
     val term = createTerm1
     expect("GO:0000001")(term.id)
   } 
   
   test("name") { 
     val term1 = createTerm1
     val term2 = createTerm2
     expect("mitochondrion inheritance")(term1.name)
     expect("")(term2.name)
   }    
   
   test("equals") { 
     val term1 = createTerm1
     val term2 = createTerm2
     val term3 = createTerm3
     assert(term1 == term1)
     assert(term1 == term2)
     assert(term1 != term3)
   }    
 
   test("relation") {
     val term1 = createTerm1
     val term2 = createTerm2
     val term3 = createTerm3
     term3.addIsA(term1)
     term3.addPartOf(term2)
     
     assert(term3 isA term1)
     assert(term3 partOf term2)
     
     expect(0)(term1.parents.length)
     expect(0)(term2.parents.length)
     expect(2)(term3.parents.length)
     
     expect(1)(term1.children.length)
     expect(1)(term2.children.length)
     expect(0)(term3.children.length)
     
     assert(term3.parents.contains(term1))
     assert(term3.parents.contains(term2))
     assert(term1.children.contains(term3))
     assert(term2.children.contains(term3))
   }    
   
} 


object TermTest {
  def main(args : Array[String]):Unit = (new TermTest).execute()
}





