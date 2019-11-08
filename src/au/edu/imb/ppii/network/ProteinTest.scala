package au.edu.imb.ppii.network

import au.edu.imb.ppii.utils._
import au.edu.imb.ppii.network.Protein._
import org.scalatest.FunSuite


class ProteinTest extends FunSuite  { 

   def createProtein1 = new Protein( new Annotations(List(
		  	 P_ID   -> "P0000001",
		  	 P_NAME -> "protein 1",
			   P_GOIDS -> "GO:0048308",
			   P_GOIDS -> "GO:0048311"
  	 )))    
   
   def createProtein2 = new Protein( new Annotations(List(
         P_ID -> "P0000001"
     )))    

   def createProtein3 = new Protein( new Annotations(List(
         P_ID -> "P0000003"
     )))    
   
      
   test("apply") { 
     val protein = createProtein1
     expect("P0000001")(protein(P_ID))
     expect("protein 1")(protein(P_NAME))
   }
   
   test("values") { 
     val protein = createProtein1
     assert(protein.values(P_GOIDS).contains("GO:0048308"))
     assert(protein.values(P_GOIDS).contains("GO:0048311"))
   }   
   
   test("contains") { 
     val protein = createProtein1
     assert(protein.contains(P_ID))
     assert(protein.contains(P_GOIDS))
     assert(protein.contains(P_NAME))
   } 
   
   test("id") { 
     val protein = createProtein1
     expect("P0000001")(protein.id)
   }
   
   test("goids") { 
     val protein1 = createProtein1
     val protein2 = createProtein2
     assert(protein1.goids.contains("GO:0048308"))
     assert(protein1.goids.contains("GO:0048311"))
     expect(List())(protein2.goids)
   }   
   
   test("equals") { 
     val protein1 = createProtein1
     val protein2 = createProtein2
     val protein3 = createProtein3
     assert(protein1 == protein1)
     assert(protein1 == protein2)
     assert(protein1 != protein3)
     assert(createProtein1 == createProtein1)
   }    
 
   test("interaction") {
     val protein1 = createProtein1
     val protein2 = createProtein2
     val protein3 = createProtein3
     val interaction1 = protein1 interact protein2
     val interaction2 = protein1 interact protein3
     
     expect(2)(protein1.interactions.length)
     expect(1)(protein2.interactions.length)
     expect(1)(protein3.interactions.length)
          
     assert(protein1.interactions.contains(interaction1))
     assert(protein1.interactions.contains(interaction2))
     assert(protein2.interactions.contains(interaction1))
     assert(protein3.interactions.contains(interaction2))
    
     assert(protein1.isInteracting(protein2))
     assert(protein2.isInteracting(protein1))
     assert(protein1.isInteracting(protein3))
     assert(protein3.isInteracting(protein1))
     assert(protein2.isInteracting(protein1))
     assert(protein1.isInteracting(protein1))
   }    
   
} 


object ProteinTest extends App {
  (new ProteinTest).execute()
}





