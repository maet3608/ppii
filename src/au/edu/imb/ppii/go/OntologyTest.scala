package au.edu.imb.ppii.go

import org.scalatest.FunSuite

/**
 * Tests a GO DAG of the following structure:
 * 0                   1
 *                  /      \
 * 1            2             3 
 *            /   \       /  |  \
 * 2         4     5      6  7  8
 *           |  X  | 
 * 3         9     10
 */
class OntologyTest extends FunSuite  { 

   def createOntology = new Ontology(OboParser("test/go_test2.obo"))
   
   test("contains") {
     val ontology = createOntology
     expect(10)(ontology.size)
     assert(ontology.contains("GO:0000001"))
     assert(ontology.contains("GO:0000002"))
     assert(ontology.contains("GO:0000003"))
     assert(ontology.contains("GO:0000004"))
     assert(ontology.contains("GO:0000005"))
     assert(ontology.contains("GO:0000006"))
     assert(ontology.contains("GO:0000007"))
     assert(ontology.contains("GO:0000008"))
     assert(ontology.contains("GO:0000009"))
     assert(ontology.contains("GO:0000010"))
    }
   
   test("relations") {
     val ontology = createOntology
     expect(11)(ontology.relations.size)
     expect(0)(ontology("GO:0000001").parents.length)
     expect(2)(ontology("GO:0000001").children.length)
     expect(1)(ontology("GO:0000002").parents.length)
     expect(2)(ontology("GO:0000002").children.length)
     expect(1)(ontology("GO:0000003").parents.length)
     expect(3)(ontology("GO:0000003").children.length)
     expect(1)(ontology("GO:0000004").parents.length)
     expect(2)(ontology("GO:0000004").children.length)
     expect(1)(ontology("GO:0000005").parents.length)
     expect(2)(ontology("GO:0000005").children.length)
     expect(1)(ontology("GO:0000006").parents.length)
     expect(0)(ontology("GO:0000006").children.length)
     expect(1)(ontology("GO:0000007").parents.length)
     expect(0)(ontology("GO:0000007").children.length)
     expect(1)(ontology("GO:0000008").parents.length)
     expect(0)(ontology("GO:0000008").children.length)
     expect(2)(ontology("GO:0000009").parents.length)
     expect(0)(ontology("GO:0000009").children.length)
     expect(2)(ontology("GO:0000010").parents.length)
     expect(0)(ontology("GO:0000010").children.length)
   } 
   
   test("ancestors") {
      val ontology = createOntology
      val term9 = ontology("GO:0000009")
      val term5 = ontology("GO:0000005")
      val ancestors9 = Set("GO:0000005", "GO:0000002", "GO:0000004", 
          "GO:0000009", "GO:0000001").map(ontology(_))
      val ancestors5 = Set("GO:0000005", "GO:0000002", 
          "GO:0000001").map(ontology(_))
    
      expect(ancestors5)(ontology.ancestors(term5))
      expect(ancestors9)(ontology.ancestors(term9))
   }
   
   test("levels") {
     val ontology = createOntology
     expect(0)(ontology("GO:0000001").level)
     expect(1)(ontology("GO:0000002").level)
     expect(1)(ontology("GO:0000003").level)
     expect(2)(ontology("GO:0000004").level)
     expect(2)(ontology("GO:0000005").level)
     expect(2)(ontology("GO:0000006").level)
     expect(2)(ontology("GO:0000007").level)
     expect(2)(ontology("GO:0000008").level)
     expect(3)(ontology("GO:0000009").level)
     expect(3)(ontology("GO:0000010").level)
   }
   
   test("isA") {
     val ontology = createOntology
     assert(ontology("GO:0000002") isA ontology("GO:0000001"))
     assert(ontology("GO:0000003") isA ontology("GO:0000001"))
     assert(ontology("GO:0000004") isA ontology("GO:0000002"))
     assert(ontology("GO:0000005") isA ontology("GO:0000002"))
     assert(ontology("GO:0000006") isA ontology("GO:0000003"))
     assert(ontology("GO:0000007") isA ontology("GO:0000003"))
     assert(ontology("GO:0000008") isA ontology("GO:0000003"))
     assert(ontology("GO:0000009") isA ontology("GO:0000004"))
     assert(ontology("GO:0000009") isA ontology("GO:0000005"))
     assert(ontology("GO:0000010") isA ontology("GO:0000005"))
   }
   
} 


object OntologyTest {
  def main(args : Array[String]):Unit = (new OntologyTest).execute()
}





