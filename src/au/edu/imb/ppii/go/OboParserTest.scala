package au.edu.imb.ppii.go

import org.scalatest.FunSuite


class OboParserTest extends FunSuite  { 

   def createParser = OboParser("test/go_test1.obo")

   test("parser") {
     val parser = createParser
     val terms = parser.toSeq
     val term1 = terms(0)
     
     expect(5)(terms.length)
     assert(terms.exists(_.id=="GO:0000001")) 
     assert(terms.exists(_.id=="GO:0000002")) 
     assert(terms.exists(_.id=="GO:0000003")) 
     assert(terms.exists(_.id=="GO:0000005")) 
     assert(terms.exists(_.id=="GO:0000006")) 
     
     expect("mitochondrion inheritance")(term1.name)
     expect("biological_process")(term1("namespace"))
     assert(term1.values("is_a").contains("GO:0048308 ! organelle inheritance"))
     assert(term1.values("is_a").contains("GO:0048311 ! mitochondrion distribution"))
   }
   
} 


object OboParserTest {
  def main(args : Array[String]):Unit = (new OboParserTest).execute()
}





