package au.edu.imb.ppii.utils

import org.scalatest.FunSuite


/** Tests for GO term annotations */
class AnnotationsTest extends FunSuite  { 

  def createAnnotations = {
    new Annotations(List(
      "id"->"GO:0000001",
		  "is_a"->"GO:0048308",
		  "is_a" -> "GO:0048311",
		  "namespace" -> "biological_process"
 	 ))
  }
   
  test("contains") {
    val annotations = createAnnotations
    assert(annotations.contains("id"))
    assert(annotations.contains("is_a"))
    assert(annotations.contains("namespace"))
    assert(!annotations.contains("nope"))
  }
   
  test("add") {
    val annotations = createAnnotations
    annotations += ("tag", "value1")
    expect("value1")(annotations("tag"))
    annotations += ("tag", "value2")
    annotations += ("tag", "value3")
    expect(List("value1","value2","value3"))(annotations.values("tag"))
  }

  test("set") {
    val annotations = createAnnotations
    annotations.set("tag", List("value1","value2","value3"))
    expect(List("value1","value2","value3"))(annotations.values("tag"))
  }

  test("remove") {
    val annotations = createAnnotations
    annotations.set("tag", List("value1","value2","value3"))
    annotations.remove("tagNotThere")
    annotations.remove("tag")
    assert(!annotations.contains("tag"))
  }  

  test("values") {
    val annotations = createAnnotations
    expect(List("GO:0000001"))(annotations.values("id"))
    intercept[java.util.NoSuchElementException] {
      annotations.values("blah")
    }
    expect(annotations.values("is_a").length)(2)
    assert(annotations.values("is_a").contains("GO:0048311"))
    assert(annotations.values("is_a").contains("GO:0048308"))
  }
   
  test("valuesOrElse") {
    val annotations = createAnnotations
    expect(List("GO:0000001"))(annotations.valuesOrElse("id"))
    expect(List())(annotations.valuesOrElse("blah"))
    expect(annotations.valuesOrElse("is_a").length)(2)
    assert(annotations.valuesOrElse("is_a").contains("GO:0048311"))
    assert(annotations.valuesOrElse("is_a").contains("GO:0048308"))
  }
   
  test("apply") {
    val annotations = createAnnotations
    expect("GO:0000001")(annotations("id"))
    intercept[java.util.NoSuchElementException] {
      annotations("nope")
    }
    expect("GO:0048308")(annotations("is_a"))  // returns only first
    expect("biological_process")(annotations("namespace"))
  }   
} 


object AnnotationsTest extends App {
  (new AnnotationsTest).execute()
}





