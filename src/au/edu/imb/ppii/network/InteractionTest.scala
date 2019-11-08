package au.edu.imb.ppii.network

import au.edu.imb.ppii.utils.Annotations
import au.edu.imb.ppii.network.Protein._
import org.scalatest.FunSuite


class InteractionTest extends FunSuite  { 

  def createProtein1 = new Protein( new Annotations(List(P_ID -> "P0000001" )))
  def createProtein2 = new Protein( new Annotations(List(P_ID -> "P0000001" )))
  def createProtein3 = new Protein( new Annotations(List(P_ID -> "P0000003" )))

  test("create") {
    val protein1 = createProtein1
    val protein2 = createProtein2
    expect(protein1)(Interaction(protein1,protein2).protein1)
    expect(protein2)(Interaction(protein1,protein2).protein2)
    expect(+1.0)(Interaction(protein1,protein2).weight)
    expect(+1.0)(Interaction(protein1,protein2,true).weight)
    expect(-1.0)(Interaction(protein1,protein2,false).weight)
  }

  test("equals") {
    val protein1 = createProtein1
    val protein2 = createProtein2
    val protein3 = createProtein3
    val interaction1 = Interaction(protein1,protein2)
    val interaction2 = Interaction(protein2,protein1)
    val interaction3 = Interaction(createProtein1,createProtein2)
    val interaction4 = Interaction(protein3,protein1)

    assert(interaction1 == interaction1)
    assert(interaction1 == interaction2)
    assert(interaction1 == interaction3)
    assert(interaction1 != interaction4)
  }

  test("hashcode") {
    val protein1 = createProtein1
    val protein3 = createProtein3
    val interaction1 = Interaction(protein1,protein3)
    val interaction2 = Interaction(protein3,protein1)
    val interactions = List(interaction1,interaction2)

    assert(interaction1.hashCode == interaction2.hashCode)
    expect(2)(interactions.size)
    expect(1)(interactions.toSet.size)
  }
 
  test("contains") {
    val protein1 = createProtein1
    val protein2 = createProtein2
    val protein3 = createProtein3
    val interaction =  Interaction(protein1,protein2)
     
    assert(interaction.contains(protein1))
    assert(interaction.contains(protein2))
    assert(!interaction.contains(protein3))
  }
} 


object InteractionTest extends App {
  (new InteractionTest).execute()
}





