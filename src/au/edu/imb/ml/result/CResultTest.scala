package au.edu.imb.ml.result

import scala.math.abs
import org.scalatest.FunSuite


/** Tests for classifier results */
class CResultTest extends FunSuite  {

  def expect(expected:Double,eps:Double)(given:Double) =
    assert(abs(expected-given) < eps)

  def createResult = {
    val outputs = List(
      COutput(0, 0),
      COutput(0, 1),
      COutput(0, 1),
      COutput(1, 0),
      COutput(1, 0),
      COutput(1, 0),
      COutput(1, 1),
      COutput(1, 1),
      COutput(1, 1),
      COutput(1, 1)
    )
    CResult(2,outputs)
  }


  test("create") {
    val result = createResult
    expect(2)(result.nClasses)
    expect(2)(result.cmatrix.length)
    expect(List("Class_1","Class_2"))(result.labels.toList)
    expect(1)(result(0,0))  // TP(0)
    expect(2)(result(0,1))  // FN(0)
    expect(3)(result(1,0))  // FP(0)
    expect(4)(result(1,1))  // TN(0)
  }

  test("colSum") {
    val result = createResult
    expect(4)(result.colSum(0))
    expect(6)(result.colSum(1))
  }

  test("rowSum") {
    val result = createResult
    expect(3)(result.rowSum(0))
    expect(7)(result.rowSum(1))
  }

  test("sum") {
    val result = createResult
    expect(10)(result.sum)
  }

  test("trace") {
    val result = createResult
    expect(5)(result.trace)
  }

  test("TP") {
    val result = createResult
    expect(1)(result.TP(0))
    expect(4)(result.TP(1))
  }

  test("TN") {
    val result = createResult
    expect(4)(result.TN(0))
    expect(1)(result.TN(1))
  }

  test("FN") {
    val result = createResult
    expect(2)(result.FN(0))
    expect(3)(result.FN(1))
  }

  test("FP") {
    val result = createResult
    expect(3)(result.FP(0))
    expect(2)(result.FP(1))
  }

  test("specificity") {
    val result = createResult
    expect(0.571, 0.01)(result.specificity(0))
  }

  test("sensitivity") {
    val result = createResult
    expect(0.333, 0.01)(result.sensitivity(0))
  }

  test("mcc") {
    val result = createResult
    expect(-0.089, 0.01)(result.mcc(0,1))
    expect(-0.089, 0.01)(result.mcc(0))
    expect(-0.089, 0.01)(result.mcc(1))
  }

  test("acc") {
    val result = createResult
    expect(0.1)(result.acc(0))
    expect(0.4)(result.acc(1))
    expect(0.5)(result.acc)
  }

  test("bacc") {
    val result = createResult
    expect(0.333, 0.01)(result.bacc(0))
    expect(0.571, 0.01)(result.bacc(1))
    expect(0.452, 0.01)(result.bacc)
  }

  test("error") {
    val result = createResult
    expect(0.5)(result.error)
  }

  test("auc0") {
    val outputs = List(
      COutput(1, 0, Array(0.1, 0.9)),
      COutput(0, 1, Array(0.1, 0.9)),
      COutput(1, 0, Array(0.3, 0.7)),
      COutput(0, 1, Array(0.3, 0.7))
    )
    val result = CResult(2,outputs)
    expect(0.50)(result.auc(1))
  }

  test("auc1") {
    val outputs = List(
      COutput(1, 0, Array(0.1, 0.9)),
      COutput(0, 1, Array(0.9, 0.1)),
      COutput(1, 0, Array(0.3, 0.7)),
      COutput(0, 1, Array(0.7, 0.3))
    )
    val result = CResult(2,outputs)
    expect(1.00)(result.auc(1))
  }

  test("auc2") {
    val outputs = List(
      COutput(1, 1, Array(0.1, 0.9)),
      COutput(1, 1, Array(0.2, 0.8)),
      COutput(1, 1, Array(0.3, 0.7)),
      COutput(1, 1, Array(0.4, 0.6)),
      COutput(0, 0, Array(0.5, 0.5)),
      COutput(0, 0, Array(0.6, 0.4)),
      COutput(1, 0, Array(0.7, 0.3)),
      COutput(0, 0, Array(0.8, 0.2)),
      COutput(0, 0, Array(0.9, 0.1))
    )
    val result = CResult(2,outputs)
    expect(0.90)(result.auc(1))
  }

} 


object CResultTest extends App {
  (new CResultTest).execute()
}





