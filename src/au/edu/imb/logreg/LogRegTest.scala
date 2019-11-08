package au.edu.imb.logreg

import org.scalatest.FunSuite
import cern.colt.matrix.{DoubleMatrix1D,DoubleMatrix2D,DoubleFactory1D,DoubleFactory2D}
import java.lang.Math.{abs}

class LogRegTest extends FunSuite  { 
  val X = DoubleFactory2D.dense.make(
        Array(Array(4.0, 10.0), 
              Array(5.0,  2.0),
              Array(0.0, -5.0),
              Array(1.0, -6.0),
              Array(1.0,  3.0)))
  val w = DoubleFactory1D.dense.ascending(2)
  val g = DoubleFactory1D.dense.make(2)
  val y = DoubleFactory1D.dense.make(Array(1.0, 1.0, 0.0, 0.0, 1.0))

  
  def compare(expect:Array[Double], got:DoubleMatrix1D):Unit = {
    for( i <- 0 until expect.size)
      if(abs(expect(i)-got.get(i))>1e-3)
        fail("EXPECTED:"+expect.mkString("[", " ", "]")+"\n  GOT:"+got)
  }

  def compare(expect:Array[Array[Double]], got:DoubleMatrix2D):Unit = {
    for( r <- 0 until got.rows)
      for( c <- 0 until got.columns)
        if(abs(expect(r)(c)-got.get(r,c))>1e-3)
          fail("EXPECTED:"+
              expect.map(_.mkString("[", " ", "]")).mkString("[", "\n", "]")+
              "\n  GOT:"+got)
  }
  
  
  test("pi") {
     compare(Array(1.000000e+00,9.998766e-01,4.539787e-05,1.670142e-05,9.990889e-01),
             LogReg.calcPi(X,w))
  }
 
  test("XW2") {
    val pi = LogReg.calcPi(X,w)
    val XW2 = LogReg.calcXW2(X,pi)
    compare(Array(
             Array(2.457685e-05,0.05553813,0.00000000,0.004086703,0.03016987),
             Array(6.144213e-05,0.02221525,-0.03368821,-0.024520219,0.09050962)
            ),
            XW2)
  }  
  
  test("Fisher") {
    val pi = LogReg.calcPi(X,w)
    val XW2 = LogReg.calcXW2(X,pi)
    val Fisher = LogReg.calcFisher(XW2)
    compare(Array(
             Array(0.004011407,0.003864252),
             Array(0.003864252,0.010421648)
            ),
            Fisher)
  }
  
  test("loglik") {
    val pi = LogReg.calcPi(X,w)
    val XW2 = LogReg.calcXW2(X,pi)
    val Fisher = LogReg.calcFisher(XW2)
    val loglik = LogReg.calcLoglik(y,pi,Fisher)
    assert(abs(loglik - (-5.263291)) < 1e-3)
  }   

  test("H") {
    val pi = LogReg.calcPi(X,w)
    val XW2 = LogReg.calcXW2(X,pi)
    val Fisher = LogReg.calcFisher(XW2)
    val H = LogReg.calcH(XW2,Fisher)
    compare(Array(
             Array(3.634889e-07,0.0001638939,-0.0001899192,-0.0001353898,0.0005312512),
             Array(1.638939e-04,0.9150337015,0.1573262521,0.1894767967,0.1307440006),
             Array(-1.899192e-04,0.1573262521,0.1694087543,0.1431024808,-0.3089976093),
             Array(-1.353898e-04,0.1894767967,0.1431024808,0.1250445333,-0.2302794025),
             Array(5.312512e-04,0.1307440006,-0.3089976093,-0.2302794025,0.7905126474)
            ),
            H)
  }  
  
  test("estimate") {
    val X = DoubleFactory2D.dense.make(
        Array(Array(1.0, 4.0, 10.0), 
              Array(1.0, 5.0,  2.0),
              Array(1.0, 0.0, -5.0),
              Array(1.0, 1.0, -6.0),
              Array(1.0, 1.0,  3.0)))
    val (loglik, weights) = LogReg.estimate(X,y)
    compare(Array(-0.01704341,0.09593336,0.25779322), weights)
    assert(abs(loglik - 0.4694124) < 1e-3)
  }

  
} 


object LogRegTest extends App {
  (new LogRegTest).execute()
}





