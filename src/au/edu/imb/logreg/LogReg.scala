package au.edu.imb.logreg

import edu.au.uq.nlo._
import java.lang.Math.{exp,log,pow,abs}
import cern.colt.function.DoubleFunction
import cern.colt.matrix.{DoubleMatrix1D,DoubleMatrix2D,DoubleFactory1D,DoubleFactory2D}
import cern.colt.matrix.linalg.Algebra


/**
 * Logistic regression with Firth penalization to deal with perfect separation
 * (and to a degree with unbalanced data sets).
 * The implementation follows the R code of the logistf function. For testing
 * and comparison with the R implementation try:
 * library(logistf)
 * d = data.frame(x1=c(4,5,0,1,1), x2=c(10,2,-5,-6,3), y=c(1,1,0,0,1))
 * fit<-logistf(y ~ x1+x2, data=d)
 * summary(fit)
 *
 * References:
 * G. Heinze
 * A comparative investigation of methods for logistic regression with separated
 * or nearly separated data.
 * Statistics in Medicine 2006, 25:4216-4226
 *
 * D. Firth
 * Bias reduction of maximum likelihood estimates.
 * Biometrika 1993, 80:27-38.
 */
object LogReg {
  type Vector = DoubleMatrix1D
  type Matrix = DoubleMatrix2D

  private val alg = new Algebra

  val sigmoidF = new DoubleFunction() {def apply(x:Double) = 1/(1+exp(-x))}
  val piF = new DoubleFunction() {def apply(x:Double) = pow(x*(1-x),0.5)}

  def calcPi(X:Matrix, w:Vector) = alg.mult(X,w).assign(sigmoidF)

  def calcXW2(X:Matrix, pi:Vector) = {
    val D = DoubleFactory2D.dense.diagonal(pi.copy.assign(piF))
    alg.mult(alg.transpose(X), D)
  }

  def calcFisher(XW2:Matrix) = alg.mult(XW2, alg.transpose(XW2))

  def calcH(XW2:Matrix, Fisher:Matrix) = {
    val covs = alg.inverse(Fisher)
    alg.mult(alg.mult(alg.transpose(XW2),covs),XW2)
  }

  def calcGrad(X:Matrix, y:Vector, pi:Vector, H:Matrix, g:Vector) {
    val diagH = DoubleFactory2D.dense.diagonal(H)
    val tmp = DoubleFactory1D.dense.make(y.size)  // temp = y - pi + diagH*(0.5-pi)
    for(i <- 0 until tmp.size)
      tmp.setQuick(i, y.getQuick(i) - pi.getQuick(i) + diagH.getQuick(i)*(0.5-pi.getQuick(i)))
    alg.transpose(X).zMult(tmp,g) // X'*(y - pi + diagH*(0.5-pi))
  }

  def calcLoglik(y:Vector, pi:Vector, Fisher:Matrix) = {
    var sum = 0.0
    for(i <- 0 until y.size)
      sum += (if(y.get(i) == 1.0) log(pi.get(i)) else log(1.0-pi.get(i)))
    sum + 0.5*log(abs(alg.det(Fisher)))
  }

  def calcGradF(X:Matrix, y:Vector, w:Vector, g:Vector):Double = {
    val pi = calcPi(X,w)
    val XW2 = calcXW2(X,pi)
    val Fisher = calcFisher(XW2)
    val loglik = calcLoglik(y,pi,Fisher)
    val H = calcH(XW2,Fisher)
    calcGrad(X,y,pi,H, g)
    loglik
  }

  def negativeGrad(g:Vector, grad:Array[Double]) {
    for(i <- 0 until g.size ) grad(i) = -g.getQuick(i)
  }

  def estimate(X:Matrix, y:Vector, verbose:Boolean = false) = {
    val iprint  = if(verbose) Array(2,3) else Array(-1,1)
    val iflag   = Array(0)
    val m       = 5
    val eps     = 1e-5
    val xtol    = 1e-16
    val n       = X.columns
    val diag    = new Array[Double](n)
    val grad    = new Array[Double](n)
    val wgts    = new Array[Double](n)
    val w       = DoubleFactory1D.dense.make(n)
    val g       = DoubleFactory1D.dense.make(n)
    var loglik  = 0.0

    do {
      w.assign(wgts)
      loglik = calcGradF(X,y,w,g)
      negativeGrad(g,grad)
      LBFGS.lbfgs(n, m, wgts, -loglik, grad, false, diag, iprint, eps, xtol, iflag)
    } while(iflag(0) > 0)
    (loglik, w)
  }


  def main(args : Array[String]) {
    val X = DoubleFactory2D.dense.make(
        Array(Array(1.0, 4.0, 10.0),
              Array(1.0, 5.0,  2.0),
              Array(1.0, 0.0, -5.0),
              Array(1.0, 1.0, -6.0),
              Array(1.0, 1.0,  3.0)))
    val y = DoubleFactory1D.dense.make(Array(1.0, 1.0, 0.0, 0.0, 1.0))
    val (loglik, w) = LogReg.estimate(X,y,true)
    println(loglik)
    println(w)
  }

}