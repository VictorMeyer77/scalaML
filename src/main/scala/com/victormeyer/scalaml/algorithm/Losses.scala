package com.victormeyer.scalaml.algorithm

import breeze.linalg.DenseMatrix
import breeze.numerics.log
import breeze.stats.mean

/** Gather all losses methods.
 *
 */
object Losses {

  /** Compute binary cross entropy
   *
   * @param expected Matrix of expected result
   * @param predictions Matrix of actual result
   * @return Double loss
   */
  def binaryCrossEntropy(expected: DenseMatrix[Double], predictions: DenseMatrix[Double]): Double ={
    val yZeroLoss: DenseMatrix[Double] = expected *:* log(predictions + 1e-9)
    val yOneLoss: DenseMatrix[Double] = (1.0 - expected) *:* log(1.0 - predictions + 1e-9)
    mean(yZeroLoss + yOneLoss) * -1.0
  }

}
