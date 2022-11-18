package com.victormeyer.scalaml.algorithm

import breeze.linalg.DenseMatrix
import breeze.numerics.exp

/** Gather all activation methods.
 *
 */
object Activations {

  /** Compute sigmoid function on a matrix
   *
   * @param x Matrix to compute
   * @return Matrix of sigmoid
   */
  def sigmoid(x: DenseMatrix[Double]): DenseMatrix[Double] ={
    (exp(x *:* -1.0) + 1.0).map(value => 1.0 / value).t
  }

}
