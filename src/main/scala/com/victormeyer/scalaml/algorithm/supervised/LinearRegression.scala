package com.victormeyer.scalaml.algorithm.supervised

import breeze.linalg.{DenseMatrix, inv}
import com.victormeyer.scalaml.model.algorithm.LinearRegressionModel

/** Regression algorithm.
 * Process a linear regression on simple or multidimensional dataset.
 */
class LinearRegression(xTrain: DenseMatrix[Double] = null, yTrain: DenseMatrix[Double] = null) extends SupervisedLearning {

  /** Apply a linear regression based on te least square error method
   */
  override def fit(): Unit ={
    val alpha: DenseMatrix[Double] = (inv(xTrain.t * xTrain) * xTrain.t) * yTrain
    model = new LinearRegressionModel(alpha.toArray)
  }

  /** Apply model on matrix
   *
   * @param x Matrix to predict
   * @return Matrix of predictions
   */
  override def predict(x: DenseMatrix[Double]): DenseMatrix[Double] ={
    x * DenseMatrix(model.asInstanceOf[LinearRegressionModel].alpha).t
  }

}
