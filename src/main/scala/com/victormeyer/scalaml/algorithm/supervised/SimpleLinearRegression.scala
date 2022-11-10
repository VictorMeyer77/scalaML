package com.victormeyer.scalaml.algorithm.supervised

import com.victormeyer.scalaml.Logging
import com.victormeyer.scalaml.data.Matrix
import com.victormeyer.scalaml.model.algorithm.SimpleLinearRegressionModel

/** Regression Algorithm
 * Apply Simple Linear Regression based on least squares method
 *
 * @tparam T Type of matrix
 */
class SimpleLinearRegression[T] extends SupervisedLearning[T] with Logging {

  /** Apply least squares method to compute alpha and beta
   *
   * @param numeric Numeric instance
   */
  def fit(implicit numeric: Numeric[T]): Unit ={
    if(xTrain.shape._1 != 1 || yTrain.shape._1 != 1){
      throw new IllegalArgumentException(s"Invalid shapes: x or y hasn't only one row (xTrain: ${xTrain.shape._1}, yTrain: ${yTrain.shape._1})")
    }
    val covariance: Matrix[Double] = Matrix.covariance(Matrix.concat(xTrain, yTrain))
    val avgX: Matrix[Double] = Matrix.avg(xTrain)
    val avgY: Matrix[Double] = Matrix.avg(yTrain)
    val alpha: Double = covariance(0)(1) / covariance(0)(0)
    val beta: Double = avgY(0)(0) - alpha * avgX(0)(0)
    model = new SimpleLinearRegressionModel(alpha, beta)
    logger.info(s"Simple Linear Regression successfully ended. alpha = $alpha, beta = $beta")
  }

  /**
   *
   * @param xTest Variables to predict
   * @return Matrix of prediction
   */
  def predict(xTest: Matrix[T]): Matrix[Double] ={
    val model: SimpleLinearRegressionModel = this.model.asInstanceOf[SimpleLinearRegressionModel]
    (xTest.asInstanceOf[Matrix[Double]] * model.alpha) + model.beta
  }

}
