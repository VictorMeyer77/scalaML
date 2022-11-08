package com.victormeyer.scalaml.model.algorithm

import com.victormeyer.scalaml.model.Model

/** Represents Simple Linear Regression model
 *
 * @param alpha Gradient of the line
 * @param beta Vertical intercept
 */
class SimpleLinearRegressionModel(alpha: Double, beta: Double) extends Model {

  /** Create Json with alpha and beta
   *
   * @return String in json format
   */
  override def toString: String = {
    s"""{"alpha": $alpha, "beta": $beta}"""
  }

}
