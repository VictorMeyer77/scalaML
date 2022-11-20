package com.victormeyer.scalaml.model.algorithm

import com.victormeyer.scalaml.model.Model

/** Represent binary logistic regression model.
 *
 * @param weights Array of weights
 * @param bias Bias of model
 */
class BinaryLogisticRegressionModel(var weights: Array[Double], var bias: Double) extends Model {

  /** Convert model to string
   *
   *  @return Json format string
   */
  override def toString: String = {
    s"""{"weights": [${weights.mkString(", ")}], "bias": $bias}"""
  }

}
