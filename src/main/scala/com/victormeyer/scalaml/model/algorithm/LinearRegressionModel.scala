package com.victormeyer.scalaml.model.algorithm

import com.victormeyer.scalaml.model.Model

/** Represents linear regression model.
 *
 * @param alpha Array of weights
 */
class LinearRegressionModel(var alpha: Array[Double]) extends Model {

  /** Convert model to string
   *
   *  @return Json format string
   */
  override def toString: String = {
    s"""{\"alpha\": [${alpha.mkString(", ")}]}"""
  }

}
