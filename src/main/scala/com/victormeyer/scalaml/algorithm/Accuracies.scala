package com.victormeyer.scalaml.algorithm

import breeze.linalg.DenseMatrix

/** Gather methods which compute accuracy of model.
 *
 */
object Accuracies {

  /** Compute F1-Score of a model predictions
   *
   * @param expected Matrix of expected result
   * @param predictions Matrix of actual result
   * @return Decimal accuracy
   */
  def f1Score(expected: DenseMatrix[Double], predictions: DenseMatrix[Double]): Double ={
    if(expected.rows != predictions.rows){
      throw new IllegalArgumentException(s"expected and predictions shapes are not equal: ${expected.rows} != ${predictions.rows}")
    }
    var (tp, fp, fn): (Int, Int, Int) = (0, 0, 0)
    for(i <- 0 until expected.rows){
      if(expected(i, 0) == 1.0 && predictions(i, 0) == 1.0){
        tp += 1
      } else if(expected(i, 0) == 0.0 && predictions(i, 0) == 1.0) {
        fp += 1
      } else if(expected(i, 0) == 1.0 && predictions(i, 0) == 0.0) {
        fn += 1
      }
    }
    val precision: Double = tp.toDouble / (tp.toDouble + fp.toDouble)
    val recall: Double = tp.toDouble / (tp.toDouble + fn.toDouble)
    if(precision * recall < 1e-9){
      0.0
    } else {
      2.0 * precision * recall / (precision + recall)
    }
  }

}
