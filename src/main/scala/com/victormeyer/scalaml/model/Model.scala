package com.victormeyer.scalaml.model

/** Trait which must be implemented by all model classes to unify saving and loading.
 *
 */
trait Model {

  /** All model class must implement this method to write and load model file
   *
   * @return Json format string
   */
  def toString: String

}
