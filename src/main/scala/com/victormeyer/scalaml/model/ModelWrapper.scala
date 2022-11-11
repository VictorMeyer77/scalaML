package com.victormeyer.scalaml.model

/** Represent model parameter which compose json model file.
 *
 * @param modelType Type of the model, which equal to class name in algorithm package
 * @param modelName Name of the model
 * @param dateMaj Timestamp of model saving
 * @param model Model instance
 */
case class ModelWrapper(
                          modelType: String,
                          modelName: String,
                          dateMaj: Long,
                          model: Model
                       )
