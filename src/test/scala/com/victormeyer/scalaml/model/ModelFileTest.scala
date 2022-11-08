package com.victormeyer.scalaml.model

import com.victormeyer.scalaml.model.algorithm.SimpleLinearRegressionModel
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.io.{BufferedSource, Source}

class ModelFileTest extends AnyFlatSpec with PrivateMethodTester {

  "ModelFileTest.saveModel" should "create file with json model" in {

    // given

    val model: SimpleLinearRegressionModel = new SimpleLinearRegressionModel(0.8172, 71.91)

    // when

    ModelFile.saveModel("src/test/resources/model", "SimpleLinearRegression", "MySimpleModel", model)
    val outputFilePath: String = new File("src/test/resources/model").listFiles.filter(_.getName.contains("MySimpleModel"))(0).getAbsolutePath
    val outputFile: BufferedSource = Source.fromFile(outputFilePath)
    val fileContent: String = outputFile.mkString
    outputFile.close()

    // then

    assert(fileContent.contains("""{
                                  |  "modelType": "SimpleLinearRegression",
                                  |  "modelName": "MySimpleModel",""".stripMargin))
    assert(fileContent.contains(""""dateMaj""""))
    assert(fileContent.contains("""  "model": {
                                  |    "alpha": 0.8172,
                                  |    "beta": 71.91
                                  |  }
                                  |}""".stripMargin))

    // after

    new File(outputFilePath).delete()

  }

  "ModelFileTest.loadModel" should "get model from file" in {

    // given

    val modelToSave: SimpleLinearRegressionModel = new SimpleLinearRegressionModel(0.8172, 71.91)
    ModelFile.saveModel("src/test/resources/model", "SimpleLinearRegression", "MySimpleModelToLoad", modelToSave)
    val outputFilePath: String = new File("src/test/resources/model").listFiles.filter(_.getName.contains("MySimpleModelToLoad"))(0).getAbsolutePath

    // when

    val modelToLoad: SimpleLinearRegressionModel = ModelFile.loadModel(outputFilePath).asInstanceOf[SimpleLinearRegressionModel]

    // then

    assert(modelToLoad.alpha ==  0.8172)
    assert(modelToLoad.beta == 71.91)

    // after

    new File(outputFilePath).delete()

  }

  "ModelFileTest.getModelType" should "return model type from json" in {

    // given

    val getModelType: PrivateMethod[String] = PrivateMethod[String]('getModelType)
    val modelJson: String = """{
                              |  "modelType": "SimpleLinearRegression",
                              |  "modelName": "name1",
                              |  "dateMaj": 1667848005963,
                              |  "model": {
                              |    "alpha": 1.0,
                              |    "beta": 2.0
                              |  }
                              |}""".stripMargin

    // when

    val modelType: String = ModelFile invokePrivate getModelType(modelJson)

    // then

    assert(modelType == "SimpleLinearRegression")

  }

  "ModelFileTest.getModelJson" should "return model dict from json" in {

    // given

    val getModelJson: PrivateMethod[String] = PrivateMethod[String]('getModelJson)
    val modelJson: String = """{
                              |  "modelType": "SimpleLinearRegression",
                              |  "modelName": "name1",
                              |  "dateMaj": 1667848005963,
                              |  "model": {
                              |   "alpha":1.819,
                              |   "beta":2.1920,
                              |   "array":[
                              |      1,
                              |      2,
                              |      3
                              |   ],
                              |   "nestedObject":{
                              |      "alpha":1.819,
                              |      "beta":2.1920,
                              |      "array":[
                              |         1,
                              |         2,
                              |         3
                              |      ]
                              |   }
                              |}
                              |}""".stripMargin

    // when

    val modelType: String = ModelFile invokePrivate getModelJson(modelJson)

    // then

    assert(modelType == """{
                          |   "alpha":1.819,
                          |   "beta":2.1920,
                          |   "array":[
                          |      1,
                          |      2,
                          |      3
                          |   ],
                          |   "nestedObject":{
                          |      "alpha":1.819,
                          |      "beta":2.1920,
                          |      "array":[
                          |         1,
                          |         2,
                          |         3
                          |      ]
                          |   }
                          |}""".stripMargin)

  }

}
