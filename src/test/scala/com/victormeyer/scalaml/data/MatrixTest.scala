package com.victormeyer.scalaml.data

import org.scalatest.flatspec.AnyFlatSpec

import java.io.ByteArrayOutputStream

class MatrixTest extends AnyFlatSpec {

  /**
   * read
   */

  "Matrix" should "initialize matrix with default value" in {

    // given

    val matrixA: Matrix[Int] = new Matrix(2, 3, 5)
    val matrixB: Matrix[Int] = new Matrix(0, 0, 5)
    val matrixC: Matrix[Int] = new Matrix(2, 2)

    // when

    val vectorA: Vector[Vector[Int]] = matrixA.get
    val vectorB: Vector[Vector[Int]] = matrixB.get
    val vectorC: Vector[Vector[Int]] = matrixC.get

    // then

    assert(vectorA.equals(Vector(Vector(5, 5, 5), Vector(5, 5, 5))))
    assert(vectorB.equals(Vector()))
    assert(vectorC.equals(Vector(Vector(null, null), Vector(null, null))))

  }

  "Matrix.shape" should "return dimensions of matrix" in {

    // given

    val matrixA: Matrix[Int] = new Matrix(2, 3, 5)
    val matrixB: Matrix[Int] = new Matrix(5416, 9817, 50)
    val matrixC: Matrix[Int] = new Matrix(712, 3817)
    val matrixD: Matrix[Int] = new Matrix(0, 0, 0)

    // when

    val shapeA: (Int, Int) = matrixA.shape
    val shapeB: (Int, Int) = matrixB.shape
    val shapeC: (Int, Int) = matrixC.shape
    val shapeD: (Int, Int) = matrixD.shape

    // then

    assert(shapeA == (2, 3))
    assert(shapeB == (5416, 9817))
    assert(shapeC == (712, 3817))
    assert(shapeD == (0, 0))

  }

  "Matrix.show" should "print string matrix" in {

    // given

    val stream: ByteArrayOutputStream = new java.io.ByteArrayOutputStream()
    val matrix: Matrix[Int] = new Matrix(3, 5, 5)
    matrix.set(Vector(Vector(1, 2, 3, 4, 5), Vector(6, 7, 8, 9, 10), Vector(11, 12, 13, 14, 15)))

    // when

    Console.withOut(stream) {
      matrix.show()
    }

    // then

    assert(stream.toString == "[[1, 2, 3, 4, 5]\n[6, 7, 8, 9, 10]\n[11, 12, 13, 14, 15]]\n")

  }

  /**
   * update
   */

  "Matrix.set" should "change matrix" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 5)

    // when

    matrix.set(Vector(Vector(1, 2, 3, 4, 5), Vector(6, 7, 8, 9, 10), Vector(11, 12, 13, 14, 15)))

    // then

    assert(matrix.get.equals(Vector(Vector(1, 2, 3, 4, 5), Vector(6, 7, 8, 9, 10), Vector(11, 12, 13, 14, 15))))

  }

  "Matrix.setValue" should "raise exception with invalid index" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 5, 5)
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      matrix.setValue(3, 2, 15)
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      matrix.setValue(2, 8, 15)
    } catch {
      case e: IllegalArgumentException => errorMessageB = e.getMessage
    }

    // then

    assert(errorMessageA == "Invalid row index: 3 >= 3")
    assert(errorMessageB == "Invalid column index: 8 >= 5")

  }

  it should "replace value in matrix" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 3, 5)

    // when

    matrix.setValue(1, 2, 15)

    // then

    assert(matrix.get.equals(Vector(Vector(5, 5, 5), Vector(5, 5, 15), Vector(5, 5, 5))))

  }

  "Matrix.setRow" should "raise exception with invalid index" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 2, 5)
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      matrix.setRow(3, Vector(1, 2))
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      matrix.setRow(3, Vector(1))
    } catch {
      case e: IllegalArgumentException => errorMessageB = e.getMessage
    }

    // then

    assert(errorMessageA == "Invalid row index: 3 >= 3")
    assert(errorMessageB == "Invalid row size: 1 != 2")

  }

  it should "replace row in matrix" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 3, 5)

    // when

    matrix.setRow(1, Vector(2, 21, 32))

    // then

    assert(matrix.get.equals(Vector(Vector(5, 5, 5), Vector(2, 21, 32), Vector(5, 5, 5))))

  }

  "Matrix.setColumn" should "raise exception with invalid index" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 2, 5)
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      matrix.setColumn(3, Vector(1, 2, 3))
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      matrix.setColumn(3, Vector(1))
    } catch {
      case e: IllegalArgumentException => errorMessageB = e.getMessage
    }

    // then

    assert(errorMessageA == "Invalid column index: 3 >= 2")
    assert(errorMessageB == "Invalid column size: 1 != 3")

  }

  it should "replace column in matrix" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 3, 5)

    // when

    matrix.setColumn(1, Vector(2, 21, 32))

    // then

    assert(matrix.get.equals(Vector(Vector(5, 2, 5), Vector(5, 21, 5), Vector(5, 32, 5))))

  }

  /**
   * delete
   */

  "Matrix.dropRow" should "raise exception with invalid index" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 2, 5)
    var errorMessage: String = ""

    // when

    try {
      matrix.dropRow(5)
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid row index: 5 >= 3")

  }

  it should "drop row in matrix" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 3, 5)
    matrix.set(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7)))

    // when

    matrix.dropRow(1)

    // then

    assert(matrix.get.equals(Vector(Vector(1, 2, 3), Vector(5, 6, 7))))

  }

  "Matrix.dropColumn" should "raise exception with invalid index" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 2, 5)
    var errorMessage: String = ""

    // when

    try {
      matrix.dropColumn(5)
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid column index: 5 >= 2")

  }

  it should "drop column in matrix" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 3, 5)
    matrix.set(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7)))

    // when

    matrix.dropColumn(1)

    // then

    assert(matrix.get.equals(Vector(Vector(1, 3), Vector(2, 4), Vector(5, 7))))

  }

  /**
   * override
   */

  "Matrix.equals" should "return true when matrices are equals" in {

    // given

    val matrixA: Matrix[Int] = new Matrix(5416, 9817, 50)
    val matrixB: Matrix[Int] = new Matrix(5416, 9817, 50)
    val matrixC: Matrix[Matrix[Double]] = new Matrix(761, 547, new Matrix(3, 7, 0.0))
    val matrixD: Matrix[Matrix[Double]] = new Matrix(761, 547, new Matrix(3, 7, 0.0))
    val matrixE: Matrix[Int] = new Matrix(0, 0, 0)
    val matrixF: Matrix[Int] = new Matrix(0, 0, 0)
    val matrixG: Matrix[Int] = new Matrix(3, 3)
    val matrixH: Matrix[Int] = new Matrix(3, 3)

    // when

    val equalA: Boolean = matrixA.equals(matrixB)
    val equalB: Boolean = matrixC.equals(matrixD)
    val equalC: Boolean = matrixE.equals(matrixF)
    val equalD: Boolean = matrixG.equals(matrixH)

    // then

    assert(equalA)
    assert(equalB)
    assert(equalC)
    assert(equalD)

  }

  it should "return false when matrices are not equals" in {

    // given

    val matrixA: Matrix[Int] = new Matrix(5416, 9817, 50)
    val matrixB: Matrix[Int] = new Matrix(5416, 9817)
    val matrixC: Matrix[Int] = new Matrix(546, 9817, 50)

    // when

    val equalA: Boolean = matrixA.equals(matrixB)
    val equalB: Boolean = matrixA.equals(matrixC)
    val equalC: Boolean = matrixA.equals(5)

    // then

    assert(!equalA)
    assert(!equalB)
    assert(!equalC)

  }

  "Matrix.toString" should "format matrix in string" in {

    // given

    val matrixA: Matrix[Int] = new Matrix(5416, 9817, 50)
    val matrixB: Matrix[Int] = new Matrix(3, 5, 5)
    matrixB.set(Vector(Vector(1, 2, 3, 4, 5), Vector(6, 7, 8, 9, 10), Vector(11, 12, 13, 14, 15)))
    val matrixC: Matrix[Int] = new Matrix(0, 0, 0)
    val matrixD: Matrix[Float] = new Matrix(2, 2)

    // when

    val matrixStrA: String = matrixA.toString
    val matrixStrB: String = matrixB.toString
    val matrixStrC: String = matrixC.toString
    val matrixStrD: String = matrixD.toString

    // then

    assert(matrixStrA.split("\n").length == 11)
    assert(matrixStrA.split("\n")(0).length == 105)
    assert(matrixStrB == "[[1, 2, 3, 4, 5]\n[6, 7, 8, 9, 10]\n[11, 12, 13, 14, 15]]")
    assert(matrixStrC == "[]")
    assert(matrixStrD == "[[null, null]\n[null, null]]")

  }

  "Matrix.clone" should "duplicate object" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 5, 5)

    // when

    val cloneMatrix: Matrix[Int] = matrix.clone

    // then

    assert(cloneMatrix.equals(matrix))

  }

  /**
   * Ops
   */

  "Matrix.vectorToMatrix" should "return matrix with vector" in {

    // given

    val vector: Vector[Vector[String]] = Vector(Vector("apple", "banana"), Vector("butter", "sugar"))
    val targetMatrix: Matrix[String] = new Matrix(0, 0)
    targetMatrix.set(vector)

    // when

    val matrix: Matrix[String] = Matrix.vectorToMatrix(vector)

    // then

    assert(matrix.equals(targetMatrix))

  }

  "Matrix.reshape" should "raise exception when dimensions are wrong" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 4, 5)
    var errorMessage: String = ""

    // when

    try {
      Matrix.reshape(matrix, 5, 6)
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid dimensions: 3 * 4 != 5 * 6")

  }

  it should "reshape matrix in new dimensions" in {

    // given

    val matrixA: Matrix[Int] = new Matrix(3, 4)
    matrixA.set(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val newMatrixA: Matrix[Int] = Matrix.reshape(matrixA, 2, 6)
    val newMatrixB: Matrix[Int] = Matrix.reshape(matrixA, 6, 2)
    val newMatrixC: Matrix[Int] = Matrix.reshape(matrixA, 1, 12)

    // then

    assert(newMatrixA.get.equals(Vector(Vector(1, 2, 3, 4, 5, 6), Vector(7, 8, 9, 10, 11, 12))))
    assert(newMatrixB.get.equals(Vector(Vector(1, 2), Vector(3, 4), Vector(5, 6), Vector(7, 8), Vector(9, 10), Vector(11, 12))))
    assert(newMatrixC.get.equals(Vector(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))))

  }

  "Matrix.select" should "raise exception when invalid indexes" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 4, 0)
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      Matrix.select(matrix, Seq(10, 12, 1), Seq())
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      Matrix.select(matrix, Seq(), Seq(15, 2, 5))
    } catch {
      case e: IllegalArgumentException => errorMessageB = e.getMessage
    }

    // then

    assert(errorMessageA == "Invalid row indexes: [10, 12, 1] >= 3")
    assert(errorMessageB == "Invalid column indexes: [15, 2, 5] >= 4")

  }

  it should "return selected rows and columns" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 4, 0)
    matrix.set(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val selectMatrixA: Matrix[Int] = Matrix.select(matrix, rows=Seq(0, 2))
    val selectMatrixB: Matrix[Int] = Matrix.select(matrix, cols=Seq(1, 3))
    val selectMatrixC: Matrix[Int] = Matrix.select(matrix, rows=Seq(0, 2), cols=Seq(1, 3))

    // then

    assert(selectMatrixA.get.equals(Vector(Vector(1, 2, 3, 4), Vector(9, 10, 11, 12))))
    assert(selectMatrixB.get.equals(Vector(Vector(2, 4), Vector(6, 8), Vector(10, 12))))
    assert(selectMatrixC.get.equals(Vector(Vector(2, 4), Vector(10, 12))))

  }

  "Matrix.sumRow" should "return matrix of row sum" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 4)
    matrix.set(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val sumMatrix: Matrix[Int] = Matrix.sumRow(matrix)

    // then

    assert(sumMatrix.get.equals(Vector(Vector(10), Vector(26), Vector(42))))

  }

  "Matrix.sumColumn" should "return matrix of row sum" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 4)
    matrix.set(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val sumMatrix: Matrix[Int] = Matrix.sumColumn(matrix)

    // then

    assert(sumMatrix.get.equals(Vector(Vector(15, 18, 21, 24))))

  }

}
