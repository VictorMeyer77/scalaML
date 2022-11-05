package com.victormeyer.scalaml.data

import org.scalatest.flatspec.AnyFlatSpec

import java.io.ByteArrayOutputStream

class MatrixTest extends AnyFlatSpec {

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
    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4, 5), Vector(6, 7, 8, 9, 10), Vector(11, 12, 13, 14, 15)))

    // when

    Console.withOut(stream) {
      matrix.show()
    }

    // then

    assert(stream.toString == "[[1, 2, 3, 4, 5]\n[6, 7, 8, 9, 10]\n[11, 12, 13, 14, 15]]\n")

  }

  "Matrix.replaceMatrix" should "change matrix" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 5)

    // when

    matrix.replaceMatrix(Vector(Vector(1, 2, 3, 4, 5), Vector(6, 7, 8, 9, 10), Vector(11, 12, 13, 14, 15)))

    // then

    assert(matrix.get.equals(Vector(Vector(1, 2, 3, 4, 5), Vector(6, 7, 8, 9, 10), Vector(11, 12, 13, 14, 15))))

  }

  it should "raise exception with invalid vectors" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 3)
    var errorMessage: String = ""

    // when

    try {
      matrix.replaceMatrix(Vector(Vector(1, 2), Vector(1)))
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid shape: different sizes of vectors found (2, 1)")

  }

  "Matrix.replaceCell" should "raise exception with invalid index" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 5, 5)
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      matrix.replaceCell(3, 2, 15)
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      matrix.replaceCell(2, 8, 15)
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

    matrix.replaceCell(1, 2, 15)

    // then

    assert(matrix.get.equals(Vector(Vector(5, 5, 5), Vector(5, 5, 15), Vector(5, 5, 5))))

  }

  "Matrix.replaceRow" should "raise exception with invalid index" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 2, 5)
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      matrix.replace(3, Vector(1, 2))
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      matrix.replace(3, Vector(1))
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

    matrix.replace(1, Vector(2, 21, 32))

    // then

    assert(matrix.get.equals(Vector(Vector(5, 5, 5), Vector(2, 21, 32), Vector(5, 5, 5))))

  }

  "Matrix.replaceColumn" should "raise exception with invalid index" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 2, 5)
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      matrix.replace(3, Vector(1, 2, 3), 'C')
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      matrix.replace(3, Vector(1), 'C')
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

    matrix.replace(1, Vector(2, 21, 32), 'C')

    // then

    assert(matrix.get.equals(Vector(Vector(5, 2, 5), Vector(5, 21, 5), Vector(5, 32, 5))))

  }

  "Matrix.replace" should "raise exception when axis is unknown" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    var errorMessage: String = ""

    // when

    try {
      matrix.replace(2, Vector(1, 2), 'U')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid axis 'U'. 'R' => row, 'C' => column")

  }

  "Matrix.deleteRows" should "raise exception with invalid indexes" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 2, 5)
    var errorMessage: String = ""

    // when

    try {
      matrix.delete(Seq(5, 8))
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid row indexes: (5, 8) >= 3")

  }

  it should "delete rows in matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7), Vector(5, 6, 7)))

    // when

    matrix.delete(Seq(1, 3))

    // then

    assert(matrix.get.equals(Vector(Vector(1, 2, 3), Vector(5, 6, 7))))

  }

  "Matrix.deleteColumns" should "raise exception with invalid index" in {

    // given

    val matrix: Matrix[Int] = new Matrix(3, 2, 5)
    var errorMessage: String = ""

    // when

    try {
      matrix.delete(Seq(5, 7, 1), 'C')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid column indexes: (5, 7) >= 2")

  }

  it should "delete columns in matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 1), Vector(2, 3, 4, 2), Vector(5, 6, 7, 5)))

    // when

    matrix.delete(Seq(1, 3), 'C')

    // then

    assert(matrix.get.equals(Vector(Vector(1, 3), Vector(2, 4), Vector(5, 7))))

  }

  "Matrix.delete" should "raise exception when axis is unknown" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    var errorMessage: String = ""

    // when

    try {
      matrix.delete(Seq(2), 'U')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid axis 'U'. 'R' => row, 'C' => column")

  }

  "Matrix +" should "add element to each cell of matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7)))

    // when

    val computeMatrix: Matrix[Int] = matrix + 6

    // then

    assert(computeMatrix.equals(Matrix.vectorToMatrix(Vector(Vector(7, 8, 9), Vector(8, 9, 10), Vector(11, 12, 13)))))

  }

  it should "add matrix to another matrix" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7), Vector(1, 2, 3)))
    val matrixB: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(6, 8, 1), Vector(4, 7, 1), Vector(3, 1, 1), Vector(4, 7, 1)))
    val matrixC: Matrix[Int] = new Matrix(300, 800, 5)

    // when

    val sumA: Matrix[Int] = matrixA + matrixB
    val sumB: Matrix[Int] = matrixC + matrixC

    // then

    assert(sumA == Matrix.vectorToMatrix(Vector(Vector(7, 10, 4), Vector(6, 10, 5), Vector(8, 7, 8), Vector(5, 9, 4))))
    assert(sumB == new Matrix[Int](300, 800, 10))

  }

  it should "raise exception when matrices don't have same dimensions" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7)))
    val matrixB: Matrix[Int] = new Matrix(300, 800, 5)
    var errorMessage: String = ""

    // when

    try {
      matrixA + matrixB
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid matrices shapes: (3,3) != (300,800)")

  }

  "Matrix -" should "subtract element to each cell of matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7)))

    // when

    val computeMatrix: Matrix[Int] = matrix - 6

    // then

    assert(computeMatrix.equals(Matrix.vectorToMatrix(Vector(Vector(-5, -4, -3), Vector(-4, -3, -2), Vector(-1, 0, 1)))))

  }

  it should "subtract matrix to another matrix" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7), Vector(1, 2, 3)))
    val matrixB: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(6, 8, 1), Vector(4, 7, 1), Vector(3, 1, 1), Vector(4, 7, 1)))
    val matrixD: Matrix[Int] = new Matrix(300, 800, 10)
    val matrixC: Matrix[Int] = new Matrix(300, 800, 6)

    // when

    val sumA: Matrix[Int] = matrixA - matrixB
    val sumB: Matrix[Int] = matrixD - matrixC

    // then

    assert(sumA == Matrix.vectorToMatrix(Vector(Vector(-5, -6, 2), Vector(-2, -4, 3), Vector(2, 5, 6), Vector(-3, -5, 2))))
    assert(sumB == new Matrix[Int](300, 800, 4))

  }

  it should "raise exception when matrices don't have same dimensions" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7)))
    val matrixB: Matrix[Int] = new Matrix(300, 800, 5)
    var errorMessage: String = ""

    // when

    try {
      matrixA - matrixB
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid matrices shapes: (3,3) != (300,800)")

  }

  "Matrix *" should "multiply element to each cell of matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7)))

    // when

    val computeMatrix: Matrix[Int] = matrix * 6

    // then

    assert(computeMatrix.equals(Matrix.vectorToMatrix(Vector(Vector(6, 12, 18), Vector(12, 18, 24), Vector(30, 36, 42)))))

  }

  it should "multiply matrix to another matrix" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 0), Vector(4, 3, -1)))
    val matrixB: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(5, 1), Vector(2, 3), Vector(3, 4)))
    val matrixC: Matrix[Int] = new Matrix(300, 800, 0)
    val matrixD: Matrix[Int] = new Matrix(800, 600, 0)

    // when

    val sumA: Matrix[Int] = matrixA * matrixB
    val sumB: Matrix[Int] = matrixC * matrixD

    // then

    assert(sumA == Matrix.vectorToMatrix(Vector(Vector(9, 7), Vector(23, 9))))
    assert(sumB == new Matrix[Int](300, 600, 0))

  }

  it should "raise exception when matrices don't have same dimensions" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7)))
    val matrixB: Matrix[Int] = new Matrix(300, 800, 5)
    var errorMessage: String = ""

    // when

    try {
      matrixA * matrixB
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid matrices shapes: n * m == m * p != 3 * 3 != 300 * 800")

  }

  "Matrix /" should "divide element to each cell of matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(5, 6, 7)))

    // when

    val computeMatrix: Matrix[Double] = matrix / 6.0

    // then

    for(i <- matrix.get.indices) {
      for(j <- matrix.get(i).indices) {
        assert((computeMatrix.get(i)(j) - matrix.get(i)(j) / 6.0) *  (computeMatrix.get(i)(j) - matrix.get(i)(j) / 6.0) < 0.000001)
      }
    }

  }

  "Matrix ^" should "pow element to each cell of matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3), Vector(5, 6, 7)))

    // when

    val computeMatrix: Matrix[Double] = matrix ^ 3.0

    // then

    for(i <- matrix.get.indices) {
      for(j <- matrix.get(i).indices) {
        assert((computeMatrix.get(i)(j) - scala.math.pow(matrix.get(i)(j), 3.0)) *  (computeMatrix.get(i)(j) - scala.math.pow(matrix.get(i)(j), 3.0)) < 0.000001)
      }
    }

  }

  it should "pow matrix" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(-3, -3), Vector(5, -6)))
    val matrixB: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8),
      Vector(9, 10, 11, 12), Vector(13, 14, 15, 16)))

    // when

    val computeMatrixA: Matrix[Int] = matrixA ^ 2
    val computeMatrixB: Matrix[Int] = matrixB ^ 4

    // then

    assert(computeMatrixA.equals(Matrix.vectorToMatrix(Vector(Vector(-6, 27), Vector(-45, 21)))))
    assert(computeMatrixB.equals(Matrix.vectorToMatrix(Vector(
      Vector(113960, 129040, 144120, 159200), Vector(263272, 298128, 332984, 367840),
      Vector(412584, 467216, 521848, 576480), Vector(561896, 636304, 710712, 785120)))))

  }

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
    val matrixB: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4, 5), Vector(6, 7, 8, 9, 10), Vector(11, 12, 13, 14, 15)))
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

  "Matrix.vectorToMatrix" should "return matrix with vector" in {

    // given

    val vector: Vector[Vector[String]] = Vector(Vector("apple", "banana"), Vector("butter", "sugar"))
    val targetMatrix: Matrix[String] = new Matrix(0, 0)
    targetMatrix.replaceMatrix(vector)

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

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

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

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val selectMatrixA: Matrix[Int] = Matrix.select(matrix, rows=Seq(0, 2))
    val selectMatrixB: Matrix[Int] = Matrix.select(matrix, cols=Seq(1, 3))
    val selectMatrixC: Matrix[Int] = Matrix.select(matrix, rows=Seq(0, 2), cols=Seq(1, 3))

    // then

    assert(selectMatrixA.get.equals(Vector(Vector(1, 2, 3, 4), Vector(9, 10, 11, 12))))
    assert(selectMatrixB.get.equals(Vector(Vector(2, 4), Vector(6, 8), Vector(10, 12))))
    assert(selectMatrixC.get.equals(Vector(Vector(2, 4), Vector(10, 12))))

  }

  "Matrix.sum" should "raise exception when axis is unknown" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    var errorMessage: String = ""

    // when

    try {
      Matrix.sum(matrix, 'T')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid axis 'T'. 'A' => all, 'R' => row, 'C' => column")

  }

  "Matrix.sumRow" should "return matrix of row sum" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val sumMatrix: Matrix[Int] = Matrix.sum(matrix, 'R')

    // then

    assert(sumMatrix.get.equals(Vector(Vector(10), Vector(26), Vector(42))))

  }

  "Matrix.sumColumn" should "return matrix of row sum" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val sumMatrix: Matrix[Int] = Matrix.sum(matrix, 'C')

    // then

    assert(sumMatrix.get.equals(Vector(Vector(15, 18, 21, 24))))

  }

  "Matrix.sumMatrix" should "return sum off all cells in another matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val sumMatrix: Matrix[Int] = Matrix.sum(matrix)

    // then

    assert(sumMatrix.get.equals(Vector(Vector(78))))

  }

  "Matrix.avgRow" should "return matrix of row average" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val computeMatrix: Matrix[Double] = Matrix.avg(matrix, 'R')

    // then

    for(i <- computeMatrix.get.indices){
      assert(scala.math.pow(computeMatrix.get(i)(0) - matrix.get(i).sum / matrix.get(i).length.toDouble, 2) < 0.00001)
    }

  }

  it should "raise exception when matrix hasn't columns" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(), Vector(), Vector()))
    var errorMessage: String = ""

    // when

    try {
      Matrix.avg(matrix, 'R')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid matrix: no columns")

  }

  "Matrix.avgColumn" should "return matrix of column average" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))
    val transposeVectors: Vector[Vector[Int]] = matrix.get.transpose

    // when

    val computeMatrix: Matrix[Double] = Matrix.avg(matrix, 'C')

    // then

    for(i <- computeMatrix.get(0).indices){
      assert(scala.math.pow(computeMatrix.get(0)(i) - transposeVectors(i).sum / transposeVectors(i).length.toDouble, 2) < 0.00001)
    }

  }

  it should "raise exception when matrix hasn't rows" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    var errorMessage: String = ""

    // when

    try {
      Matrix.avg(matrix, 'C')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid matrix: no rows")

  }

  "Matrix.avgMatrix" should "return average of all cells in another matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(5, 6, 7, 8), Vector(9, 10, 11, 12)))

    // when

    val computeMatrix: Matrix[Double] = Matrix.avg(matrix)

    // then

    assert(scala.math.pow(computeMatrix.get(0)(0) - 78.0 / 12.0, 2) < 0.00001)

  }

  it should "raise exception when matrix is empty" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    val matrixB: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector()))
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      Matrix.avg(matrixA)
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      Matrix.avg(matrixB)
    } catch {
      case e: IllegalArgumentException => errorMessageB = e.getMessage
    }

    // then

    assert(errorMessageA == "Matrix is empty")
    assert(errorMessageB == "Matrix is empty")

  }

  "Matrix.avg" should "raise exception when axis is unknown" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    var errorMessage: String = ""

    // when

    try {
      Matrix.avg(matrix, 'T')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid axis 'T'. 'A' => all, 'R' => row, 'C' => column")

  }

  "Matrix.covariance" should "return covariance matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(
      Vector(1, 4, 8, 7),
      Vector(7, 9, 89, 9),
      Vector(2, 3, 6, 89),
      Vector(17, 56, 89, 12)
    ))
    val targetMatrix: Matrix[Double] = Matrix.vectorToMatrix(Vector(
      Vector(7.5, 62.0, 46.25, 41.75),
      Vector(62.0 , 1220.75, -368.5,  923.25),
      Vector(46.25, -368.5, 1367.5, -636.5),
      Vector(41.75,  923.25, -636.5 , 980.25)
    ))

    // when

    val covariance: Matrix[Double] = Matrix.covariance(matrix)

    // then

    for(i <- targetMatrix.get.indices) {
      for(j <- targetMatrix.get(i).indices) {
        assert(scala.math.pow(covariance.get(i)(j) - targetMatrix.get(i)(j), 2) < 0.000001)
      }
    }

  }

  it should "raise exception when matrix is empty" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    val matrixB: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector()))
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      Matrix.covariance(matrixA)
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      Matrix.variance(matrixB)
    } catch {
      case e: IllegalArgumentException => errorMessageB = e.getMessage
    }

    // then

    assert(errorMessageA == "Matrix is empty")
    assert(errorMessageB == "Matrix is empty")

  }

  "Matrix.varianceMatrix" should "return variance of whole matrix" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(
      Vector(1, 4, 8, 7),
      Vector(7, 9, 89, 9),
      Vector(2, 3, 6, 89),
      Vector(17, 56, 89, 12)
    ))

    // when

    val variance: Matrix[Double] = Matrix.variance(matrix)

    // then

    assert(variance.equals(Matrix.vectorToMatrix(Vector(Vector(1082.375)))))

  }

  it should "raise exception when matrix is empty" in {

    // given

    val matrixA: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    val matrixB: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector()))
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      Matrix.variance(matrixA)
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      Matrix.variance(matrixB)
    } catch {
      case e: IllegalArgumentException => errorMessageB = e.getMessage
    }

    // then

    assert(errorMessageA == "Matrix is empty")
    assert(errorMessageB == "Matrix is empty")

  }

  "Matrix.varianceRow" should "return variance of matrix rows" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(
      Vector(1, 4, 8, 7),
      Vector(7, 9, 89, 9),
      Vector(2, 3, 6, 89),
      Vector(17, 56, 89, 12)
    ))
    val targetMatrix: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(7.5, 1220.75, 1367.5, 980.25)))

    // when

    val variance: Matrix[Double] = Matrix.variance(matrix, 'R')

    // then

    for(i <- targetMatrix.get.indices) {
      for(j <- targetMatrix.get(i).indices) {
        assert(scala.math.pow(variance.get(i)(j) - targetMatrix.get(i)(j), 2) < 0.000001)
      }
    }

  }

  it should "raise exception when matrix hasn't columns" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(Vector(), Vector(), Vector()))
    var errorMessage: String = ""

    // when

    try {
      Matrix.variance(matrix, 'R')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid matrix: no columns")

  }

  "Matrix.varianceColumn" should "return variance of matrix columns" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector(
      Vector(1, 4, 8, 7),
      Vector(7, 9, 89, 9),
      Vector(2, 3, 6, 89),
      Vector(17, 56, 89, 12)
    ))
    val targetMatrix: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(40.1875, 486.5, 1681.5, 1193.1875)))

    // when

    val variance: Matrix[Double] = Matrix.variance(matrix, 'C')

    // then

    for(i <- targetMatrix.get.indices) {
      for(j <- targetMatrix.get(i).indices) {
        assert(scala.math.pow(variance.get(i)(j) - targetMatrix.get(i)(j), 2) < 0.000001)
      }
    }

  }

  it should "raise exception when matrix hasn't rows" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    var errorMessage: String = ""

    // when

    try {
      Matrix.variance(matrix, 'C')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid matrix: no rows")

  }

  "Matrix.variance" should "raise exception when axis is unknown" in {

    // given

    val matrix: Matrix[Int] = Matrix.vectorToMatrix(Vector())
    var errorMessage: String = ""

    // when

    try {
      Matrix.variance(matrix, 'T')
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "Invalid axis 'T'. 'A' => all, 'R' => row, 'C' => column")

  }

}
