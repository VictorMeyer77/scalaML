package com.victormeyer.scalaml.data

import scala.math.Numeric.Implicits.infixNumericOps

/** Matrix 2D compose with Vectors.
 * Both of row and column operations are available, but it's optimize on row operations.
 * Type of this Matrix is generic, but for many methods, Numeric types are mandatory.
 *
 * @param rows Number of rows
 * @param cols Number of columns
 * @param initValue Default value for each cell (optional)
 * @tparam T Type of matrix
 */
class Matrix[T](rows: Int, cols: Int, initValue: T=null.asInstanceOf[T]) {

  private var matrix: Vector[Vector[T]] = Vector.fill(rows)(Vector.fill(cols)(initValue))

  /** Get the matrix
   *
   * @return Vector of vectors composing matrix
   */
  def get: Vector[Vector[T]] = matrix

  /** Print matrix in a formatted string */
  def show(): Unit = println(toString)

  /** Get shape of the matrix
   *
   * @return Tuple of integer composed by number of rows and number of columns
   */
  def shape: (Int, Int) = {
    if(matrix.nonEmpty){
      (matrix.length, matrix(0).length)
    } else {
      (0, 0)
    }
  }

  /** Replace current vectors with given vectors
   *
   * @param matrix New vector of vectors
   */
  def set(matrix: Vector[Vector[T]]): Unit ={
    if(matrix.map(row => row.length).distinct.length > 1){
      throw new IllegalArgumentException(s"Invalid shape: different sizes of vectors found (${matrix.map(row => row.length).distinct.mkString(", ")})")
    }
    this.matrix = matrix
  }

  /** Replace value of a specific cell
   *
   * @param row Row index
   * @param col Column index
   * @param value New value
   */
  def setValue(row: Int, col: Int, value: T): Unit ={
    if(row >= shape._1){
      throw new IllegalArgumentException(s"Invalid row index: $row >= ${shape._1}")
    } else if(col >= shape._2){
      throw new IllegalArgumentException(s"Invalid column index: $col >= ${shape._2}")
    }
    matrix = matrix updated (row, matrix(row) updated (col, value))
  }

  /** Replace entire row
   *
   * @param index Row index
   * @param row New row
   */
  def setRow(index: Int, row: Vector[T]): Unit ={
    if(row.length != shape._2){
      throw new IllegalArgumentException(s"Invalid row size: ${row.length} != ${shape._2}")
    } else if(index >= shape._1){
      throw new IllegalArgumentException(s"Invalid row index: $index >= ${shape._1}")
    }
    matrix = matrix updated (index, row)
  }

  /** Replace entire column
   *
   * @param index Column index
   * @param column New column
   */
  def setColumn(index: Int, column: Vector[T]): Unit ={
    if(column.length != shape._1){
      throw new IllegalArgumentException(s"Invalid column size: ${column.length} != ${shape._1}")
    } else if(index >= shape._2){
      throw new IllegalArgumentException(s"Invalid column index: $index >= ${shape._2}")
    }
    matrix = (matrix.transpose updated (index, column)).transpose
  }

  /** Delete specific row
   *
   * @param index Row index
   */
  def dropRow(index: Int): Unit ={
    if(index >= shape._1){
      throw new IllegalArgumentException(s"Invalid row index: $index >= ${shape._1}")
    }
    matrix = matrix.filter(row => matrix.indexOf(row) != index)
  }

  /** Delete specific column
   *
   * @param index Column Index
   */
  def dropColumn(index: Int): Unit ={
    if(index >= shape._2){
      throw new IllegalArgumentException(s"Invalid column index: $index >= ${shape._2}")
    }
    val matrixTranspose: Vector[Vector[T]] = matrix.transpose
    matrix = matrixTranspose.filter(row => matrixTranspose.indexOf(row) != index).transpose
  }

  /** Add element to each cell of matrix
   *
   * @param value Element to add
   * @param numeric Numeric instance
   * @return New matrix with incrementing cells
   */
  def :+(value: T)(implicit numeric: Numeric[T]): Matrix[T] ={
    val outputVector: Vector[Vector[T]] = matrix.map(row => row.map(col => col + value))
    Matrix.vectorToMatrix(outputVector)
  }

  /** Subtract element to each cell of matrix
   *
   * @param value Element to subtract
   * @param numeric Numeric instance
   * @return New matrix with subtracting cells
   */
  def :-(value: T)(implicit numeric: Numeric[T]): Matrix[T] ={
    val outputVector: Vector[Vector[T]] = matrix.map(row => row.map(col => col - value))
    Matrix.vectorToMatrix(outputVector)
  }

  /** Multiply element to each cell of matrix
   *
   * @param value Element to multiply
   * @param numeric Numeric instance
   * @return New matrix with multiplying cells
   */
  def :*(value: T)(implicit numeric: Numeric[T]): Matrix[T] ={
    val outputVector: Vector[Vector[T]] = matrix.map(row => row.map(col => col * value))
    Matrix.vectorToMatrix(outputVector)
  }

  /** Divide element to each cell of matrix
   *
   * @param value Element to divide
   * @param numeric Numeric instance
   * @return New matrix with dividing cells
   */
  def :/(value: Double)(implicit numeric: Numeric[T]): Matrix[Double] ={
    val outputVector: Vector[Vector[Double]] = matrix.map(row => row.map(col => col.toDouble / value))
    Matrix.vectorToMatrix(outputVector)
  }

  /** Pow element to each cell of matrix
   *
   * @param value Element to pow
   * @param numeric Numeric instance
   * @return New matrix with powered cells
   */
  def :^(value: Double)(implicit numeric: Numeric[T]): Matrix[Double] ={
    val outputVector: Vector[Vector[Double]] = matrix.map(row => row.map(col => scala.math.pow(col.toDouble, value)))
    Matrix.vectorToMatrix(outputVector)
  }

  override def toString: String ={
    val matrixStr: StringBuilder = new StringBuilder("[")
    matrix.foreach(row => matrixStr.append(s"[${row.mkString(", ")}]\n"))
    if(matrixStr.length > 1)
      matrixStr.deleteCharAt(matrixStr.size - 1)
    truncateString(matrixStr.append("]").mkString)
  }

  /** Format matrix string
   *
   * @param matrixStr Matrix issued of toString
   * @return Formatted string
   */
  private def truncateString(matrixStr: String): String ={
    val truncateMatrixStr: StringBuilder = new StringBuilder()
    val rows: Array[String] = matrixStr.split("\n")
    val truncateRows: Array[String] = rows.map(row => {
      if(row.length > 100){
        row.slice(0, 50) + "\t...\t" + row.slice(row.length - 50, row.length)
      } else {
        row
      }
    })
    if(truncateRows.length > 10){
      truncateMatrixStr.append(truncateRows.slice(0, 5).mkString("\n"))
      truncateMatrixStr.append("\n...\n")
      truncateMatrixStr.append(truncateRows.slice(truncateRows.length - 5, truncateRows.length).mkString("\n"))
    } else {
      truncateMatrixStr.append(truncateRows.mkString("\n"))
    }
    truncateMatrixStr.mkString
  }

  override def equals(obj: Any): Boolean ={
    var equal: Boolean = true
    if(obj.getClass != this.getClass){
      equal = false
    } else {
      val matrix: Matrix[T] = obj.asInstanceOf[Matrix[T]]
      if(matrix.shape != shape){
        equal = false
      } else {
        var i: Int = 0
        while(i < this.matrix.length && equal){
          if(matrix.get(i) != this.matrix(i))
            equal = false
          i += 1
        }
      }
    }
    equal
  }

  override def clone: Matrix[T] ={
    val matrix: Matrix[T] = new Matrix[T](rows, cols, initValue)
    matrix.set(get)
    matrix
  }

}

/**
 * Group of methods for advanced processing of Matrix.
 * Each of these methods returns another instance of Matrix.
 */
object Matrix {

  /** Convert a vector of vectors to an instance of Matrix
   *
   * @param vector Vector of vectors to convert
   * @tparam T Type of matrix
   * @return Instance of matrix with given vector
   */
  def vectorToMatrix[T](vector: Vector[Vector[T]]): Matrix[T] ={
    val newMatrix: Matrix[T] = new Matrix(0, 0)
    newMatrix.set(vector)
    newMatrix
  }

  /** Resize matrix
   *
   * @param matrix Matrix to resize
   * @param rows New number of rows
   * @param cols New columns of rows
   * @tparam T Type of matrix
   * @return Resized matrix
   */
  def reshape[T](matrix: Matrix[T], rows: Int, cols: Int): Matrix[T] ={
    if(matrix.shape._1 * matrix.shape._2 != rows * cols){
      throw new IllegalArgumentException(s"Invalid dimensions: ${matrix.shape._1} * ${matrix.shape._2} != $rows * $cols")
    }
    var vectorBuffer: Vector[Vector[T]] = Vector.fill(rows)(Vector())
    val flatMatrix: Vector[T] = matrix.get.foldLeft(Vector[T]())((acc, v) => acc ++ v)
    for(i <- 0 until rows){
      vectorBuffer = vectorBuffer updated (i, flatMatrix.slice(i * cols, i * cols + cols))
    }
    vectorToMatrix(vectorBuffer)
  }

  /** Select given rows or/and columns of Matrix
   *
   * @param matrix Matrix to select
   * @param rows Row indexes to select (optional)
   * @param cols Column indexes to select (optional)
   * @tparam T Type of matrix
   * @return Matrix with selected rows and columns
   */
  def select[T](matrix: Matrix[T], rows: Seq[Int]=Seq(), cols: Seq[Int]=Seq()): Matrix[T] ={
    val shape: (Int, Int) = matrix.shape
    if(rows.exists(index => index >= shape._1)){
      throw new IllegalArgumentException(s"Invalid row indexes: [${rows.mkString(", ")}] >= ${shape._1}")
    } else if(cols.exists(index => index >= shape._2)){
      throw new IllegalArgumentException(s"Invalid column indexes: [${cols.mkString(", ")}] >= ${shape._2}")
    }
    val selectRows: Vector[Vector[T]] = if(rows.nonEmpty) {
      matrix.get.filter(row => rows.contains(matrix.get.indexOf(row)))
    } else {
      matrix.get
    }
    val selectVector: Vector[Vector[T]] = if(cols.nonEmpty) {
      selectRows.map(row => row.filter(col => cols.contains(row.indexOf(col))))
    } else {
      selectRows
    }
    vectorToMatrix(selectVector)
  }

  /** Return sum of each matrix rows
   *
   * @param matrix Matrix to calculate sums
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of sums
   */
  def sumRow[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] ={
    val vectorSum: Vector[Vector[T]] = matrix.get.map(vector => Vector(vector.sum))
    vectorToMatrix(vectorSum)
  }

  /** Return sum of each matrix columns
   *
   * @param matrix Matrix to calculate sums
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of sums
   */
  def sumColumn[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] ={
    val vectorSum: Vector[Vector[T]] = Vector(matrix.get.transpose.map(_.sum))
    vectorToMatrix(vectorSum)
  }

}
