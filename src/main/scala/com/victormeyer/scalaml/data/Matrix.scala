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
  def replaceMatrix(matrix: Vector[Vector[T]]): Unit ={
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
  def replaceCell(row: Int, col: Int, value: T): Unit ={
    if(row >= shape._1){
      throw new IllegalArgumentException(s"Invalid row index: $row >= ${shape._1}")
    } else if(col >= shape._2){
      throw new IllegalArgumentException(s"Invalid column index: $col >= ${shape._2}")
    }
    matrix = matrix updated (row, matrix(row) updated (col, value))
  }

  /** Replace specific row or column
   *
   * @param index Index of row or column
   * @param vector New row or column
   * @param axis Type of replacement. 'R' for row, 'C' for column
   */
  def replace(index: Int, vector: Vector[T], axis: Char='R'): Unit ={
    axis.toUpper match {
      case 'R' => replaceRow(index, vector)
      case 'C' => replaceColumn(index, vector)
      case _ => throw new IllegalArgumentException(s"Invalid axis '$axis'. 'R' => row, 'C' => column")
    }
  }

  /** Replace entire row
   *
   * @param index Row index
   * @param row New row
   */
  private def replaceRow(index: Int, row: Vector[T]): Unit ={
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
  private def replaceColumn(index: Int, column: Vector[T]): Unit ={
    if(column.length != shape._1){
      throw new IllegalArgumentException(s"Invalid column size: ${column.length} != ${shape._1}")
    } else if(index >= shape._2){
      throw new IllegalArgumentException(s"Invalid column index: $index >= ${shape._2}")
    }
    matrix = (matrix.transpose updated (index, column)).transpose
  }

  /** Remove rows or columns from the matrix
   *
   * @param indexes Indexes of rows or columns
   * @param axis Type of suppression. 'R' for row, 'C' for column
   */
  def delete(indexes: Seq[Int], axis: Char='R'): Unit ={
    axis.toUpper match {
      case 'R' => deleteRows(indexes)
      case 'C' => deleteColumns(indexes)
      case _ => throw new IllegalArgumentException(s"Invalid axis '$axis'. 'R' => row, 'C' => column")
    }
  }

  /** Delete specific row
   *
   * @param indexes Row indexes
   */
  private def deleteRows(indexes: Seq[Int]): Unit ={
    if(indexes.exists(index => index >= shape._1)){
      throw new IllegalArgumentException(s"Invalid row indexes: (${indexes.filter(index => index >= shape._1).mkString(", ")}) >= ${shape._1}")
    }
    matrix = matrix.zipWithIndex.filter(row => !indexes.contains(row._2)).map(row => row._1)
  }

  /** Delete specific column
   *
   * @param indexes Column Indexes
   */
  private def deleteColumns(indexes: Seq[Int]): Unit ={
    if(indexes.exists(index => index >= shape._2)){
      throw new IllegalArgumentException(s"Invalid column indexes: (${indexes.filter(index => index >= shape._2).mkString(", ")}) >= ${shape._2}")
    }
    matrix = matrix.transpose.zipWithIndex.filter(row => !indexes.contains(row._2)).map(row => row._1).transpose
  }

  /** Add element to each cell of matrix
   *
   * @param value Element to add
   * @param numeric Numeric instance
   * @return New matrix with incrementing cells
   */
  def +(value: T)(implicit numeric: Numeric[T]): Matrix[T] ={
    val outputVector: Vector[Vector[T]] = matrix.map(row => row.map(col => col + value))
    Matrix.vectorToMatrix(outputVector)
  }

  /** Addition between two matrices
   *
   * @param matrix Matrix to add
   * @param numeric Numeric instance
   * @return New matrix of addition
   */
  def +(matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] ={
    if(shape != matrix.shape){
      throw new IllegalArgumentException(s"Invalid matrices shapes: $shape != ${matrix.shape}")
    }
    val sum: Vector[Vector[T]] =
      this.matrix.zipWithIndex.map(row => Vector(row._1, matrix.get(row._2)).transpose.map(_.sum))
    Matrix.vectorToMatrix(sum)
  }

  /** Subtract element to each cell of matrix
   *
   * @param value Element to subtract
   * @param numeric Numeric instance
   * @return New matrix with subtracting cells
   */
  def -(value: T)(implicit numeric: Numeric[T]): Matrix[T] ={
    val outputVector: Vector[Vector[T]] = matrix.map(row => row.map(col => col - value))
    Matrix.vectorToMatrix(outputVector)
  }

  /** Subtraction between two matrices
   *
   * @param matrix Matrix to subtract
   * @param numeric Numeric instance
   * @return New matrix of subtraction
   */
  def -(matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] ={
    if(shape != matrix.shape){
      throw new IllegalArgumentException(s"Invalid matrices shapes: $shape != ${matrix.shape}")
    }
    val difference: Vector[Vector[T]] =
      this.matrix.zipWithIndex.map(row => Vector(row._1, matrix.get(row._2)).transpose.map(v => v(0) - v(1)))
    Matrix.vectorToMatrix(difference)
  }

  /** Multiply element to each cell of matrix
   *
   * @param value Element to multiply
   * @param numeric Numeric instance
   * @return New matrix with multiplying cells
   */
  def *(value: T)(implicit numeric: Numeric[T]): Matrix[T] ={
    val outputVector: Vector[Vector[T]] = matrix.map(row => row.map(col => col * value))
    Matrix.vectorToMatrix(outputVector)
  }

  /** Product between two matrices
   *
   * @param matrix Matrix to multiply
   * @param numeric Numeric instance
   * @return Product of the two matrices
   */
  def *(matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] ={
    if(shape._2 != matrix.shape._1){
      throw new IllegalArgumentException(s"Invalid matrices shapes: " +
        s"n * m == m * p != ${shape._1} * ${shape._2} != ${matrix.shape._1} * ${matrix.shape._2}")
    }
    val product: Vector[Vector[T]] =
      this.matrix.map(a => matrix.get.transpose.map(b => Vector(a, b)))
        .map(c => c.map(d => d.transpose.map(e => e(0) * e(1))).map(f => f.sum))
    Matrix.vectorToMatrix(product)
  }

  /** Divide element to each cell of matrix
   *
   * @param value Element to divide
   * @param numeric Numeric instance
   * @return New matrix with dividing cells
   */
  def /(value: Double)(implicit numeric: Numeric[T]): Matrix[Double] ={
    val outputVector: Vector[Vector[Double]] = matrix.map(row => row.map(col => col.toDouble / value))
    Matrix.vectorToMatrix(outputVector)
  }

  def /(matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] = ???

  /** Pow element to each cell of matrix
   *
   * @param value Element to pow
   * @param numeric Numeric instance
   * @return New matrix with powered cells
   */
  def ^(value: Double)(implicit numeric: Numeric[T]): Matrix[Double] ={
    val outputVector: Vector[Vector[Double]] = matrix.map(row => row.map(col => scala.math.pow(col.toDouble, value)))
    Matrix.vectorToMatrix(outputVector)
  }

  /** Pow matrix
   *
   * @param value Exponent of the matrix
   * @param numeric Numeric instance
   * @return Powered matrix
   */
  def ^(value: Int)(implicit numeric: Numeric[T]): Matrix[T] ={
    if(value > 0) {
      (1 until value).foldLeft(this)((acc, m) => this * acc)
    } else {
      ???
    }
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
    matrix.replaceMatrix(get)
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
    newMatrix.replaceMatrix(vector)
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
      matrix.get.zipWithIndex.filter(row => rows.contains(row._2)).map(row => row._1)
    } else {
      matrix.get
    }
    val selectVector: Vector[Vector[T]] = if(cols.nonEmpty) {
      selectRows.map(row => row.zipWithIndex.filter(col => cols.contains(col._2)).map(col => col._1))
    } else {
      selectRows
    }
    vectorToMatrix(selectVector)
  }

  /** Compute sum of a matrix
   *
   * @param matrix Matrix to compute sum
   * @param axis Type of sum: 'A' for all cells, 'R' for rows, 'C' for columns
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of sums
   */
  def sum[T](matrix: Matrix[T], axis: Char='A')(implicit numeric: Numeric[T]): Matrix[T] ={
    axis.toUpper match {
      case 'A' => sumMatrix(matrix)
      case 'R' => sumRow(matrix)
      case 'C' => sumColumn(matrix)
      case _ => throw new IllegalArgumentException(s"Invalid axis '$axis'. 'A' => all, 'R' => row, 'C' => column")
    }
  }

  /** Return sum of each rows
   *
   * @param matrix Matrix to calculate sums
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of sums
   */
  private def sumRow[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] ={
    val vectorSum: Vector[Vector[T]] = matrix.get.map(vector => Vector(vector.sum))
    vectorToMatrix(vectorSum)
  }

  /** Return sum of each columns
   *
   * @param matrix Matrix to calculate sums
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of sums
   */
  private def sumColumn[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] ={
    val vectorSum: Vector[Vector[T]] = Vector(matrix.get.transpose.map(_.sum))
    vectorToMatrix(vectorSum)
  }

  /** Return sum of each cells
   *
   * @param matrix Matrix to calculate sums
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of sum
   */
  private def sumMatrix[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[T] ={
    vectorToMatrix(Vector(Vector(matrix.get.map(row => row.sum).sum)))
  }

  /** Compute average of a matrix
   *
   * @param matrix Matrix to compute average
   * @param axis Type of average: 'A' for all cells, 'R' for rows, 'C' for columns
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of averages
   */
  def avg[T](matrix: Matrix[T], axis: Char='A')(implicit numeric: Numeric[T]): Matrix[Double] ={
    axis.toUpper match {
      case 'A' => avgMatrix(matrix)
      case 'R' => avgRow(matrix)
      case 'C' => avgColumn(matrix)
      case _ => throw new IllegalArgumentException(s"Invalid axis '$axis'. 'A' => all, 'R' => row, 'C' => column")
    }
  }

  /** Compute average of each rows
   *
   * @param matrix Matrix to compute average
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of averages
   */
  private def avgRow[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[Double] ={
    if(matrix.shape._2 == 0){
      throw new IllegalArgumentException("Invalid matrix: no columns")
    }
    sumRow(matrix) / matrix.shape._2
  }

  /** Compute average of each columns
   *
   * @param matrix Matrix to compute average
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of averages
   */
  private def avgColumn[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[Double] ={
    if(matrix.shape._1 == 0){
      throw new IllegalArgumentException("Invalid matrix: no rows")
    }
    sumColumn(matrix) / matrix.shape._1
  }

  /** Compute average of each cells
   *
   * @param matrix Matrix to compute average
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of average
   */
  private def avgMatrix[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[Double] ={
    if(matrix.shape._1 == 0 || matrix.shape._2 == 0){
      throw new IllegalArgumentException("Matrix is empty")
    }
    sumMatrix(matrix) / (matrix.shape._1 * matrix.shape._2)
  }

  /** Compute the covariance matrix
   *
   * @param matrix Matrix to compute covariance
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of covariance
   */
  def covariance[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[Double] ={
    if(matrix.shape._1 == 0 || matrix.shape._2 == 0){
      throw new IllegalArgumentException("Matrix is empty")
    }
    val avgRows: Matrix[Double] = avgRow(matrix)
    val vectors: Vector[Vector[T]] = matrix.get
    val rowLength: Double = matrix.shape._1.toDouble
    val covVectors: Vector[Vector[Double]] = vectors.indices.map(rowIndexA => {
      vectors.indices.map(rowIndexB => {
        vectors(rowIndexA).zip(vectors(rowIndexB)).map(pair => {
          pair._1 * pair._2
        }).sum.toDouble / rowLength - avgRows.get(rowIndexA)(0) * avgRows.get(rowIndexB)(0)
      }).toVector
    }).toVector
    vectorToMatrix(covVectors)
  }

  /** Compute variance of a matrix
   *
   * @param matrix Matrix to compute variance
   * @param axis Type of variance: 'A' for all cells, 'R' for rows, 'C' for columns
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of variances
   */
  def variance[T](matrix: Matrix[T], axis: Char='A')(implicit numeric: Numeric[T]): Matrix[Double] ={
    axis.toUpper match {
      case 'A' => varianceMatrix(matrix)
      case 'R' => varianceRow(matrix)
      case 'C' => varianceColumn(matrix)
      case _ => throw new IllegalArgumentException(s"Invalid axis '$axis'. 'A' => all, 'R' => row, 'C' => column")
    }
  }

  /** Compute variance of each rows
   *
   * @param matrix Matrix to compute variance
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of variances
   */
  private def varianceRow[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[Double] ={
    if(matrix.shape._2 == 0){
      throw new IllegalArgumentException("Invalid matrix: no columns")
    }
    val variances: Vector[Double] = matrix.get.map(row => {
      (row.map(c => c * c).sum.toDouble / row.length.toDouble) - scala.math.pow(row.sum.toDouble / row.length.toDouble, 2)
    })
    vectorToMatrix(Vector(variances))
  }

  /** Compute variances of each columns
   *
   * @param matrix Matrix to compute variances
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of variances
   */
  private def varianceColumn[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[Double] ={
    if(matrix.shape._1 == 0){
      throw new IllegalArgumentException("Invalid matrix: no rows")
    }
    val variances: Vector[Double] = matrix.get.transpose.map(row => {
      (row.map(c => c * c).sum.toDouble / row.length.toDouble) - scala.math.pow(row.sum.toDouble / row.length.toDouble, 2)
    })
    vectorToMatrix(Vector(variances))
  }

  /** Compute variance of each cells
   *
   * @param matrix Matrix to compute variance
   * @param numeric Numeric instance
   * @tparam T Type of matrix
   * @return Matrix of variance
   */
  private def varianceMatrix[T](matrix: Matrix[T])(implicit numeric: Numeric[T]): Matrix[Double] ={
    if(matrix.shape._1 == 0 || matrix.shape._2 == 0){
      throw new IllegalArgumentException("Matrix is empty")
    }
    avgMatrix(matrix ^ 2.0) - (avgMatrix(matrix) ^ 2.0)
  }

}
