package com.victormeyer.scalaml.data


class Matrix[T](rows: Int, cols: Int, initValue: T) {

  // create

  private var matrix: Vector[Vector[T]] = Vector.fill(rows)(Vector.fill(cols)(initValue))

  // read

  def get: Vector[Vector[T]] = matrix

  def show(): Unit = println(toString)

  def shape: (Int, Int) = {
    if(matrix.nonEmpty){
      (matrix.length, matrix(0).length)
    } else {
      (0, 0)
    }
  }

  // update

  def set(matrix: Vector[Vector[T]]): Unit ={
    this.matrix = matrix
  }

  def setValue(row: Int, col: Int, value: T): Unit ={
    if(row >= shape._1){
      throw new IllegalArgumentException(s"Invalid row index: $row >= ${shape._1}")
    } else if(col >= shape._2){
      throw new IllegalArgumentException(s"Invalid column index: $col >= ${shape._2}")
    }
    matrix = matrix updated (row, matrix(row) updated (col, value))
  }

  def setRow(index: Int, row: Vector[T]): Unit ={
    if(row.length != shape._2){
      throw new IllegalArgumentException(s"Invalid row size: ${row.length} != ${shape._2}")
    } else if(index >= shape._1){
      throw new IllegalArgumentException(s"Invalid row index: $index >= ${shape._1}")
    }
    matrix = matrix updated (index, row)
  }

  def setColumn(index: Int, column: Vector[T]): Unit ={
    if(column.length != shape._1){
      throw new IllegalArgumentException(s"Invalid column size: ${column.length} != ${shape._1}")
    } else if(index >= shape._2){
      throw new IllegalArgumentException(s"Invalid column index: $index >= ${shape._2}")
    }
    for(i <- column.indices){
      matrix = matrix updated (i, matrix(i) updated (index, column(i)))
    }
  }

  // delete

  def dropRow(index: Int): Unit ={
    if(index >= shape._1){
      throw new IllegalArgumentException(s"Invalid row index: $index >= ${shape._1}")
    }
    matrix = matrix.filter(row => matrix.indexOf(row) != index)
  }

  def dropColumn(index: Int): Unit ={
    if(index >= shape._2){
      throw new IllegalArgumentException(s"Invalid column index: $index >= ${shape._2}")
    }
    for(i <- matrix.indices){
      matrix = matrix updated (i, matrix(i).filter(row => matrix(i).indexOf(row) != index))
    }
  }

  // override

  override def toString: String ={
    val matrixStr: StringBuilder = new StringBuilder("[")
    matrix.foreach(row => matrixStr.append(s"[${row.mkString(", ")}]\n"))
    if(matrixStr.length > 1)
      matrixStr.deleteCharAt(matrixStr.size - 1)
    truncateString(matrixStr.append("]").mkString)
  }

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
        for(i <- this.matrix.indices)
          for(j <- this.matrix(i).indices)
            if(!matrix.get(i)(j).equals(this.matrix(i)(j)))
              equal = false
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
 * Matrix operations
 */

object Matrix {

  def reshape[T](matrix: Matrix[T], rows: Int, cols: Int): Matrix[T] ={
    if(matrix.shape._1 * matrix.shape._2 != rows * cols){
      throw new IllegalArgumentException(s"Invalid dimensions: ${matrix.shape._1} * ${matrix.shape._2} != $rows * $cols")
    }
    var vectorBuffer: Vector[Vector[T]] = Vector.fill(rows)(Vector())
    val flatMatrix: Vector[T] = matrix.get.foldLeft(Vector[T]())((acc, v) => acc ++ v)
    for(i <- 0 until rows){
      vectorBuffer = vectorBuffer updated (i, flatMatrix.slice(i * cols, i * cols + cols))
    }
    val newMatrix: Matrix[T] = matrix.clone
    newMatrix.set(vectorBuffer)
    newMatrix
  }

}
