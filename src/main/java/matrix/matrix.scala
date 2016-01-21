/**
  * Created by lywen on 16/1/19.
  * 定义 Matrix 用于矩阵计算
  * + 代表矩阵加法
  * - 代表矩阵减法
  * * 代表矩阵乘法
  * ^^ 矩阵指数
  */

package matrix.matrix
//矩阵运算
class Matrix[T](A: List[List[T]]) {
  //矩阵加法
  private val B: List[List[Double]] = A.map(_.map(_.toString.toDouble))

  def toList = this.B

  def +(that: Matrix[T]): Matrix[Double] = new Matrix((this.B, that.B).zipped.map((_, _).zipped.map(_ + _)))

  // 矩阵减法
  def -(that: Matrix[T]): Matrix[Double] = new Matrix((this.B, that.B).zipped.map((_, _).zipped.map(_ - _)))

  //矩阵乘法:A:n*m,B:m*k
  def *(that: Matrix[T]): Matrix[Double] = {
    val NewThat = that.T //对矩阵进行转制
    new Matrix(this.B.map(x => NewThat.B.map(y => (x, y).zipped.map(_ * _).reduce(_ + _))))
  }

  //标量乘除法
  def plus(a: T): Matrix[Double] = new Matrix(this.B.map(_ map (_ * a.toString.toDouble)))

  // 矩阵元素取指数
  def ^(a: T): Matrix[Double] = new Matrix(this.B.map(_ map (math.pow(_, a.toString.toDouble))))

  //矩阵转置
  def T = {
    var Tran: List[List[Double]] = this.B(0).map(x => List())
    this.B.foreach(append(_))
    def append(x: List[Double]) = {
      Tran = (Tran, x).zipped.map(_ ::: List(_))
    }
    new Matrix(Tran)
  }

  override def toString =

    this.B.map(_ mkString (", ")).mkString("[", "\n ", "]") + " -->Dim(" + this.B.length + "," + this.B(0).length + ")" + "\n"


  }