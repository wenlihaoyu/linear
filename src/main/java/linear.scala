/**
  * Created by lywen on 16/1/19.
  */

//梯度法求解线性回归方程参数
package src.linear
import matrix.matrix.Matrix

class linear[T](X:List[List[T]], Y:List[T]){
  // X 为自变量矩阵
  // Y
  private val lm_X :List[List[Double]]= X.map(1.0::_).map(_.map(_.toString.toDouble))
  private var weight:List[Double] = (lm_X(0).map(x=>0.0)) // 初始化参数向量
  private val lm_Y :List[Double]= Y.map(_.toString.toDouble)
  private var a =0.001

  // 损失函数
  private def LossFunction(lm_X:List[List[Double]],lm_Y:List[Double],W:List[Double]): List[Double] ={

      val n = lm_X.length
      val NewX = (new Matrix(lm_X)).T.toList
      val  H :List[Double]= lmFunction(lm_X, W)
      val New_weight =  (W, NewX).zipped.map((x,y)=>x+ a*(lm_Y, H, y).zipped.map((y,h,x)=>(y-h)*x).sum/n)
      return New_weight

  }
  //回归函数
  private def lmFunction(lm_X:List[List[Double]], W:List[Double]): List[Double] ={

      lm_X.map(x=>(x,W).zipped.map(_ * _).sum)

  }
  def Inter(Alph:Double=0.0001,a:Double=0.0001):List[Double]={
    this.a = a
      var weight = this.weight

      var weight_1 = LossFunction(this.lm_X,this.lm_Y,weight )

      while ( (weight,weight_1).zipped.map((x,y)=>(x-y).abs).max >Alph  ){

          weight = weight_1
          weight_1 = LossFunction(this.lm_X,this.lm_Y,weight )

      }
      return weight
  }
}



