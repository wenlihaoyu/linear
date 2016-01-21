import src.linear._
object A extends App {

  import java.util.Random

  val random = new Random(2)
  val X = List.range(1, 100).map(x => List(random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble))
  val Y = List.range(1, 100).map(x => random.nextDouble)
  val lin = new linear(X, Y)
  print(lin.Inter(a=0.001))
}