import scala.util.Random

trait Utils {
  // generate array of n length with random values [0, Int.MaxValue)
  def generateRandArrayInt(n: Int = 50000): Array[Int] = generateRandArrayInt(n, Int.MaxValue)

  def generateRandArrayInt(n: Int, maxValue: Int): Array[Int] = Array.fill(n)(Random.nextInt(maxValue))
}
