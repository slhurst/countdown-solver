package countdown

import scala.util.Random

object Solver extends App {

  val bigs = List(25, 50, 75, 100)
  val smalls = List.range(1, 11) ++ List.range(1, 11)

  val list = List()
  implicit def intToString(i: Int) = i.toString()

  //  println(smalls.sortWith(_>_))
  val numbers = selectNumbers(1)
  val range = 100 to 999
  val target = range(new Random().nextInt(range.length))
  //  val target = 108
  println("Target: " + target)
  println("Numbers: " + numbers)
  //  val solution = solve(List(100), List(5, 2, 9, 1, 8), target)
  val solution = solve(numbers._1, numbers._2, target)
  println(solution)

  def solve(bigs: List[Int], smalls: List[Int], target: Int): Option[Solution] = {
    var solutions = List[Solution]()

    for (big <- bigs)
      for (s <- smalls if isItClose(s, big, smalls diff List(s), target)) {
        val others = smalls diff List(s)
        val mult = big * s
        val n = others.reduce((a: Int, b: Int) => nearest(mult + a, mult + b, target)) - mult
        solutions = new Solution(mult + n, big + "*" + s + "+" + n) :: solutions
      }
    return solutions.sortWith((a: Solution, b: Solution) => math.abs(target - b.x) > math.abs(target - a.x)).headOption
  }

  def nearest(a: Int, b: Int, target: Int): Int = {
    if (math.abs(target - b) > math.abs(target - a))
      return a
    return b
  }

  def isItClose(s: Int, b: Int, others: List[Int], target: Int): Boolean = {
    val max = others.sortWith(_ > _).take(2).reduce(_ * _)
    val mult = s * b
    return mult < target + max && mult > target - max
  }

  def selectNumbers(numOfBigs: Int): (List[Int], List[Int]) = {
    return (select(numOfBigs, bigs), select(6 - numOfBigs, smalls))
  }

  def select(n: Int, originals: List[Int]): List[Int] = {
    var bag = originals
    var result = List[Int]()
    val rand = new Random()
    for (x <- 1 to n) {
      val selected = bag(rand.nextInt(bag.length))
      result = selected :: result
      //      selected :: result
      bag = bag diff List(selected)
    }

    return result
  }
}