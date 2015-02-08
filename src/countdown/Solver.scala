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
        val mult = big * s
        var currentSolution = new Solution(mult, big+"*"+s)
        possibles(smalls diff List(s)).foreach { (k:(Int, String)) => 
          if (nearer(mult+k._1, currentSolution.value, target))
            currentSolution = new Solution(mult+k._1, currentSolution.method+"+"+k._2)
          if (nearer(mult-k._1, currentSolution.value, target))
            currentSolution = new Solution(mult-k._1, currentSolution.method+"-"+k._2)
        }
//        val n = others.reduce((a: Int, b: Int) => nearest(mult + a, mult + b, target)) - mult
        solutions = currentSolution :: solutions
      }
    println(solutions)
    solutions.sortWith((a: Solution, b: Solution) => math.abs(target - b.value) > math.abs(target - a.value)).headOption
  }

  def nearer(a: Int, b: Int, target: Int): Boolean = {
    math.abs(target - b) > math.abs(target - a)
  }
  
  //TODO: return unused, apply recursion
  def possibles(entries:List[Int]) : Map[Int,String] = {
    var result = entries map (a => a -> a.toString()) toMap;
    for (e <- entries) {
      result = result ++ (entries diff List(e)).map(b => b+e -> getString(b,"+",e)).toMap 
      result = result ++ (entries diff List(e)).map(b => b-e -> getString(b,"-",e)).toMap
      result = result ++ (entries diff List(e)).map(b => b*e -> getString(b,"*",e)).toMap
      //TODO: think about division...
//      result = (entries diff List(e)).map(b => b+e -> getString(b,"+",e)).toMap
    }
    println(result)
    return result
  }
  
  def getString(a:(String, String, String)) : String = {
    "("+a._1+a._2+a._3+")"
  }

  def isItClose(s: Int, b: Int, others: List[Int], target: Int): Boolean = {
    val max = others.sortWith(_ > _).take(2).reduce(_ * _)
    val mult = s * b
    mult < target + max && mult > target - max
  }

  def selectNumbers(numOfBigs: Int): (List[Int], List[Int]) = {
    (select(numOfBigs, bigs), select(6 - numOfBigs, smalls))
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