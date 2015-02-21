package countdown

import scala.util.Random
import scala.collection.immutable.Queue

object Solver {

  val bigs = Seq(25, 50, 75, 100)
  val smalls = Seq.range(1, 11) ++ Seq.range(1, 11)

  implicit def intToString(i: Int) = i.toString()

  def main(args: Array[String]): Unit = {
    //  println(smalls.sortWith(_>_))
    val numbers = selectNumbers(1)
    val range = 100 to 999
    val target = range(new Random().nextInt(range.length))
    //  val target = 108
    println("Target: " + target)
    println("Numbers: " + numbers)
    val solution = solve(Seq(100), Seq(5, 2, 9, 1, 8), target)
    //  val solution = solve(numbers._1, numbers._2, target)
    println(solution)

  }

  def solve(bigs: Seq[Int], smalls: Seq[Int], target: Int): Option[Solution] = {
    var solutions = Seq[Solution]()

    for (big <- bigs)
      for (s <- smalls if isItClose(s, big, smalls diff Seq(s), target)) {
        val mult = big * s
        var currentSolution = new Solution(mult, big + "*" + s)
        //        possibles(smalls diff Seq(s)).foreach { (k:(Int, String)) => 
        //          if (nearer(mult+k._1, currentSolution.value, target))
        //            currentSolution = new Solution(mult+k._1, currentSolution.method+"+"+k._2)
        //          if (nearer(mult-k._1, currentSolution.value, target))
        //            currentSolution = new Solution(mult-k._1, currentSolution.method+"-"+k._2)
        //        }
        //        val n = others.reduce((a: Int, b: Int) => nearest(mult + a, mult + b, target)) - mult
        solutions = solutions :+ currentSolution
      }
    println(solutions)
    solutions.sortWith((a: Solution, b: Solution) => math.abs(target - b.value) > math.abs(target - a.value)).headOption
  }

  def nearer(a: Int, b: Int, target: Int): Boolean = {
    math.abs(target - b) > math.abs(target - a)
  }

  //TODO: return unused, apply recursion, key on unused: Map[Seq?[Int], Map[Int, String]]
  def possibles(entries: Seq[Int]): Map[Set[Int], Map[Int, String]] = {
    var m = Map[Set[Int], Map[Int, String]]()

    for (e <- entries) {
      val leftovers = entries diff Seq(e)
      var result = entries map (a => a -> a.toString()) toMap;
      result = result ++ leftovers.map(b => b + e -> getString(b, "+", e)).toMap
      result = result ++ leftovers.map(b => b - e -> getString(b, "-", e)).toMap
      result = result ++ leftovers.map(b => b * e -> getString(b, "*", e)).toMap
      //      result = result ++ leftovers
      //                            .filter(_%e==0)
      //                            .map(b => b/e -> getString(b,"/",e)).toMap
    }
    return m
  }

  def getString(a: (String, String, String)): String = {
    "(" + a._1 + a._2 + a._3 + ")"
  }

  def isItClose(s: Int, b: Int, others: Seq[Int], target: Int): Boolean = {
    val max = others.sortWith(_ > _).take(2).reduce(_ * _)
    val mult = s * b
    mult < target + max && mult > target - max
  }

  def selectNumbers(numOfBigs: Int): (Seq[Int], Seq[Int]) = {
    (select(numOfBigs, bigs), select(6 - numOfBigs, smalls))
  }

  def select(n: Int, originals: Seq[Int]): Seq[Int] = {
    var bag = originals
    var result = Seq[Int]()
    val rand = new Random()
    for (x <- 1 to n) {
      val selected = bag(rand.nextInt(bag.length))
      result = result :+ selected
      bag = bag diff Seq(selected)
    }

    return result
  }
}