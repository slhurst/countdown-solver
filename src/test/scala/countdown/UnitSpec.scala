package countdown

import org.scalatest._

class UnitSpec extends FlatSpec {
  
  "1" must "result in 1 returning, keyed on empty list" in {
    assert(Solver.possibles(Seq(1)) == Map[Seq[Int], Map[Int, String]](Seq() -> Map(1 -> "1")))
  }
  
  "(1,1)" must "result in (1 -> 1), (() -> 1), (() -> 2)" in {
    assert(Solver.possibles(Seq(1, 1)) == Map[Seq[Int], Map[Int, String]](Seq() -> Map(1 -> "1"),
        Seq() -> Map(2 -> "1+1"),
        Seq(1) -> Map(1 -> "1")))
    
  }

}