package countdown

class Solution(val value: Int, val method: String) extends Equals {
  override def toString() = {
    "[answer: "+value+", method: "+method+"]"
  }
  
  def canEqual(other: Any) = {
    other.isInstanceOf[countdown.Solution]
  }

  override def equals(other: Any) = {
    other match {
      case that: countdown.Solution => that.canEqual(Solution.this) && value == that.value && method == that.method
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + value.hashCode) + method.hashCode
  }
}