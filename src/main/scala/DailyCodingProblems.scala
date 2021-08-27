import scala.annotation.tailrec

object DailyCodingProblems {

  /**
   * #560
   * Given a list of numbers and a number k, return whether any two numbers from the list add up to k.
     For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.
   */
  @tailrec
  def listSimsToThis(number: Int, ints: List[Int]): Boolean = ints match {
    case Nil => false
    case h :: tail => if (tail.map(_ + h).contains(number)) true else listSimsToThis(number, tail)
  }

  def listSumsToThisFunctionalSolution(number: Int, ints: List[Int]): Boolean = ints.combinations(2).exists(_.sum == number)


}
