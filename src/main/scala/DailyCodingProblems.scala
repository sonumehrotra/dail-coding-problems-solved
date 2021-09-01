import scala.annotation.tailrec

object DailyCodingProblems {

  /**
   * #560
   * Given a list of numbers and a number k, return whether any two numbers from the list add up to k.
     For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.
   */
  @tailrec
  def listSumsToThis(number: Int, ints: List[Int]): Boolean = ints match {
    case Nil => false
    case h :: tail => if (tail.map(_ + h).contains(number)) true else listSumsToThis(number, tail)
  }

  def listSumsToThisFunctionalSolution(number: Int, ints: List[Int]): Boolean = ints.combinations(2).exists(_.sum == number)

  /**
   * #854
   * Given a string s and an integer k, break up the string into multiple lines such that each line has a length of k or less.
   * You must break it up so that words don't break across lines. Each line has to have the maximum possible amount of words. If there's no way to break the text up, then return null.
   * You can assume that there are no spaces at the ends of the string and that there is exactly one space between each word.
   * For example, given the string "the quick brown fox jumps over the lazy dog" and k = 10, you should return: ["the quick", "brown fox", "jumps over", "the lazy", "dog"].
   * No string in the list has a length of more than 10.
   */
  def breakTheString(string: String, k: Int): List[String] = {
    @tailrec
    def mkListWithAllWordsLessThanKLength(all: List[String], broken: List[String], makingSentence: String = ""): List[String] = all match {
      case Nil                                  => broken :+ makingSentence
      case h :: _ if h.length > k               => Nil
      case h :: tail if makingSentence.isEmpty  => mkListWithAllWordsLessThanKLength(tail, broken, h)
      case h :: tail                            =>
        if ((makingSentence + " " + h).length <= k) {
          mkListWithAllWordsLessThanKLength(tail, broken, makingSentence + " " + h)
        } else {
          mkListWithAllWordsLessThanKLength(tail, broken :+ makingSentence, h)
        }
    }

    mkListWithAllWordsLessThanKLength(string.split(" ").toList, Nil)

  }

}
