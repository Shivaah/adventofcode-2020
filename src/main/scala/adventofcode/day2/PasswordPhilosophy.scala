package adventofcode.day2

import scala.io.Source

case class PasswordPolicy(min : Int, max : Int, letter : Char) {

  def validateWithCount(password : String) : Boolean = {
    val result = password.filter(x => x == letter)

    result.length >= min && result.length <= max
  }

  def validateWithPositions(password : String) : Boolean = {
    val candidates = password.charAt(min - 1) :: password.charAt(max - 1) :: Nil

    areRightLetters(candidates) || areRightLetters(candidates.reverse)
  }

  private def areRightLetters(candidates : Seq[Char]) : Boolean =
    candidates.head == letter && candidates.last != letter

}

object PasswordPolicy {

  def from(rawPolicy : String) : PasswordPolicy = {
    val result = rawPolicy.split(" ")
    val (min, max) = result(0).split('-') match { case Array(f1, f2) => (f1.toInt, f2.toInt) }
    val letter = result(1).charAt(0)

    PasswordPolicy(min, max, letter)
  }

}

object PasswordPhilosophy {

  def parse(entry : String) : List[String] = {
    val result = entry.split(':')

    val formattedPasswordPolicy = result(0)
    val trimPassword = result(1).trim

    formattedPasswordPolicy :: trimPassword :: Nil
  }

  def getPasswordAndPolicyFrom(entry : String) : (String, PasswordPolicy) = {
    val parsedEntry = parse(entry)
    val password = parsedEntry.last
    val policy = PasswordPolicy.from(parsedEntry.head)

    (password, policy)
  }

  def getPart1(input : Seq[String]) : Int = {
    val result = input.filter {
      x => {
        val (password, policy) = getPasswordAndPolicyFrom(x)
        policy.validateWithCount(password)
      }
    }

    result.length
  }

  def getPart2(input : Seq[String]) : Int = {
    val result = input.filter {
      x => {
        val (password, policy) = getPasswordAndPolicyFrom(x)
        policy.validateWithPositions(password)
      }
    }

    result.length
  }

  def main(args: Array[String]): Unit = {
    val bufferedResource = Source.fromResource("day2.txt")
    val input = bufferedResource.getLines().toSeq
    bufferedResource.close()
0
    println(getPart1(input))
    println(getPart2(input))
  }

}
