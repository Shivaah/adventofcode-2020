package adventofcode.day2

import scala.io.Source

case class PasswordPolicy(min : Int, max : Int, letter : Char) {

  def validate(password : String) : Boolean = {
    val result = password.filter(x => x == letter)

    result.length >= min && result.length <= max
  }

}

object PasswordPolicy {

  def createFromRaw(rawPolicy : String) : PasswordPolicy = {
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

  def getPart1(input : Seq[String]) : Int = {
    val result = input.filter {
      x => {
        val parsedEntry = parse(x)
        val password = parsedEntry.last
        val policy = PasswordPolicy.createFromRaw(parsedEntry.head)

        policy.validate(password)
      }
    }

    result.length
  }

  def main(args: Array[String]): Unit = {
    val bufferedResource = Source.fromResource("day2.txt")
    val input = bufferedResource.getLines().toSeq
    bufferedResource.close()
    
    println(getPart1(input))
  }

}
