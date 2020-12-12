package adventofcode.day6

import scala.io.Source

object CustomCustoms {
  val EMPTY_STRING = ""
  val CARRIAGE_RETURN = "\n"
  val SPACE_STRING = " "

  def getPart1(input: Seq[String]): Int = {
    input.map(x => if (x == EMPTY_STRING) CARRIAGE_RETURN else x)
      .reduce((a, b) => a concat SPACE_STRING concat b)
      .split("\n")
      .map(x => x.split(" "))
      .map(x => x.reduce {
        (a, b) =>
          if (a.length > b.length) b.concat(a.diff(b))
          else a.concat(b.diff(a))
        }
      )
      .map(x => x.length)
      .sum
  }

  def main(args: Array[String]): Unit = {
    val bufferedResource = Source.fromResource("day6.txt")
    val input = bufferedResource.getLines().toList
    bufferedResource.close()

    println(getPart1(input))
  }
}
