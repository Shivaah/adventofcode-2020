package adventofcode.day3

import scala.io.Source

case class Point(x : Int, y : Int) {
  def translate(dx : Int, dy : Int): Point = Point(x + dx, y + dy)
}

object TobogganTrajectory {

  def getPart1(input : Seq[String]) : Int = 0 // TODO
  def getPart2(input : Seq[String]) : Int = 0 // TODO

  def main(args: Array[String]): Unit = {
    val bufferedResource = Source.fromResource("day3.txt")
    val input = bufferedResource.getLines().toSeq
    bufferedResource.close()

    val map = input.zipWithIndex.map {
      case (line, lineIndex) => (lineIndex, line.zipWithIndex)
    } flatMap {
      case (lineIndex, vec) =>
        vec map {
          case (char, charIndex) => (Point(charIndex, lineIndex), char)
        }
    } toMap

    println(getPart1(input))
    println(getPart2(input))
  }

}
