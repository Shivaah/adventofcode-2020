package adventofcode.day3

import scala.io.Source

case class AirportMap(map : Map[(Int, Int), Char], rightUpperBound : Int, bottomUpperBound : Int)

object AirportMap {
  def apply(input : Seq[String]): AirportMap = {
    val map : Map[(Int, Int), Char] = input.zipWithIndex.map {
      case (line, lineIndex) => (lineIndex, line.zipWithIndex)
    } flatMap {
      case (lineIndex, vec) =>
        vec map {
          case (char, charIndex) => ((charIndex, lineIndex), char)
        }
    } toMap

    new AirportMap(map, input.head.length, input.length - 1)
  }
}

object TobogganTrajectory {
  /** Return a sequence of positions that represents the path
   * which was used to reach the lower bottom upped bound of the airport
   * by successively translating the top left point */
  def getPath(translation : (Int, Int),  airport: AirportMap) : Seq[(Int, Int)] = {
    val (x, y) = translation
    val AirportMap(_, rightLimit, bottomLimit) = airport

    for(i <- 0 to bottomLimit / y)
      yield ((x*i) % rightLimit, y*i)
  }

  /** Count trees by giving a translation vector and the airport */
  def countTrees(translation : (Int, Int), airport: AirportMap) : Long = {
    val AirportMap(map, _, _) = airport

    val path = getPath(translation, airport)

    path map { x => map(x) } count { x => x == '#' }
  }

  def getPart1(airport : AirportMap) : Long = {
    val translation = (3, 1)

    countTrees(translation, airport)
  }

  def getPart2(airport : AirportMap) : Long = {
    val translations = (1, 1) :: (3, 1) :: (5, 1) :: (7, 1) :: (1, 2) :: Nil

    translations.map(x => countTrees(x, airport)).product
  }

  def main(args: Array[String]): Unit = {
    val bufferedResource = Source.fromResource("day3.txt")
    val input = bufferedResource.getLines().toSeq
    bufferedResource.close()

    val map = AirportMap(input)

    println(getPart1(map))
    println(getPart2(map))
  }
}
