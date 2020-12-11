package adventofcode.day5

import scala.annotation.tailrec
import scala.io.Source

object BinaryBoarding {
  type SearchPattern = (Char, Char)

  def halfRegion(pattern: SearchPattern)(range : Range, letter: Char): Range = {
    val (forUpperHalf, forLowerHalf) = pattern

    letter match {
      case `forUpperHalf` => range.start + (range.length / 2) to range.end
      case `forLowerHalf` => range.start to range.end - (range.length / 2)
      case _ => range
    }
  }

  @tailrec
  def find(pattern: SearchPattern)(range : Range, displacement: List[Char]): Option[Int] = {
    val (forUpperHalf, forLowerHalf) = pattern

    displacement match {
      case Nil => None
      case head :: Nil => head match {
        case `forUpperHalf` => Some(range.end)
        case `forLowerHalf` => Some(range.start)
        case _ => None
      }

      case head :: tail => find(pattern)(halfRegion(pattern)(range, head), tail)
    }
  }

  def row(range: Range, displacement: List[Char]): Option[Int] = find('B', 'F')(range, displacement)
  def column(range: Range, displacement: List[Char]): Option[Int] = find('R', 'L')(range, displacement)

  def getPart1(input : Seq[String]): Int = {
    val rows = 0 to 127
    val columns = 0 to 7

    input.map(x => (x.slice(0, 7).toList, x.slice(7, input.head.length).toList))
      .map(x => row(rows, x._1).getOrElse(0) * 8 + column(columns, x._2).getOrElse(0))
      .max
  }

  def main(args: Array[String]): Unit = {
    val bufferedResource = Source.fromResource("day5.txt")
    val input = bufferedResource.getLines().toList
    bufferedResource.close()

    println(getPart1(input))
  }
}
