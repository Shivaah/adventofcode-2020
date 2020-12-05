package adventofcode.day1

import scala.annotation.tailrec
import scala.io.Source

object ReportRepair {
  @tailrec
  def find(source : Seq[Int])(element : Int) : Option[Int] = source match {
      case n :: _ if n == element => Some(n)
      case _ :: tail => find(tail)(element)
      case Nil => None
  }

  @tailrec
  def findTwoElementsSumEqualTo(source : Seq[Int])(desiredEquality : Int) : Option[(Int, Int)] = find(source)(desiredEquality - source.head) match {
    case None => source match {
      case Nil => None
      case _ :: tail => findTwoElementsSumEqualTo(tail)(desiredEquality)
    }

    case Some(y) => Some(desiredEquality - y, y)
  }

  def getPart1(content : Seq[Int]) : Int = {
    val result = findTwoElementsSumEqualTo(content)(2020).get
    result._1 * result._2
  }

  def main(args: Array[String]): Unit = {
    val bufferedResource = Source.fromResource("day1.txt")
    val input = bufferedResource.getLines().map(_.toInt).toSeq
    bufferedResource.close()

    println(getPart1(input))
  }
}
