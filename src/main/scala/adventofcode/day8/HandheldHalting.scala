package adventofcode.day8

import scala.annotation.tailrec
import scala.io.Source

sealed trait Instruction[+A]
case object NoOperation extends Instruction[Nothing]
case class Accumulation[Int](value: Int) extends Instruction[Int]
case class Jump[Int](offset: Int) extends Instruction[Int]

object Parser {
  def parse(input : List[String]) : List[Instruction[Int]] = {
    for(element <- input)
      yield element.split(" ").toList match {
        case List(a, b) => a match {
          case "nop" => NoOperation
          case _ => {
            val operator = b.charAt(0)
            val value = b.slice(1, b.length).toInt
            val operationValue = if (operator == '+') value else -1 * value

            if (a == "acc") Accumulation(operationValue)
            else Jump(operationValue)
          }
        }
      }

  }
}

object HandheldHalting {
  type Instructions = List[Instruction[Int]]

  /** Execute instruction by giving a read head index and the instructions list.
   * The returned value depends of the instruction's type and is a 2-uple which contains
   * the evaluation of the instruction and the next read head index respectively. */
  def execute(head: Int, instructions: Instructions): (Int, Int) = instructions(head) match {
    case NoOperation => (head + 1, 0)
    case Accumulation(value) => (head + 1, value)
    case Jump(offset) => (head + offset, 0)
  }

  def runUntilLoop(instructions: Instructions): Int = {
    @tailrec
    def seqExec(head : Int, accumulated : Int, executed : List[Int]) : Int = {
      if (executed.contains(head)) accumulated
      else {
        val (h, value) = execute(head, instructions);
        seqExec(h, accumulated + value, executed :+ head)
      }
    }

    seqExec(0, 0, Nil)
  }

  def getPart1(input: List[String]): Int = {
    val instructions = Parser.parse(input)

    runUntilLoop(instructions)
  }

  def main(args: Array[String]): Unit = {
    val bufferedResource = Source.fromResource("day8.txt")
    val input = bufferedResource.getLines().toList
    bufferedResource.close()

    println(getPart1(input))
  }
}
