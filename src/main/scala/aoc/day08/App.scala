package aoc.day08

import aoc.shared.Reader

sealed trait Instruction

case class Jmp(n: Int) extends Instruction
case class Noop(n: Int) extends Instruction
case class Acc(n: Int) extends Instruction

sealed trait Termination
case class Loop(n: Int) extends Termination
case class Success(n: Int) extends Termination

object App {
  def main(args: Array[String]): Unit = {

    val input = Reader.lines("08.txt")

    val program = read(input)

    val res = execute(program, 0, 0, Set.empty[Int])

    println(res)

    val indicesToChange = program.zipWithIndex.filter(i => instructionToChange(i._1)).map(_._2)

    val runs = indicesToChange.map { i =>
      val newCode = change(program, i)
      execute(newCode, 0, 0, Set.empty[Int])
    }

    println(runs.filter(_.isInstanceOf[Success]))

  }

  def read(input: List[String]): List[Instruction] =
    input.map(s =>
      s.split(" ").toList match {
        case "nop" :: c :: Nil => Noop(c.toInt)
        case "acc" :: c :: Nil => Acc(c.toInt)
        case "jmp" :: c :: Nil => Jmp(c.toInt)
      }
    )

  def execute(code: List[Instruction], line: Int, acc: Int, visitedLines: Set[Int]): Termination = {
    if(line == code.size)
      Success(acc)
    else {
      val instruction = code(line)

      val (newLine, newAcc) = instruction match {
        case Noop(_) => (line + 1, acc)
        case Acc(n) => (line + 1, acc + n)
        case Jmp(n) => (line + n, acc)
      }

      if (visitedLines.contains(newLine))
        Loop(newAcc)
      else {
        val newVisitedLines = visitedLines + newLine

        execute(code, newLine, newAcc, newVisitedLines)
      }
    }
  }

  def change(code: List[Instruction], line: Int): List[Instruction] = {
    val instruction = code(line)
    instruction match {
      case Jmp(n) => code.updated(line, Noop(n))
      case Noop(n) => code.updated(line, Jmp(n))
    }

  }

  def instructionToChange(i: Instruction): Boolean =
    i match {
      case Noop(_) => true
      case Jmp(_) => true
      case _ => false
    }
}
