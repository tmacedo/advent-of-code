package aoc.day02

import aoc.shared.Reader

case class Problem(range: (Int, Int), char: Char, password: String)

object App {
  def main(args: Array[String]): Unit = {

    val input = Reader.lines("02.txt")

    val data = input.map(s => {
        val parts = s.split(" ").toList
        val numbers = parts(0).split("-").toList.map(_.toInt)

        val char = parts(1).charAt(0)
        val password = parts(2)

        Problem((numbers(0), numbers(1)), char, password)
      }
    )

    val valid = data.filter(p => {
      val count = p.password.count(_ == p.char)
      count >= p.range._1 && count <= p.range._2
    })

    println(valid.size)

    val validPart2 = data.filter(p => {
      val newS = List(
        p.password.charAt(p.range._1 - 1),
        p.password.charAt(p.range._2 - 1)
      ).mkString

      newS.count(_ == p.char) == 1
    })

    println(validPart2.size)
  }
}
