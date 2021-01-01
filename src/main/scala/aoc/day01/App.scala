package aoc.day01

import aoc.shared.Reader

object App {
  def main(args: Array[String]): Unit = {

    val input = Reader.lines("01.txt")
      .map(n => n.toInt)

    val numbers = input.combinations(2)

    val pair = find(numbers)

    println(multiply(pair))

    val numbersPart2 = input.combinations(3)

    val three = find(numbersPart2)

    println(multiply(three))
  }

  def find(numbers: Iterator[List[Int]]): List[Int] =
    numbers.collectFirst {
      case l if l.sum == 2020 =>
        l
    }.get

  def multiply(numbers: List[Int]): Int =
    numbers.fold(1) { (a, b) =>
      a * b
    }
}
