package aoc.day09

import aoc.shared.Reader

object App {

  val PreambleSize = 25

  def main(args: Array[String]): Unit = {

    val input = Reader.lines("09.txt")

    val numbers = input.map(_.toLong)

    val numbers2 = numbers.slice(PreambleSize, numbers.size)
    val zipped = numbers.sliding(PreambleSize).zip(numbers2).toList

    val failures = zipped.filter(l => !validate(l._1, l._2))

    val part1 = failures.head._2
    println(part1)

    val contiguous = numbers.zipWithIndex.map(n =>
      numbers.slice(n._2, numbers.size)
    )

    val part2 = contiguous
      .flatMap(l => checksum(l, part1))
      .filter(_.size >= 2)

    val solution = part2.head.min + part2.head.max

    println(solution)
  }

  def validate(numbers: List[Long], checksum: Long): Boolean = {
    numbers.combinations(2).exists(_.sum == checksum)
  }

  def checksum(numbers: List[Long], checksum: Long): List[List[Long]] = {
    val n2 = numbers.zipWithIndex.map(n =>
      numbers.slice(0, n._2 + 1)
    )

    n2.filter(_.sum == checksum)
  }
}
