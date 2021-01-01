package aoc.day10

import aoc.shared.Reader

object App {

  def main(args: Array[String]): Unit = {

    val input = Reader.lines("10.txt")

    val numbers = input.map(_.toLong)

    val sorted = numbers.sorted

    val paired = diffs(sorted)

    val diff = paired.map(p => p._2 - p._1)

    println((diff.filter(_ == 1).size + 1) * (diff.filter(_ == 3).size + 1))

    val r = (1 to sorted.size)

    val deviceRating = sorted.last + 3

    println(part2(sorted, deviceRating))
  }

  def diffs(list: List[Long]): List[(Long, Long)] = {
    list.zip(list.tail)
  }

  def part2(list: List[Long], target: Long): Long = {
    val m = list.sorted.reverse.foldLeft(Map[Long, Long]())((m, i) => {
      val add = if (target <= i + 3)
        1
      else
        0

      val closeEnough = list.filter(n => n > i && n <= i + 3)
      val v = add + closeEnough.map(n => m.get(n).get).sum

      m.updated(i, v)
    })

    list.filter(n => n <= 3).map(n => m.get(n).get).sum
  }
}
