package aoc.day03

import aoc.shared.Reader

case class Counter(position: Int, trees: Int)

object App {
  def main(args: Array[String]): Unit = {

    val input = Reader.lines("03.txt")

   println(trees(input, 3))

    val everyOtherRow = input.zipWithIndex.collect {
      case (str, i) if i % 2 == 0 => str
    }

    val part2 = List(
        trees(input, 1),
        trees(input, 3),
        trees(input, 5),
        trees(input, 7),
        trees(everyOtherRow, 1)
    )
    println(part2.foldLeft(1L) { (a, b) => a * b})
  }

  def trees(input: List[String], step: Int): Int =
    input.map(_.toCharArray.toList).tail.foldLeft(Counter(0,0)) { (c, list) =>
      val newPos = (c.position + step) % list.size

      val newTrees = if(list(newPos) == '#')
        c.trees + 1
      else
        c.trees

      Counter(newPos, newTrees)
    }.trees
}
