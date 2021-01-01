package aoc.day06

import aoc.shared.Reader

object App {
  def main(args: Array[String]): Unit = {
    val groups = Reader.groups("06.txt")

    val groupChars: List[List[Char]] = groups.map(_.flatMap(_.toCharArray.toList))
    println(groupChars.map(_.distinct.size).sum)

    val answers: List[List[List[Char]]] = groups.map(_.map(_.toCharArray.toList))

    val commonAnswers = answers.map(intersections)
    println(commonAnswers.map(_.size).sum)
  }

  def intersections(l: List[List[Char]]): List[Char] = {
    l.tail.foldLeft(l.head) { (list, acc) =>
      list.intersect(acc)
    }
  }
}
