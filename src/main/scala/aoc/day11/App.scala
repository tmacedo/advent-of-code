package aoc.day11

import aoc.shared.Reader

import scala.annotation.tailrec

object App {

  def main(args: Array[String]): Unit = {

    val input = Reader.lines("11.txt")

    val m: Map[(Int, Int), Char] = input.zipWithIndex.flatMap(r => {
      r._1.toCharArray.toList.zipWithIndex.map(c =>
        ((r._2, c._2), c._1)
      )
    }).toMap

    val maxX = m.keys.map(_._1).max
    val maxY = m.keys.map(_._2).max

    val part1VisibilityF: (Map[(Int, Int), Char], (Int, Int)) => Int = occupied(maxX, maxY)

    val part1 = run(m, 4, part1VisibilityF)
    println(part1.values.filter(_ == '#').size)

    val part2VisibilityF: (Map[(Int, Int), Char], (Int, Int)) => Int = visible(maxX, maxY)
    val part2 = run(m, 5, part2VisibilityF)
    println(part2.values.filter(_ == '#').size)
  }

  @tailrec
  def run(m: Map[(Int, Int), Char], tolerance: Int, visibilityF: (Map[(Int, Int), Char], (Int, Int)) => Int): Map[(Int, Int), Char] = {
    val newM = iter(m, tolerance, visibilityF)

    if (newM == m)
      newM
    else
      run(newM, tolerance, visibilityF)
  }

  def iter(m: Map[(Int, Int), Char], tolerance: Int, visibilityF: (Map[(Int, Int), Char], (Int, Int)) => Int): Map[(Int, Int), Char] = {

    m.map(e =>
      e._2 match {
        case '.' => e
        case 'L' =>
          if (visibilityF(m, e._1) == 0)
            (e._1, '#')
          else
            e
        case '#' =>
          if (visibilityF(m, e._1) >= tolerance)
            (e._1, 'L')
          else
            e
      }
    )
  }

  def visible(maxX: Int, maxY: Int)(m: Map[(Int, Int), Char], seat: (Int, Int)): Int = {
    val l = lines(maxX, maxY)(seat)

    l
      .map(
        _.map(m.get(_).get)
          .collectFirst {
            case 'L' =>
              0
            case '#' =>
              1
          }
      ).flatten.sum
  }

  def occupied(maxX: Int, maxY: Int)(m: Map[(Int, Int), Char], seat: (Int, Int)): Int = {
    neighbours(seat, maxX, maxY).foldLeft(0)((acc, seat) => {
      val n = m.get(seat) match {
        case Some('#') => 1
        case _ => 0
      }
      acc + n
    })
  }

  def neighbours(seat: (Int, Int), maxX: Int, maxY: Int): List[(Int, Int)] = {
    List(
      (seat._1 - 1, seat._2 - 1),
      (seat._1 - 1, seat._2),
      (seat._1 - 1, seat._2 + 1),
      (seat._1, seat._2 - 1),
      (seat._1, seat._2 + 1),
      (seat._1 + 1, seat._2 - 1),
      (seat._1 + 1, seat._2),
      (seat._1 + 1, seat._2 + 1),
    ).filter(s => s._1 >= 0 && s._1 <= maxX)
      .filter(s => s._2 >= 0 && s._2 <= maxY)
  }

  def lines(maxX: Int, maxY: Int)(seat: (Int, Int)): List[List[(Int, Int)]] = {
    List(
      // up left
      line(maxX, maxY)(seat, (s: (Int, Int)) => (s._1 - 1, s._2 - 1)),
      // up
      line(maxX, maxY)(seat, (s: (Int, Int)) => (s._1 - 1, s._2)),
      // up right
      line(maxX, maxY)(seat, (s: (Int, Int)) => (s._1 - 1, s._2 + 1)),
      // right
      line(maxX, maxY)(seat, (s: (Int, Int)) => (s._1, s._2 + 1)),
      // down right
      line(maxX, maxY)(seat, (s: (Int, Int)) => (s._1 + 1, s._2 + 1)),
      // down
      line(maxX, maxY)(seat, (s: (Int, Int)) => (s._1 + 1, s._2)),
      // down left
      line(maxX, maxY)(seat, (s: (Int, Int)) => (s._1 + 1, s._2 - 1)),
      // left
      line(maxX, maxY)(seat, (s: (Int, Int)) => (s._1, s._2 - 1))
    )
  }

  def line(maxX: Int, maxY: Int)(seat: (Int, Int), f: ((Int, Int)) => (Int, Int)): List[(Int, Int)] = {
    val newS: (Int, Int) = f(seat)

    if (newS._1 >= 0 && newS._1 <= maxX && newS._2 >= 0 && newS._2 <= maxY)
      newS +: line(maxX, maxY)(newS, f)
    else
      List.empty
  }
}
