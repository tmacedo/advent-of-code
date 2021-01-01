package aoc.day15

import scala.annotation.tailrec

object App {

  def main(args: Array[String]): Unit = {
    val input = List(7, 14, 0, 17, 11, 1, 2)

    val curr = input.last
    // except the last
    val m = input.reverse.tail.reverse.zipWithIndex.toMap

    println(iter(m, curr, m.size, 2020))
    println(iter(m, curr, m.size, 30000000))
  }

  @tailrec
  def iter(m: Map[Int, Int], curr: Int, counter: Int, max: Int): Int = {
    if(counter + 1 == max)
      curr
    else {
      val newNumber = m.get(curr) match {
        case Some(n) => counter - n
        case None => 0
      }

      iter(m.updated(curr, counter), newNumber, counter + 1, max)
    }
  }
}
