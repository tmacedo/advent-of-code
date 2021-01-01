package aoc.day13

import aoc.shared.Reader

import scala.util.{Success, Try}

object App {

  val NumberPattern = "([0-9]*)".r

  def main(args: Array[String]): Unit = {

    val input = Reader.lines("13.txt")

    val timestamp = input.head.toInt

    val buses = input.tail.head
      .split(",")
      .collect {
        case NumberPattern(n) => n.toLong
      }.toList

    val streams = buses.map(b =>
      (b, stream(b)(b))
    )

    val min = streams.map(b =>
      (b._1, b._2.find(_ >= timestamp))
    )
    val earliestBus = min.minBy(_._2)
    println(earliestBus._1 * (earliestBus._2.get - timestamp))

    val busesWithOffsets = input.tail.head
      .split(",")
      .collect {
        case NumberPattern(n) => Some(n)
        case _ => None
      }
      .zipWithIndex
      .flatMap({
        case (b, i) => b.map(b => (b.toInt, i))
      })
      .map(t => (t._1, basedOffset(t._1, t._2)))
      .toList

    val part2 = chineseRemainder(busesWithOffsets.map(_._1), busesWithOffsets.map(_._2))
    println(part2)
  }

  // first multiple of bus bigger than gap - gap
  // this is for normalising the input to crt
  // as n % b can't be bigger than b
  def basedOffset(bus: Int, gap: Int): Int =
    bus * math.ceil(gap.toFloat / bus).toInt - gap

  def stream(origN: Long)(n: Long): LazyList[Long] =
    LazyList.cons(n, stream(origN)(n + origN))

  // from https://rosettacode.org/wiki/Chinese_remainder_theorem#Scala
  def chineseRemainder(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product

    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def mulInv(a: Long, b: Long): Long = {
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _ => None
    }
  }
}
