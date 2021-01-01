package aoc.day23

import scala.annotation.tailrec

case class Node(v: Int) {
  var next: Node = null
}

object App {
  def main(args: Array[String]): Unit = {
    val input = List(3,6,4,2,9,7,5,8,1).toVector

    val part1 = round(input, 0, 0, 100)
    println(ordered(part1._1).mkString(""))

    val r = Range(10, 1_000_000).inclusive
    val part2Input = input ++ r.toVector
    println(part2(part2Input))
  }

  def part2(input: Vector[Int]): Long = {
    val nodes = input
      .map(Node(_))

    val m = scala.collection.mutable.Map.empty[Int, Node]
    val linked = nodes
      .zip(nodes.drop(1) ++ nodes.take(1))
      .map(t => {
        t._1.next = t._2
        m.update(t._1.v, t._1)
        t._1
      })

    val max = input.max
    var curr: Node = null
    (1 to 10_000_000).foreach( _ => {
      curr = Option(curr).map(_.next).getOrElse(linked.head)

      val pickup = List(
        curr.next,
        curr.next.next,
        curr.next.next.next
      )
      curr.next = pickup.last.next

      val destination = m.get(nextNumber(curr.v, pickup.map(_.v), max)).get
      pickup.last.next = destination.next
      destination.next = pickup.head
    })

    val n = m.get(1).get.next

    n.v.toLong * n.next.v.toLong
  }

  def ordered(input: Vector[Int]): Vector[Int] = {
    val pos1 = input.indexOf(1)
    input.slice(pos1 + 1, input.size) ++ input.slice(0, pos1)
  }

  @tailrec
  def round(input: Vector[Int], index: Int, count: Int = 0, max: Int): (Vector[Int], Int) = {
    if (count == max)
      (input, index)
    else {
      val first = input(index)

      val pickupRight = input.slice(index + 1, index + 4)
      val pickupLeft = input.take(3 - pickupRight.size)
      val pickup = pickupRight ++ pickupLeft

      val restRight = input.slice(index + 4, input.size)
      val restLeft = input.slice(pickupLeft.size, index)
      val unchanged = restLeft ++ List(first) ++ restRight

     val nextPos = nextPosition(first, unchanged, pickup, input.size)

      val split = unchanged.splitAt(nextPos + 1)
      val newList = split._1 ++ pickup ++ split._2

      val newPostIndex = if(nextPos < index)
        index + 4
      else
        index + 1
      val newIndex = if(newPostIndex >= input.size)
        0
      else
        newPostIndex

      round(newList, newIndex, count + 1, max)
    }
  }

  @tailrec
  def nextPosition(first: Int, l: Vector[Int], pickup: Vector[Int], maxSize: Int): Int = {
    val curr = first - 1

    val possibleMaxes = List(
      maxSize,
      maxSize - 1,
      maxSize - 2,
      maxSize - 3
    )

    val v = pickup.indexOf(curr)

    if(curr <= 0) {
      val max = possibleMaxes.filterNot(m => pickup.contains(m)).max
      return l.indexOf(max)
    }

    if(v == -1)
      l.indexOf(curr)
    else
      nextPosition(curr, l, pickup, maxSize)
  }

  @tailrec
  def nextNumber(curr: Int, pickup: List[Int], maxSize: Int): Int = {
    val target = curr - 1

    if(target <= 0)
      return maxSize

    if(pickup.contains(target))
      nextNumber(target, pickup, maxSize)
    else
      target
  }
}