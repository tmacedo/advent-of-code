package aoc.day17

import aoc.shared.Reader

object App {
  def main(args: Array[String]): Unit = {
    val input = Reader.lines("17.txt")

    val mapped = input
      .map(_.toCharArray.toList.zipWithIndex)
      .zipWithIndex
      .flatMap(row => row._1.map(column => (column._1, (row._2, column._2))))

    val filtered = mapped.filter(_._1 == '#')
    val active = filtered.map(c => (c._2._1, c._2._2, 0))

    println(loop(active, neighbours, 6).size)

    val active4d = filtered.map(c => (c._2._1, c._2._2, 0, 0))
    println(loop(active4d, neighbours4d, 6).size)
  }

  def loop[A](active: List[A], fNeighbours: A => List[A], max: Int): List[A] =
    (0 until max).foldLeft(active)( (a, _) =>
      iter(a, fNeighbours)
    )

  val RemainActive = List(2,3)
  def iter[A](active: List[A], fNeighbours: A => List[A]): List[A] = {
    val remainActive = active.filter(p => {
      val n = fNeighbours(p)
      val activeCount = n.intersect(active).size

      RemainActive.contains(activeCount)
    })

    val allInactiveNeighbours = active
      .flatMap(fNeighbours)
      .distinct
      .filterNot(active.contains)

    val becomeActive = allInactiveNeighbours.filter(p => {
      val n = fNeighbours(p)
      val activeCount = n.intersect(active).size

      activeCount == 3
    })

    remainActive.concat(becomeActive)
  }

  val NeighbourRange = -1 to 1
  def neighbours(point: (Int, Int, Int)): List[(Int, Int, Int)] = {
    (for {
      x <- NeighbourRange.map(i => point._1 + i)
      y <- NeighbourRange.map(i => point._2 + i)
      z <- NeighbourRange.map(i => point._3 + i)
    } yield (x,y,z)
      ).filterNot(_ == point).toList
  }

  def neighbours4d(point: (Int, Int, Int, Int)): List[(Int, Int, Int, Int)] = {
    (for {
      x <- NeighbourRange.map(i => point._1 + i)
      y <- NeighbourRange.map(i => point._2 + i)
      z <- NeighbourRange.map(i => point._3 + i)
      w <- NeighbourRange.map(i => point._4 + i)
    } yield (x,y,z,w)
      ).filterNot(_ == point).toList
  }
}