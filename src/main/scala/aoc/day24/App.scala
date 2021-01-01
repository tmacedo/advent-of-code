package aoc.day24

import aoc.shared.Reader

sealed trait Direction {
  val movement: (Int, Int)

  def move(tile: (Int, Int)): (Int, Int) =
    (tile._1 + movement._1, tile._2 + movement._2)
}
case object East extends Direction {
  val movement = (0, 2)
}
case object Southeast extends Direction {
  val movement = (1, 1)
}
case object Southwest extends Direction {
  val movement = (1, -1)
}
case object West extends Direction {
  val movement = (0, -2)
}
case object Northwest extends Direction {
  val movement = (-1, -1)
}
case object Northeast extends Direction {
  val movement = (-1, 1)
}

object App {
  def main(args: Array[String]): Unit = {

    val input = Reader.lines("24.txt")

    val tiles = input.map(s => parse(s.toCharArray.toList))

    val reducedTiles = tiles.map(_.foldLeft((0,0))( (a, b) =>
      b.move(a)
    ))

    val grouped = reducedTiles.groupBy(identity)
    val blackTiles = grouped.filter(_._2.size % 2 == 1).keys.toList
    println(blackTiles.size)

    val part2 = (1 to 100).foldLeft(blackTiles)( (tiles, _) =>
      day(tiles)
    )

    println(part2.size)
  }

  def day(blackTiles: List[(Int, Int)]): List[(Int, Int)] = {
    val allTiles = blackTiles.flatMap(neighbours).distinct

    allTiles.filter(t => {
      val isBlack = blackTiles.contains(t)
      val blackNeighbourCount = neighbours(t).filter(blackTiles.contains).size

      (isBlack, blackNeighbourCount) match {
        case (true, c) if c == 0 || c > 2 => false // goes white if 0 or more than 2 neighbours are black
        case (true, _) => true // stays black otherwise
        case (false, 2) => true // goes black if it has 2 black neighbours
        case (false, _) => false // stays white otherwise
      }
    })
  }

  def parse(chars: List[Char]): List[Direction] = {
    chars match {
      case 's' :: 'e' :: Nil  => List(Southeast)
      case 's' :: 'e' :: rest => List(Southeast) ++ parse(rest)
      case 's' :: 'w' :: Nil  => List(Southwest)
      case 's' :: 'w' :: rest => List(Southwest) ++ parse(rest)
      case 'n' :: 'w' :: Nil  => List(Northwest)
      case 'n' :: 'w' :: rest => List(Northwest) ++ parse(rest)
      case 'n' :: 'e' :: Nil  => List(Northeast)
      case 'n' :: 'e' :: rest => List(Northeast) ++ parse(rest)
      case 'e' :: Nil         => List(East)
      case 'e' :: rest        => List(East) ++ parse(rest)
      case 'w' :: Nil         => List(West)
      case 'w' :: rest        => List(West) ++ parse(rest)
    }
  }

  def neighbours(tile: (Int, Int)) =
    List(
      East.move(tile),
      Southeast.move(tile),
      Southwest.move(tile),
      West.move(tile),
      Northwest.move(tile),
      Northeast.move(tile)
    )
  }