package aoc.day20

import aoc.shared.Reader

sealed trait Orientation {
  def opposite: Orientation
}
case object Top extends Orientation {
  def opposite = Bottom
}
case object Bottom extends Orientation {
  def opposite = Top
}
case object Left extends Orientation {
  def opposite = Right
}
case object Right extends Orientation {
  def opposite = Left
}

case class Edge(id: Int, orientation: Orientation, chars: List[Char]) {
  def fits(other: Edge): Boolean =
    id != other.id && chars == other.chars && orientation == other.orientation.opposite
}

case class Tile(id: Int, chars: List[List[Char]]) {
  def maxY = chars.head.size - 1
  def maxX = chars.size - 1

  def rotate: Tile =
    Tile(id,
      chars.transpose.reverse
    )

  def flip: Tile =
    Tile(id,
      chars.reverse
    )

  def orientations: List[Tile] =
    List(
      this,
      rotate,
      rotate.rotate,
      rotate.rotate.rotate,
      flip,
      flip.rotate,
      flip.rotate.rotate,
      flip.rotate.rotate.rotate
    )

  def edges: List[Edge] = {
    List(
      Edge(id, Top, top),
      Edge(id, Right, right),
      Edge(id, Bottom, bottom),
      Edge(id, Left, left)
    )
  }

  def top = chars.head
  def right = chars.map(_(maxY))
  def bottom = chars(maxX)
  def left = chars.map(_(0))

  def withoutEdges = chars.drop(1).dropRight(1).map(_.drop(1).dropRight(1))
}

object App {

  val monster =
    """                  #.
      |#    ##    ##    ###
      | #  #  #  #  #  #   """.stripMargin

  val monsterPattern: List[String] = monster.lines.map(s =>
    s.replaceAll(" ", ".")
  ).toList

  def main(args: Array[String]): Unit = {

    val groups = Reader.groups("20.txt")

    val tiles = groups.map(parseGroup)

    val edges = for {
      t <- tiles
      r <- t.orientations
      e <- r.edges
    } yield e

    val edgeMatches = edges.combinations(2).filter(l => l(0).fits(l(1)))
      .map(_.map(e => (e.id, e.orientation, e.chars))).toList

    val edgeMatchCounts = edgeMatches.flatMap(_.map(e => (e._1, e._2))).groupBy(identity).mapValues(_.size).toMap
    val minValue = edgeMatchCounts.values.min
    val corners = edgeMatchCounts.filter(_._2 == minValue).map(_._1._1).toList.distinct

    println(corners.foldLeft(1L)((a ,b) => a * b))
    println(part2(tiles, corners))
  }

  def part2(tiles: List[Tile], corners: List[Int]) = {
    val edgeMap: Map[Tile, List[List[Char]]] = tiles.map(t =>
      (t, t.orientations.flatMap(_.edges.map(_.chars)).distinct)
    ).toMap
    val reverseMap: Map[List[Char], List[Tile]] = edgeMap.toList.flatMap(t =>
      t._2.map(e => (e, t._1))
    ).groupBy(_._1).mapValues(_.map(_._2)).toMap

    val edges: Map[Tile, List[List[Char]]] =
      reverseMap
        .collect({
          case (edge, List(t)) => edge -> t
        }).groupMapReduce(_._2)(t => List(t._1))(_ ++ _)

    val corner1Id = corners.head
    val corner1 = tiles.find(_.id == corner1Id).get
    val corner1Edges = edges(corner1)

    val corner1CorrectOrientation = corner1.orientations.find(tile => corner1Edges.contains(tile.top) && corner1Edges.contains(tile.left)).get

    val row1 = List(corner1CorrectOrientation) ++ row(corner1CorrectOrientation, reverseMap)
    val res = List(row1) ++ puzzle(corner1CorrectOrientation, reverseMap)
    val withoutBorders = res.map(_.map(_.withoutEdges))

    val assembled: List[List[Char]] = withoutBorders.flatMap(tileRow =>
      tileRow.transpose.map(_.flatten)
    )

    val assembledRotations: List[List[String]] = rotations(assembled).map(_.map(_.mkString))

    val monsterCounts = assembledRotations.map(i => (i, countMonster(i)))

    val numMonsters = monsterCounts.map(_._2).max

    val waterInMonsters = monster.count(_ == '#') * numMonsters
    val waterInTile = assembled.flatten.count(_ == '#')

    waterInTile - waterInMonsters
  }

  def countMonster(tile: List[String]): Int = {
    val a: List[List[(String, String)]] = tile
      .sliding(monster.lines.size)
      .map(l => {
        l.zip(monsterPattern)
      }).toList


    a.map(p => {
      val matches = p.map(t =>
        matchMonster(t._1, t._2))
      // groups of 3
      matches(0).intersect(matches(1)).intersect(matches(2)).size
    }).sum
  }

  def matchMonster(s: String, r: String) = {
    (0 until s.size - r.size).filter(i =>
      s"^$r".r.findFirstIn(s.substring(i)).isDefined
    )
  }

  def rotations(tiles: List[List[Char]]): List[List[List[Char]]] = {
    def rotate(l: List[List[Char]]) =
      l.transpose.reverse

    def flip(l: List[List[Char]]) =
      l.reverse

    List(
      tiles,
      rotate(tiles),
      rotate(rotate(tiles)),
      rotate(rotate(rotate(tiles))),
      flip(tiles),
      rotate(flip(tiles)),
      rotate(rotate(flip(tiles))),
      rotate(rotate(rotate(flip(tiles))))
    )
  }

  def row(t: Tile, m: Map[List[Char], List[Tile]]): List[Tile] = {
    val right = t.right
    val tiles = m(right).filterNot(_.id == t.id)

    if(tiles.isEmpty) {
      List.empty
    } else {
      val newTile = tiles.head
      val correctOrientation = newTile.orientations.find(_.left == right).get

      correctOrientation +: row(correctOrientation, m)
    }
  }

  def puzzle(t: Tile, m: Map[List[Char], List[Tile]]): List[List[Tile]] = {
    // this is the first one of the row above
    val bottom = t.bottom
    val tiles = m(bottom).filterNot(_.id == t.id)

    if(tiles.isEmpty) {
      List.empty
    } else {
      val newTile = tiles.head
      val correctOrientation = newTile.orientations.find(_.top == bottom).get

      List(correctOrientation +: row(correctOrientation, m)) ++ puzzle(correctOrientation, m)
    }
  }

  val tileExpr = "Tile (.*):".r
  def parseGroup(g: List[String]): Tile = {
    val id = g.head match {
      case tileExpr(n) => n.toInt
    }

    Tile(id, g.tail.map(_.toCharArray.toList))
  }
}