package aoc.day05

import aoc.shared.Reader

case class Seat(row: Int, column: Int) {
  def id = row * 8 + column
}

object Day5 {
  def main(args: Array[String]): Unit = {
    val input = Reader.lines("05.txt")

    val passes = input.map(s => (s.substring(0, 7), s.substring(7, 10)))

    val seats = passes.map{p =>
      Seat(
        row(p._1),
        column(p._2)
      )
    }

    val takenIds = seats.map(_.id)
    println(takenIds.max)

    val possibleRows = List.range(0, 128)
    val possibleColumns = List.range(0, 8)

    val possibleSeats =
      (for {
        x <- possibleRows
        y <- possibleColumns
      } yield Seat(x,y)).filter(s => s.row != 0 && s.row != 127)

    val excludingTaken = possibleSeats.filter(s => !seats.contains(s))

    val availableIds = excludingTaken.map(_.id)
    println(availableIds.filter(id => takenIds.contains(id - 1) && takenIds.contains(id + 1)))
  }

  def row(s: String) =
    s.toCharArray.toList.reverse.zipWithIndex.reverse.foldLeft(0){ (i, t) =>
      t._1 match {
        case 'F' => i
        case 'B' =>  i + Math.pow(2, t._2).toInt
      }
    }

  def column(s: String) =
    s.toCharArray.toList.reverse.zipWithIndex.reverse.foldLeft(0){ (i, t) =>
      t._1 match {
        case 'L' => i
        case 'R' =>  i + Math.pow(2, t._2).toInt
      }
    }
}
