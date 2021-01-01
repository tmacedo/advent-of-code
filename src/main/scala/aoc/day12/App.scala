package aoc.day12

import aoc.shared.Reader

import scala.util.matching.Regex

sealed trait Instruction
case class North(n: Int) extends Instruction
case class South(n: Int) extends Instruction
case class East(n: Int) extends Instruction
case class West(n: Int) extends Instruction
case class Left(n: Int) extends Instruction
case class Right(n: Int) extends Instruction
case class Forward(n: Int) extends Instruction

object North extends Direction {
  val angle = 0
}
object South extends Direction {
  val angle = 180
}
object East extends Direction {
  val angle = 90
}
object West extends Direction {
  val angle = 270
}

object Angle {
  def normalised(n: Int): Int = n % 360
}

sealed trait Direction {
  val angle: Int

  def left(n: Int): Direction = {
    val newAngle = Angle.normalised(360 + (angle - n))

    direction(newAngle)
  }

  def right(n: Int): Direction = {
    val newAngle = Angle.normalised(360 + (angle + n))

    direction(newAngle)
  }

  def direction(n: Int): Direction =
    n match {
      case North.angle => North
      case South.angle => South
      case East.angle => East
      case West.angle => West
    }
}

case class Position(direction: Direction, xPos: Int, yPos: Int) {
  def manhattan: Int =
    Math.abs(xPos) + Math.abs(yPos)
}

case class ShipAndWaypoint(ship: (Int, Int), waypoint: (Int, Int)) {
  def manhattan: Int =
    Math.abs(ship._1) + Math.abs(ship._2)

  def left(n: Int): ShipAndWaypoint = {
    copy(
      waypoint = Angle.normalised(n) match {
        case 0 => waypoint
        case 90 => (waypoint._2, 0 - waypoint._1)
        case 180 => (0 - waypoint._1, 0 - waypoint._2)
        case 270 => (0 - waypoint._2, waypoint._1)
      }
    )
  }

  def right(n: Int): ShipAndWaypoint = {
    copy(
      waypoint = Angle.normalised(n) match {
        case 0 => waypoint
        case 90 => (0 - waypoint._2, waypoint._1)
        case 180 => (0 - waypoint._1, 0 - waypoint._2)
        case 270 => (waypoint._2, 0 - waypoint._1)
      }
    )
  }
}

object App {

  def main(args: Array[String]): Unit = {

    val input = Reader.lines("12.txt")

    val instructions = input.map(parse)

    val pos = instructions.foldLeft(Position(East, 0, 0))( (pos, i) => {
      i match {
        case North(n) => pos.copy(yPos = pos.yPos - n)
        case South(n) => pos.copy(yPos = pos.yPos + n)
        case East(n) => pos.copy(xPos = pos.xPos + n)
        case West(n) => pos.copy(xPos = pos.xPos - n)
        case Left(n) => pos.copy(direction = pos.direction.left(n))
        case Right(n) => pos.copy(direction = pos.direction.right(n))
        case Forward(n) =>
          pos.direction match {
            case North => pos.copy(yPos = pos.yPos - n)
            case South => pos.copy(yPos = pos.yPos + n)
            case East => pos.copy(xPos = pos.xPos + n)
            case West => pos.copy(xPos = pos.xPos - n)
          }
      }
    })

    println(pos.manhattan)

    val part2 = instructions.foldLeft(ShipAndWaypoint((0, 0), (10, -1)))( (positions, i) => {
      i match {
        case North(n) => positions.copy(waypoint = (positions.waypoint._1, positions.waypoint._2 - n))
        case South(n) => positions.copy(waypoint = (positions.waypoint._1, positions.waypoint._2 + n))
        case East(n) => positions.copy(waypoint = (positions.waypoint._1 + n, positions.waypoint._2))
        case West(n) => positions.copy(waypoint = (positions.waypoint._1 - n, positions.waypoint._2))

        case Left(n) =>
          positions.left(n)
        case Right(n) =>
          positions.right(n)
        case Forward(n) =>
          positions.copy(ship = (
            positions.ship._1 + n * positions.waypoint._1,
            positions.ship._2 + n * positions.waypoint._2))
      }
    })

    println(part2.manhattan)
  }

  def parse(s: String): Instruction = {
    s match {
      case northPattern(n) => North(n.toInt)
      case southPattern(n) => South(n.toInt)
      case eastPattern(n) => East(n.toInt)
      case westPattern(n) => West(n.toInt)
      case leftPattern(n) => Left(n.toInt)
      case rightPattern(n) => Right(n.toInt)
      case forwardPattern(n) => Forward(n.toInt)
    }
  }

  val northPattern = pattern('N')
  val southPattern = pattern('S')
  val eastPattern = pattern('E')
  val westPattern = pattern('W')
  val leftPattern = pattern('L')
  val rightPattern = pattern('R')
  val forwardPattern = pattern('F')

  def pattern(i: Char): Regex =
    s"${i}([0-9]+)".r
}
