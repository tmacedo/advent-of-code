package aoc.day14

import aoc.shared.Reader

sealed trait Input
case class Mask(s: String) extends Input
case class Memory(address: Int, value: Int) extends Input

case class State(mem: List[Long], mask: List[Char])

case class LongState(mem: Map[Long, Long], mask: List[Char])

object App {

  def main(args: Array[String]): Unit = {

    val input = Reader.lines("14.txt")
    val instructions = input.map(parse)

    val maxAddress = instructions
      .flatMap({
        case Memory(a, _) => Some(a)
        case _ => None
      }).max

    val part1 = instructions.foldLeft(State(List.fill(maxAddress + 1)(0), List.empty))( (state, inst) => {
      inst match {
        case Mask(s) => state.copy(mask = s.toCharArray.toList)
        case Memory(a, v) => {
          state.copy(mem = state.mem.updated(a, applyMask(state.mask, v)))
        }
      }
    })

    println(part1.mem.sum)

    val part2 = instructions.foldLeft(LongState(Map.empty, List.empty))( (state, inst) => {
      inst match {
        case Mask(s) => state.copy(mask = s.toCharArray.toList)
        case Memory(a, v) => {
          maskedAddresses(state.mask, a).foldLeft(state)( (s, maskedAddress) =>
            s.copy(
              mem = s.mem.updated(maskedAddress, v)
            )
          )
        }
      }
    })

    println(part2.mem.values.sum)
  }

  def applyMask(mask: List[Char], value: Int) : Long = {
    val valueAsBits = value.toBinaryString
    val missingBits = mask.size - valueAsBits.size
    val paddedValue = List.fill(missingBits)(0) ++ valueAsBits.split("").map(_.toInt)

    val s = mask.zip(paddedValue).map( t => {
      t._1 match {
        case 'X' => t._2
        case '0' => 0
        case '1' => 1
      }
    })

    java.lang.Long.parseUnsignedLong(s.mkString(""), 2)
  }

  def maskedAddresses(mask: List[Char], address: Int) : List[Long] = {
    val valueAsBits = address.toBinaryString
    val missingBits = mask.size - valueAsBits.size
    val paddedValue = List.fill(missingBits)(0) ++ valueAsBits.split("").map(_.toInt)

    val s = mask.zip(paddedValue).map( t => {
      t._1 match {
        case 'X' => 'X'
        case '0' => t._2.toString.charAt(0)
        case '1' => '1'
      }
    })
    val replaced = replaceX(List.empty, s.mkString(""))

    replaced.map(n => {
      java.lang.Long.parseUnsignedLong(n, 2)
    }
    )
  }

  def replaceX(l: List[String], s: String): List[String] = {
    if(s.exists(_ == 'X')) {
      replaceX(l, s.replaceFirst("X",  "0")) ++
        replaceX(l, s.replaceFirst("X",  "1"))
    } else
      l :+ s
  }

  val mask = "mask = ([0-9X]*)".r
  val mem = "mem\\[([0-9]*)\\] = ([0-9]*)".r
  def parse(s: String): Input = {
    s match {
      case mask(v) => Mask(v)
      case mem(a, v) => Memory(a.toInt, v.toInt)
    }
  }
}
