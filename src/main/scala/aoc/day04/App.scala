package aoc.day04

import aoc.shared.Reader

case class Field(name: String, value: String)
case class Passport(fields: List[Field])

object Day4 {
  val Fields = List(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  )

  val Colours = List(
    "amb",
    "blu",
    "brn",
    "gry",
    "grn",
    "hzl",
    "oth"
  )

  def main(args: Array[String]): Unit = {
    val input = Reader.groups("04.txt").map(_.mkString(" "))

    val passports = input.map(parse)

    val valid = passports.filter(hasAllFields)
    println(valid.size)

    val rules = valid.filter(p => p.fields.filter(f => Fields.contains(f.name)).forall(validField))
    println(rules.size)

  }

  def hasAllFields(p: Passport): Boolean = {
    Fields.forall(s => p.fields.map(_.name).contains(s))
  }

  def validField(f: Field): Boolean = {
    try {
      f match {
        case Field("byr", s) if s.toInt >= 1920 && s.toInt <= 2002 => true
        case Field("iyr", s) if s.toInt >= 2010 && s.toInt <= 2020 => true
        case Field("eyr", s) if s.toInt >= 2020 && s.toInt <= 2030 => true
        case Field("hgt", s) => heightCheck(s)
        case Field("hcl", s) if s.matches("^#[0-9a-f]{6,6}$") => true
        case Field("ecl", s) => Colours.contains(s)
        case Field("pid", s) if s.matches("^[0-9]{9,9}$") => true
        case _ => false
      }
    } catch {
      case _: NumberFormatException => false
    }
  }

  def parse(s: String): Passport = {
    val fields = s.split(" ").toList

    Passport(
      fields.map(f => {
        val split = f.split(":")
        Field(split(0), split(1))
      }))
  }

  def heightCheck(s: String): Boolean = {
    val height = s.dropRight(2).toFloat
    val unit = s.takeRight(2)

    unit match {
      case "cm" =>
        height >= 150 && height <= 193
      case "in" =>
        height >= 59 && height <= 76
      case _ =>
        false
    }
  }
}
