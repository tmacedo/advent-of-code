package aoc.day07

import aoc.shared.Reader

case class Bags(colour: String, amount: Int)
case class Rule(colour: String, inside: List[Bags]) {
  def includes(c: String, rules: List[Rule]): Boolean = {
    inside.exists(b => {
      val r = rules.find(p => p.colour == b.colour)

      b.colour == c || r.get.includes(c, rules)
    })
  }

  def childrenCount(rules: List[Rule]): Int = {
    inside.map(b => {
      val r = rules.find(p => p.colour == b.colour).get

      r.childrenCount(rules) * b.amount + b.amount
    }).sum
  }
}

object App {
  def main(args: Array[String]): Unit = {

    val input = Reader.lines("07.txt")

    val parsed = input.map(parseRule)

    println(parsed.filter(r => r.includes("shiny gold", parsed)).size)

    val shinyGold = parsed.find(r => r.colour == "shiny gold").get
    println(shinyGold.childrenCount(parsed))
  }

  def parseRule(rule: String) = {
    val emptyPattern = "(.+) bags contain no other bags.".r
    val containsPattern = "(.+) bags contain (.+).".r

    rule match {
      case emptyPattern(c) => Rule(c, List())
      case containsPattern(c, bags) => Rule(c, bags.split(", ").toList.map(parseBag))
    }
  }

  def parseBag(bag: String): Bags = {
    val bagPattern = "([0-9]+) (.+) bag[s]?".r

    val bagPattern(n, c) = bag

    Bags(c, n.toInt)
  }
}
