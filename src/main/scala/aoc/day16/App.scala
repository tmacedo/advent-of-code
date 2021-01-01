package aoc.day16

import aoc.shared.Reader

case class Rule(field: String, ranges: List[Range])

object App {

  def main(args: Array[String]): Unit = {

    val groups = Reader.groups("16.txt")

    val rules = groups.head.map(parseRule)
    val allRanges = rules.flatMap(_.ranges)
    val nearbyTickets = parseTickets(groups.reverse.head)

    val invalidFields = nearbyTickets.flatMap(_.filter(n => !allRanges.exists(_.contains(n))))
    println(invalidFields.sum)

    val validTickets = nearbyTickets.filter(t => t.forall(i => allRanges.exists(_.contains(i))))
    val mapped = validTickets.flatMap(_.zipWithIndex).groupBy(_._2)
    val valuesByIndex = mapped.view.mapValues(_.map(_._1)).toMap

    val fieldRules: Map[Int, List[Rule]] = valuesByIndex.map(e =>
      (e._1, validRules(rules, e._2))
    )

    val assigned = assign(fieldRules)

    val myTicket = parseTickets(groups(1)).head.zipWithIndex
    val filteredRules = assigned.filter(_._2.field.startsWith("departure"))

    val filteredFields = filteredRules.keys.map(k => myTicket.find(_._2 == k).get._1)
    println(filteredFields.foldLeft(1L)( (a, b) => a * b))
  }

  // FIXME: this isn't very good
  // only works if the assignments can be made linearly sorting by number of matching rules
  def assign(rules: Map[Int, List[Rule]]): Map[Int, Rule] = {
    val allRules = rules.values.flatten.toList.distinct
    rules.toList.sortBy(_._2.size).foldLeft((allRules, Map.empty[Int, Rule]))( (acc, mapEntry) => {
      val availableRules = acc._1
      val m = acc._2

      val rule = availableRules.filter(r => mapEntry._2.contains(r)).head
      val remainingRules = availableRules.filterNot(_ == rule)

      (remainingRules, m.updated(mapEntry._1, rule))
    })._2
  }

  def parseTickets(tickets: List[String]): List[List[Int]] =
    tickets.tail.map(_.split(",").map(_.toInt).toList)

  def parseRule(s: String): Rule = {
    val parser = " ([0-9]*)-([0-9]*) or ([0-9]*)-([0-9]*)".r

    val parts = s.split(":")

    parts(1) match {
      case parser(a,b,c,d) => Rule(parts(0), List(Range.inclusive(a.toInt,b.toInt), Range.inclusive(c.toInt,d.toInt)))
    }
  }

  def validRules(rules: List[Rule], values: List[Int]): List[Rule] = {
    rules.filter(rule =>
      values.forall(v => rule.ranges.exists(_.contains(v))))
  }
}