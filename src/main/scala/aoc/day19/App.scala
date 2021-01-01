package aoc.day19

import aoc.shared.Reader

import scala.annotation.tailrec

object App {

  def main(args: Array[String]): Unit = {

    val groups = Reader.groups("19.txt")

    val rules: Map[Int, String] = groups(0).map(rule => {
      val split = rule.split(": ")
      val number = split(0).toInt

      val rulePart = split(1)
      val replaced = rulePart
        .replaceAll("\"", "")
        .replaceAll("(\\d+)", "[$1]")

      val withParens = if(replaced.contains("|"))
        s"($replaced)"
      else
        replaced

      (number, withParens)
    }).toMap


    result(groups(1), rules)

    val updatedRules = rules
      .updated(8, "([42]+)")
      .updated(11, generateMatches(42,31))

    result(groups(1), updatedRules)
  }

  def result(messages: List[String], rules: Map[Int, String]) = {
    val rule0 = replace(rules)(0)
      .replaceAll("\\s", "")

    println(messages.count(s => rule0.r.matches(s)))
  }


  val NumberExp = "\\[(\\d)+\\]".r

  @tailrec
  def replace(rules: Map[Int, String]): Map[Int, String] = {
    val rule0 = rules.get(0).get

    if(rule0.contains("[")) {
      val m = NumberExp.findFirstMatchIn(rule0).get.toString
      val sub = m.replaceAll("\\[", "").replaceAll("\\]", "")

      val updatedRules = rules.updated(0, rule0.replaceFirst(s"\\[$sub\\]", rules.get(sub.toInt).get))
      replace(updatedRules)
    } else
      rules
  }

  val Times = 10
  def generateMatches(a: Int, b: Int): String = {
    val matches = (1 to Times).map(n =>
      s"([$a]{$n}[$b]{$n})"
    ).mkString("|")

    s"($matches)"
  }
}
