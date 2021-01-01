package aoc.day18

import fastparse._
import NoWhitespace._
import aoc.shared.Reader

object App {
  def main(args: Array[String]): Unit = {

    val input = Reader.lines("18.txt")

    val withoutWhitespace = input.map(s => s.replaceAll("\\s", ""))

    println(part1(withoutWhitespace))
    println(part2(withoutWhitespace))
  }

  def number[_: P] = P( CharIn("0-9").rep(1).!.map(_.toLong) )
  def eval(tree: (Long, Seq[(String, Long)])) = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right
      case "*" => left * right
    }}
  }

  def part1(input: List[String]): Long = {
    def parens[_: P] = P( "(" ~/ op ~ ")" )
    def factor[_: P] = P( number | parens )
    def op[_: P]: P[Long] = P( factor ~ (CharIn("+*").! ~/ factor).rep ).map(eval)
    def expr[_: P]: P[Long]   = P( op ~ End )

    res(input.map(s => parse(s, expr(_))))
  }

  def part2(input: List[String]): Long = {
    def parens[_: P]: P[Long] = P( "(" ~/ mul ~ ")" )
    def factor[_: P]: P[Long] = P( number | parens )

    def add[_: P]: P[Long] = P( factor ~ (CharIn("+").! ~/ factor).rep ).map(eval)
    def mul[_: P]: P[Long] = P( add ~ (CharIn("*").! ~/ add).rep ).map(eval)
    def expr[_: P]: P[Long]   = P( mul ~ End )

    res(input.map(s => parse(s, expr(_))))
  }


  def res(input: List[Parsed[Long]]): Long = {
    input.collect {
      case Parsed.Success(v, _) => v
    }.sum
  }
}