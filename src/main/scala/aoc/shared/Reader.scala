package aoc.shared

import scala.io.Source

object Reader {

  def lines(fileName: String): List[String] =
    Source.fromResource(fileName)
      .getLines.toList

  def groups(fileName: String): List[List[String]] =
    lines(fileName).foldLeft(List.empty[List[String]]) {
      case (acc, s) if s.isEmpty || acc.isEmpty || acc.last.contains("") => acc :+ List(s)
      case (acc, s) => acc.init :+ (acc.last :+ s)
    }.filter(s => !s.contains(""))
}
