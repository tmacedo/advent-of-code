package aoc.day25

object App {
  def main(args: Array[String]): Unit = {
    val cardKey = 19241437
    val doorKey = 17346587

    val cardLoopSize = findLoopSize(cardKey)
    println(operation(cardLoopSize, doorKey))
  }

  val InitialSubjectNumber = 7
  def findLoopSize(targetKey: Int): Int = {
    Iterator.iterate(1)(n => (InitialSubjectNumber * n) % 20201227)
      .zipWithIndex
      .find(_._1 == targetKey)
      .get
      ._2
  }

  def operation(loopSize: Int, subjectNumber: Int): Long = {
    (1 until loopSize).foldLeft(subjectNumber.toLong)( (acc, _) =>
      (acc * subjectNumber) % 20201227L
    )
  }
}
