import scala.io.Source

object Day09:
  lazy val source = Source.fromFile("input/day09.txt")
  lazy val histories = source.getLines.map(_.split(" ").map(_.toInt).toSeq).toSeq

  lazy val part1 = histories.map(diffs).sum
  lazy val part2 = histories.map(diffsBack).sum

  def diffs(numbers: Seq[Int]) =
    var lastValues = Seq(numbers.last)
    var seq = numbers
    while (!seq.forall(_ == 0)) {
      seq = seq.sliding(2).map { case Seq(i, j) => j - i }.toSeq
      lastValues = lastValues.prepended(seq.last)
    }
    lastValues.sum

  def diffsBack(numbers: Seq[Int]) =
    var firstValues = Seq(numbers.head)
    var seq = numbers
    while (!seq.forall(_ == 0)) {
      seq = seq.sliding(2).map { case Seq(i, j) => j - i }.toSeq
      firstValues = firstValues.prepended(seq.head)
    }
    firstValues.foldLeft(0) { (acc, i) => i - acc }

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)