import scala.io.Source
import scala.util.matching.Regex

object Day03:
  lazy val source = Source.fromFile("input/day03.txt")
  lazy val lines = source.getLines.toArray.toSeq
  lazy val numberRegex = "\\d+".r

  lazy val adjacent: Seq[(Int, Int)] = for {
    (line, row) <- lines.zipWithIndex
    (char, col) <- line.toCharArray.zipWithIndex
    if isSymbol(char)
    i <- (row - 1) to (row + 1)
    j <- (col - 1) to (col + 1)
    if !(i == row && j == col)
  } yield (i, j)

  lazy val gears: Seq[(Int, Int)] = for {
    (line, row) <- lines.zipWithIndex
    (char, col) <- line.toCharArray.zipWithIndex
    if char == '*'
  } yield (row, col)

  lazy val gearAdjucents =
    gears.map((row, column) => adjacentTo(row, column)).filter(_.length == 2)

  lazy val numbers = for {
    (line, row) <- lines.zipWithIndex
    m <- numberRegex.findAllMatchIn(line)
    if (m.start until m.end).map(i => (row, i)).exists(adjacentSet.contains)
  } yield (row, m.start until m.end, m.matched.toInt)

  lazy val adjacentSet = adjacent.toSet

  lazy val partNumbers = for {
    (line, row) <- lines.zipWithIndex
    m <- numberRegex.findAllMatchIn(line)
    if (m.start until m.end).map(i => (row, i)).exists(adjacentSet.contains)
  } yield m.matched.toInt

  def findAllNumbers(line: String): Iterator[Regex.Match] =
    numberRegex.findAllMatchIn(line)

  def adjacentTo(row: Int, col: Int): Seq[Int] =
    for {
      (numRow, numColRange, number) <- numbers
      if (numRow - 1 to numRow + 1).contains(row) && (col - 1 to col + 1).exists(numColRange.contains)
    } yield number

  def isSymbol(char: Char): Boolean = !(char.isDigit || char == '.')

  def part1: Int =
    partNumbers.sum

  def part2: Int =
    gearAdjucents.map(_.product).sum

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)
