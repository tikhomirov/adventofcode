import scala.io.Source

object Day1:
  lazy val source = Source.fromFile("input/day1.txt")
  lazy val lines = source.getLines.toList

  lazy val wordToDigit = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  def maybeDigitAt(str: String)(offset: Int): Option[Int] =
    val char = str.charAt(offset)
    if char.isDigit then Some(char.asDigit) else None

  def findDigits(str: String): Array[Int] =
    (0 until str.length).flatMap(maybeDigitAt(str)).toArray

  def findDigitsWithWords(str: String): Array[Int] =
    (0 until str.length).flatMap(findDigitAt(str)).toArray

  def findDigitAt(str: String)(offset: Int): Option[Int] =
    wordToDigit.keys
      .find { word => str.startsWith(word, offset) }
      .map(wordToDigit)
      .orElse(maybeDigitAt(str)(offset))

  def mkCalibrationValue(digits: Array[Int]): Int =
    digits.head * 10 + digits.last

  def part1: Int =
    lines.map(findDigits andThen mkCalibrationValue).sum

  def part2: Int =
    lines.map(findDigitsWithWords andThen mkCalibrationValue).sum

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)