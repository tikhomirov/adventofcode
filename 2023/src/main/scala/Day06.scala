import scala.io.Source

object Day06:
  lazy val source = Source.fromFile("input/day06.txt")
  lazy val lines = source.getLines.toSeq

  lazy val timeValues = extractNumbers(lines.head)
  lazy val distanceValues = extractNumbers(lines.tail.head)

  lazy val part1 = timeValues.zip(distanceValues)
    .map { (raceTime, previousRecord) => distances(raceTime, previousRecord).length }
    .product

  lazy val part2 =
    val raceTime = timeValues.map(_.toString).mkString.toLong
    val previousRecord = distanceValues.map(_.toString).mkString.toLong
    distances(raceTime, previousRecord).length

  def distances(raceTime: Long, previousRecord: Long): Seq[Long] =
    LazyList.from(1L until raceTime)
      .dropWhile { holdTime => previousRecord >= raceDistance(raceTime, holdTime) }
      .takeWhile { holdTime => previousRecord <  raceDistance(raceTime, holdTime) }

  def raceDistance(raceTime: Long, holdTime: Long): Long =
    (raceTime - holdTime) * holdTime

  def extractNumbers(str: String): IndexedSeq[Long] =
    "\\d+".r.findAllIn(str).map(_.toLong).toIndexedSeq

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)
