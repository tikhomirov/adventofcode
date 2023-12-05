import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.io.Source

object Day05:
  lazy val source = Source.fromFile("input/day05.txt")
  lazy val lines = source.getLines.toSeq

  lazy val seeds = extractNumbers(lines.head.split(":").last)
  lazy val seedRanges = seeds.sliding(2, 2).map(xs => xs.head until (xs.head + xs.last)).toSeq

  lazy val mappings =
    @tailrec
    def collectMappings(acc: Seq[Seq[String]], it: Iterator[String]): Seq[Seq[String]] =
      if !it.hasNext then
        acc
      else
        val newAcc = acc.appended(it.takeWhile(_.nonEmpty).toSeq)
        collectMappings(newAcc, it)

    collectMappings(Seq.empty, lines.iterator.drop(2))
      .map(_.tail.map(parseMapping))

  lazy val part1: Long = seeds.map(seedLocation).min

  lazy val part2: Long =
    seedRanges.foldLeft(Long.MaxValue) { (acc, range) =>
      range.foldLeft(acc) { (result, seed) =>
        math.min(result, seedLocation(seed))
      }
    }

  def extractNumbers(str: String): Seq[Long] =
    "\\d+".r.findAllIn(str).map(_.toLong).toSeq

  def parseMapping(str: String): (NumericRange.Exclusive[Long], NumericRange.Exclusive[Long]) =
    val Seq(dst, src, len) = extractNumbers(str)
    (src until src + len) -> (dst until dst + len)

  def seedLocation(seed: Long): Long =
    mappings.foldLeft(seed) { (num, ranges) =>
      ranges
        .find { (src, _) => src.contains(num) }
        .map { (src, dst) => num - src.start + dst.start }
        .getOrElse(num)
    }

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)