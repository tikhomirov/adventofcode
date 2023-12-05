import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day04:
  case class Card(winning: Set[Int], numbers: Set[Int]):
    lazy val matchesCount = winning.intersect(numbers).size
    lazy val points = if matchesCount > 0 then math.pow(2, matchesCount - 1).toInt else 0

  object Card:
    def parse(line: String): Card =
      val parts = line.split(":").last.split("\\|").toSeq
      val Seq(winning, numbers) = parts.map(parseInts)
      Card(winning.toSet, numbers.toSet)

    private def parseInts(str: String): Seq[Int] =
      "\\d+".r.findAllIn(str).map(_.toInt).toSeq

  lazy val source = Source.fromFile("input/day04.txt")
  lazy val lines = source.getLines

  lazy val cards = lines.map(Card.parse).toSeq

  lazy val part1 = cards.map(_.points).sum

  lazy val part2 =
    @tailrec
    def processQueue(queue: Queue[Int], acc: Int): Int =
      if queue.isEmpty then
        acc
      else
        val (i, dequeued) = queue.dequeue
        val matchesCount = cards(i).matchesCount
        val newQueue = dequeued.appendedAll((1 to matchesCount).map(_ + i))
        processQueue(newQueue, acc + matchesCount)
    processQueue(Queue.from(cards.indices), cards.size)

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)
