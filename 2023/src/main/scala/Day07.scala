import scala.io.Source

object Day07:
  val cardLabels = Seq('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
  val cardRanks = cardLabels.zipWithIndex.map((c, i) => c -> (i + 1)).toMap

  case class Hand(cards: Array[Char], bid: Long):
    lazy val cardGroups = cards.groupBy(identity).map((label, cards) => label -> cards.length)
    lazy val rankDigits = (cardGroups.values.toSeq.sortWith(_ > _) ++ Seq.fill(5)(0)).take(5)
    lazy val rank = rankDigits.foldLeft(0)((acc, i) => acc * 0xf + i)
    lazy val value = cards.foldLeft(0) { (acc, card) => acc * 0xf + cardRanks(card) }
    lazy val sortKey = (rank, value)
    lazy val valueWithJokers = cards.foldLeft(0) { (acc, card) => acc * 0xf + (if card == 'J' then 1 else cardRanks(card) + 1) }
    lazy val rankWithJokers = {
      val jokersCount = cardGroups.getOrElse('J', 0)
      val newCardGroups = cardGroups.removed('J')
      val digits = (newCardGroups.values.toSeq.sortWith(_ > _) ++ Seq.fill(5)(0)).take(5)
      ((digits.head + jokersCount) +: digits.tail).foldLeft(0)((acc, i) => acc * 0xf + i)
    }
    lazy val sortKeyWithJoker = (rankWithJokers, valueWithJokers)

  object Hand:
    def fromString(str: String): Hand =
      val Array(cards, bid) = str.split(" ")
      Hand(cards.toCharArray, bid.toLong)

  lazy val source = Source.fromFile("input/day07.txt")
  lazy val hands = source.getLines.map(Hand.fromString).toSeq

  lazy val part1 = hands.sortBy(_.sortKey).zipWithIndex.map((hand, i) => (i + 1) * hand.bid).sum
  lazy val part2 = hands.sortBy(_.sortKeyWithJoker).zipWithIndex.map((hand, i) => (i + 1) * hand.bid).sum

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)
