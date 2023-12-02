import scala.io.Source

object Day2:
  case class Round(r: Int, g: Int, b: Int):
    val isPossible: Boolean = (r <= 12) && (g <= 13) && (b <= 14)

  case class Game(id: Int, rounds: Seq[Round]):
    val isPossible: Boolean = rounds.forall(_.isPossible)

    val power: Int =
      Seq(rounds.map(_.r).max, rounds.map(_.g).max, rounds.map(_.b).max)
        .product

  def parseGame(str: String): Game =
    val Seq(id, rounds) = str.split(": ").toSeq
    Game(extractNumber(id), rounds.split("; ").map(parseRound))

  def parseRound(str: String): Round =
    val cubes = str.split(", ").map(parseCube).toMap
    Round(cubes.getOrElse('r', 0), cubes.getOrElse('g', 0), cubes.getOrElse('b', 0))

  def parseCube(str: String): (Char, Int) =
    val Seq(count, color) = str.split(" ").toSeq
    (color.charAt(0), extractNumber(count))

  def extractNumber(str: String): Int =
    "\\d+".r.findFirstIn(str).get.toInt

  lazy val source = Source.fromFile("input/day2.txt")
  lazy val input = source.getLines.toSeq
  lazy val games = input.map(parseGame)

  lazy val part1 = games.filter(_.isPossible).map(_.id).sum
  lazy val part2 = games.map(_.power).sum

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)
