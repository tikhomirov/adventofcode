import scala.io.Source

object Day08:
  lazy val source = Source.fromFile("input/day08.txt")
  lazy val lines = source.getLines.toSeq

  lazy val instructions = LazyList.continually(lines.head.toCharArray).flatten
  lazy val network  = lines.drop(2).map(parseNetworkLine).toMap
  lazy val startNodes = network.keys.filter(_.charAt(2) == 'A').toSeq

  def parseNetworkLine(line: String): (String, (String, String)) =
    val Seq(src, lft, rgt) = "\\w{3}".r.findAllIn(line).toSeq
    src -> (lft, rgt)

  def lcm(list: Seq[Long]): Long = list.foldLeft(1L) { (i, j) =>
    j * i / LazyList.iterate((i, j)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
  }

  lazy val part1 = instructions.scanLeft(0 -> "AAA") { (acc, instruction) =>
    val (step, node) = acc
    val (lft, rgt) = network(node)
    val result = (step + 1) -> (if instruction == 'L' then lft else rgt)
    result
  }.takeWhile(_._2 != "ZZZ").last._1 + 1

  lazy val part2 =
    val paths = startNodes.map { startNode =>
      instructions.scanLeft(0 -> startNode) { (acc, instruction) =>
        val (step, node) = acc
        val (lft, rgt) = network(node)
        val result = (step + 1) -> (if instruction == 'L' then lft else rgt)
        result
      }.dropWhile(_._2.charAt(2) != 'Z').take(1).last
    }
    lcm(paths.map(_._1))

  def calculateLCM(numbers: Seq[Int]): Int = -1

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)
