import scala.io.Source

object Day11:
  lazy val source = Source.fromFile("input/day11.txt")

  lazy val grid = source.getLines.map(_.toIndexedSeq).toIndexedSeq
  lazy val rows = grid.indices
  lazy val cols = grid.head.indices

  lazy val emptyRows = rows.filter(row => grid(row).forall(_ == '.'))
  lazy val emptyCols = cols.filter(col => rows.forall(row => grid(row)(col) == '.'))

  lazy val galaxies = for {
    row <- rows
    col <- cols
    if grid(row)(col) == '#'
  } yield row.toLong -> col.toLong

  lazy val galaxyPairs = galaxies.zipWithIndex.flatMap((g1, i) => galaxies.drop(i + 1).map(_ -> g1))

  lazy val part1 = sumOfDistances(2)
  lazy val part2 = sumOfDistances(1000000)

  def sumOfDistances(expansion: Long): Long =
    galaxyPairs.map { (g1, g2) => distance(g1, g2) + additionalSpace(g1, g2, expansion) }.sum

  def distance(g1: (Long, Long), g2: (Long, Long)): Long =
    math.abs(g2._1 - g1._1) + math.abs(g2._2 - g1._2)

  def additionalSpace(g1: (Long, Long), g2: (Long, Long), expansion: Long): Long =
    val v = emptyRows.count(row => row > math.min(g1._1, g2._1) && row < math.max(g1._1, g2._1))
    val h = emptyCols.count(col => col > math.min(g1._2, g2._2) && col < math.max(g1._2, g2._2))
    v * (expansion - 1) + h * (expansion - 1)

  def main(args: Array[String]): Unit =
    println(part1)
    println(part2)
