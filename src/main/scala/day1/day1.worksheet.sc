import scala.io.Source

val input = Source.fromFile("src/main/resources/day1/input.txt").getLines().map(_.toInt).toList.view

lazy val part1 = (for {
  x <- input
  y <- input if (x + y == 2020)
} yield x * y).head

lazy val part2 = (for {
  x <- input
  y <- input
  z <- input if (x + y + z == 2020)
} yield x * y * z).head

part1
part2