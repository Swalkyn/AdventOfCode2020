import scala.io.Source

val input = Source.fromFile("src/main/resources/day1/input.txt").getLines().map(_.toInt).toList.view

val part1 = input.combinations(2).find(_.sum == 2020).flatMap(xs => Some(xs.product))
val part2 = input.combinations(3).find(_.sum == 2020).flatMap(xs => Some(xs.product))