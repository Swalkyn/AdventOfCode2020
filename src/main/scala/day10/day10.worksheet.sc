import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromFile("src/main/resources/day10/input.txt").getLines().map(_.toInt).toList

val joltageDiffs = (0 :: input.sorted).sliding(2).map {
  case a :: b :: Nil => b - a
  case _ => throw new Exception("Should not happen")
}.toList.groupBy(identity).view.mapValues(_.size)

val diffsProduct = joltageDiffs(1) * (joltageDiffs(3) + 1)

lazy val sComb: LazyList[Int] = 1 #:: 2 #:: 4 #::
  sComb.zip(sComb.tail.zip(sComb.drop(2))).map { case (a, (b, c)) => a + b + c }

sComb.take(10)

val test = (0 :: input.sorted)
  .zipWithIndex
  .groupMap{ case (x, i) => x - i }(_._1)
  .map(_._2.size)
  .filter(_ > 2)
  .map(x => BigInt(sComb(x - 2)))
  .product
