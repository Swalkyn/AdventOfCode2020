import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromFile("src/main/resources/day9/input.txt").getLines().toList.map(BigInt(_))
val p = 25

def hasSumIn(target: BigInt, xs: List[BigInt]): Boolean =
  xs.combinations(2).exists(_.sum == target)

val exception = input.view.sliding(p + 1).find(range => !hasSumIn(range.last, range.take(p).toList)).get.last

val reducedInput = input.takeWhile(_ != exception)
val sumSet = (for {
  i <- (2 until reducedInput.size).view
  set <- reducedInput.sliding(i)
  if set.sum == exception
} yield set.min + set.max).head