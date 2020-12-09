import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromFile("src/main/resources/day9/input.txt").getLines().toList.map(BigInt(_))
val p = 25

def hasSumIn(target: BigInt, xs: List[BigInt]): Boolean = (for {
    x1 <- xs
    x2 <- xs if (x2 < x1 && x2 + x1 == target)
  } yield (x1, x2)).nonEmpty

val exception = input.view.sliding(p + 1).find(range => !hasSumIn(range.last, range.take(p).toList)).get.last

def takeSeqWhile[A](list: List[A], p: List[A] => Boolean): Option[List[A]] = {
  @tailrec
  def helper(pre: List[A], post: List[A]): Option[List[A]] =
    if (p(pre)) Some(pre)
    else post match {
      case h :: tail => helper(h :: pre, tail)
      case Nil => None
    }
  
  helper(List(), list)
}
  
def contiguousSumSet(input: List[BigInt], target: BigInt): Option[List[BigInt]] =
  takeSeqWhile(input, (l: List[BigInt]) => l.sum >= target).flatMap(xs =>
    if (xs.sum == target) Some(xs)
    else contiguousSumSet(input.tail, target)
  )

val sumSet = contiguousSumSet(input.takeWhile(_ != exception).reverse, exception).get
sumSet.min + sumSet.max
