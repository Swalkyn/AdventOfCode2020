import scala.io.Source

val input = Source.fromFile("src/main/resources/day3/input.txt").getLines().toList

val mapWidth = input.head.length

def treeCollisions(slope: (Int, Int)): Int =
  input.sliding(1, slope._2).foldLeft((0, 0)) {
    case ((x, trees), xs) => (x + slope._1, if (xs.head(x % mapWidth) == '#') trees + 1 else trees)
  }._2

treeCollisions((3, 1))

val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
slopes.map(s => BigInt(treeCollisions(s))).product

