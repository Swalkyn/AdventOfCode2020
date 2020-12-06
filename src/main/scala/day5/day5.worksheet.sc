import scala.io.Source

val input = Source.fromFile("src/main/resources/day5/input.txt").getLines().toList

def getID(s: String): Int = {
  val (row, col) = s.toList.foldLeft(((0, 128), (0, 8))) { case (((rowF, rowB), (colL, colR)), c) =>
    if (c == 'F') ((rowF, Math.floorDiv(rowF + rowB, 2)), (colL, colR))
    else if (c == 'B') ((Math.floorDiv(rowF + rowB, 2), rowB), (colL, colR))
    else if (c == 'L') ((rowF, rowB), (colL, Math.floorDiv(colL + colR, 2)))
    else if (c == 'R') ((rowF, rowB), (Math.floorDiv(colL + colR, 2), colR))
    else throw new IllegalArgumentException("Badly formatted seat location")
  }

  row._1 * 8 + col._1
}

input.map(getID).sorted.sliding(2).find(l => l.head + 1 != l.tail.head).get.head + 1
