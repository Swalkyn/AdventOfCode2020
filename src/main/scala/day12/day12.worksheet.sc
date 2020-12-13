import scala.io.Source

val input = Source.fromFile("src/main/resources/day12/input.txt").getLines().toList

val instructions = input.map(s => (s.head, s.tail.toInt))

case class Coord(x: Int, y: Int) {
  def +(other: Coord): Coord =
    Coord(this.x + other.x, this.y + other.y)
  
  def *(scalar: Int): Coord =
    Coord(x * scalar, y * scalar)
}

val charToDirection = Map('N' -> N, 'S' -> S, 'W' -> W, 'E' -> E)

sealed trait Direction {
  def left: Direction
  def right: Direction
  def unit: Coord
}
case object N extends Direction {
  def left = W
  def right = E
  def unit = Coord(0, 1)
}
case object S extends Direction {
  def left = E
  def right = W
  def unit = Coord(0, -1)
}
case object E extends Direction {
  def left = N
  def right = S
  def unit = Coord(1, 0)
}
case object W extends Direction {
  def left = S
  def right = N
  def unit = Coord(-1, 0)
}

val (finalC, finalD) = instructions.foldLeft((Coord(0, 0), E: Direction)) {
  case ((c, d), (instr, n)) =>
    if (instr == 'F') (c + d.unit*n, d)
    else if (instr == 'L') (c, LazyList.iterate(d)(_.left)(n / 90))
    else if (instr == 'R') (c, LazyList.iterate(d)(_.right)(n / 90))
    else (c + charToDirection(instr).unit*n, d)
}

Math.abs(finalC.x) + Math.abs(finalC.y)

val (finalC2, finalD2) = instructions.foldLeft((Coord(0, 0), Coord(10, 1))) {
  case ((sc, wc), (instr, n)) =>
    if (instr == 'F') (sc + wc*n, wc)
    else if (instr == 'L') (sc, LazyList.iterate(wc)(c => Coord(-c.y, c.x))(n / 90))
    else if (instr == 'R') (sc, LazyList.iterate(wc)(c => Coord(c.y, -c.x))(n / 90))
    else (sc, wc + charToDirection(instr).unit*n)
}

Math.abs(finalC2.x) + Math.abs(finalC2.y)
// (1, 2), (-2, 1), (-1, -2), (2, -1)