import scala.io.Source

val input = Source.fromFile("src/main/resources/day11/input.txt").getLines().toList

case class Coord(x: Int, y: Int) {
  def +(other: Coord): Coord =
    Coord(this.x + other.x, this.y + other.y)
  }

implicit def ordering[A <: Coord]: Ordering[A] =
  Ordering.by(c => (c.x, c.y))

val incrs = List(Coord(0, 1), Coord(0, -1), Coord(1, -1), Coord(1, 0), Coord(1, 1), Coord(-1, -1), Coord(-1, 0), Coord(-1, 1))

def printSeats(seats: Map[Coord, Char]) =
  seats.toList.sortBy(_._1).groupBy(_._1.y).toList.sortBy(_._1).map(_._2.map(_._2).mkString).foreach(println)

def tick(seats: Map[Coord, Char], seat: (Coord, Char)) = {
  val (c, state) = seat
  val adj = (for {
      i <- c.y - 1 to c.y + 1
      j <- c.x - 1 to c.x + 1
  } yield seats(Coord(j, i))).toList
  (
    c,
    if (state == 'L' && adj.count(_ == '#') == 0) '#'
    else if (state == '#' && adj.count(_ == '#') > 4) 'L'
    else state
  )
}

def tick2(seats: Map[Coord, Char], seat: (Coord, Char)) = {
  val (c, state) = seat
  val adj = incrs.map(inc => LazyList.iterate(c)(_ + inc).map(seats).tail.find(_ != '.').get)
  (
    c,
    if (state == 'L' && adj.count(_ == '#') == 0) '#'
    else if (state == '#' && adj.count(_ == '#') > 4) 'L'
    else state
  )
}

val initial = input
  .zipWithIndex
  .map{ case (s, y) => s.zipWithIndex.map { case (c, x) => Coord(x, y) -> c }.toMap }
  .reduce(_ ++ _)
  .withDefaultValue('x')

def solveWith(tickF: (Map[Coord, Char], (Coord, Char)) => (Coord, Char)) = {
  lazy val seatsStates = LazyList.iterate(initial)(seats => seats.map(tickF(seats, _)).withDefaultValue('x'))
  for {
    mm <- seatsStates.zip(seatsStates.tail).find{ case (s1, s2) => s1 == s2 }
  } yield mm._1.count(_._2 == '#')
}

solveWith(tick)
solveWith(tick2)