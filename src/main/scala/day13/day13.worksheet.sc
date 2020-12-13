import scala.io.Source

val input = Source.fromFile("src/main/resources/day13/input.txt").getLines().toList

val departure = input.head.toInt
val sndLine = input.tail.head.split(',')

val busses = sndLine.filter(_ != "x").map(_.toInt).toSet
val (busID, dTime) = busses.map(ts => (ts, LazyList.iterate(0)(_ + ts).find(_ >= departure))).minBy(_._2)
busID * (dTime.get - departure)

val busses2 = sndLine.zipWithIndex.filter(_._1 != "x").map{ case (bus, i) => (BigInt(bus), BigInt(i % bus.toInt)) }.toList
busses2.tail.foldLeft(busses2.head) {
  case ((m1, r1), (m2, r2)) => (m1 * m2, LazyList.iterate(r1)(_ + m1).find(i => (i + r2) % m2 == 0).get)
}._2
