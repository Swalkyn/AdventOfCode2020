import scala.io.Source

val input = Source.fromFile("src/main/resources/day7/input.txt").getLines().toList

case class Graph[A](v: Set[A], e: Set[(A, A, Int)]) {
  def reverse = Graph(v, e.map { case (v1, v2, w) => (v2, v1, w) })

  lazy val adjList: Map[A, Set[A]] = e.groupMap(_._1)(_._2).withDefault(_ => Set[A]())
  lazy val weightedAdjList: Map[A, Set[(A, Int)]] = e.groupMap(_._1)(e => (e._2, e._3)).withDefault(_ => Set())

  def reachableFrom(vertex: A): Set[A] = {
    def reachableFromHelper(vertex: A, visited: Set[A]): Set[A] =
      if (visited.contains(vertex)) visited
      else adjList(vertex).foldLeft(visited + vertex)((acc, v) => reachableFromHelper(v, acc))

    reachableFromHelper(vertex, Set()) - vertex
  }

  def allPathsCostFrom(vertex: A): Int = {
    weightedAdjList(vertex).foldLeft(0){ case (acc, (v, w)) => acc + w * (1 + allPathsCostFrom(v)) }
  }
}

val bagR = raw"^\w+ \w+".r
val contentR = raw"(\d+) (\w+ \w+) bags?".r 

val (vertices, edges) = input.map { s =>
  val bag = bagR.findFirstIn(s).get
  val contents = (for (m <- contentR.findAllMatchIn(s)) yield (m.group(2), m.group(1).toInt)).toList
  (Set(bag), contents.map { case (v, w) => (bag, v, w) }.toSet) 
}.reduce { (x, y) => (x._1 ++ y._1, x._2 ++ y._2)}  

val g = Graph(vertices, edges)

g.reverse.reachableFrom("shiny gold").size
g.allPathsCostFrom("shiny gold")
