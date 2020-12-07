import scala.io.Source

val input = Source.fromFile("src/main/resources/day6/input.txt").getLines().toList :+ ""

input.foldLeft((Set[Char](), 0)) { case ((set, count), answers) =>
  if (answers.isEmpty) (Set(), count + set.size)
  else (set ++ answers.toSet, count)
}._2

val allQuestions = "abcdefghijklmnopqrstuvwxyz".toSet

input.foldLeft((allQuestions, 0)) { case ((set, count), answers) =>
  if (answers.isEmpty) (allQuestions, count + set.size)
  else (set & answers.toSet, count)
}._2
