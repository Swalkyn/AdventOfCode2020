import scala.io.Source
import scala.util.matching.Regex

val input = Source.fromFile("src/main/resources/day2/input.txt").getLines().toList
val passwordFormat = raw"(\d+)-(\d+) ([a-z]): ([a-z]+)".r

def validation1(min: Int, max: Int, c: Char, pw: String) = {
  val charCount = pw.count(_ == c)
  (charCount >= min.toInt) && (charCount <= max.toInt)
}

def validation2(a: Int, b: Int, c: Char, pw: String) = {
  val posA = pw(a - 1) == c
  val posB = pw(b - 1) == c
  (posA && !posB) || (!posA && posB)
}

def passwordValidation(validationFunc: (Int, Int, Char, String) => Boolean)(s: String): Boolean = s match {
  case passwordFormat(min, max, c, pw) => validationFunc(min.toInt, max.toInt, c.head, pw)
  case _ => throw new IllegalArgumentException(s"Bad format: $s")
}

input.map(passwordValidation(validation1)(_)).count(identity)
input.map(passwordValidation(validation2)(_)).count(identity)
