import scala.io.Source

val input = Source.fromFile("src/main/resources/day4/input.txt").getLines().toList

val field = raw"([a-z]+):(\S+)".r
val hgtin = raw"(\d{2})in".r
val hgtcm = raw"(\d{3})cm".r
def fourDigitsRange(s: String, min: Int, max: Int): Boolean = {
  val n = raw"(\d{4})".r.findFirstIn(s).getOrElse("-1").toInt
  (n >= min) && (n <= max)
}
val fieldsValidation = Map(
  "ecl" -> ((s: String) => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(s)),
  "pid" -> ((s: String) => raw"\d{9}".r.matches(s)),
  "hcl" -> ((s: String) => raw"#[a-f0-9]{6}".r.matches(s)),
  "byr" -> ((s: String) => fourDigitsRange(s, 1920, 2002)),
  "iyr" -> ((s: String) => fourDigitsRange(s, 2010, 2020)),
  "eyr" -> ((s: String) => fourDigitsRange(s, 2020, 2030)),
  "hgt" -> ((s: String) => s match {
    case hgtcm(h) => (h.toInt >= 150) && (h.toInt <= 193)
    case hgtin(h) => (h.toInt >= 59) && (h.toInt <= 76)
    case _ => false
  }),
  "cid" -> ((s: String) => true)
)
val mandatoryFields = fieldsValidation.keySet - "cid"

def groupPassFields(input: List[String], acc: List[String]): List[String] = {
  val (fields, more) = input.span(_.nonEmpty)
  val concat = fields.mkString(" ")
  if (more.isEmpty) concat :: acc
  else groupPassFields(more.tail, concat :: acc)
}

def isPassValid(pass: String): Boolean = {
  val fieldSet = (for (m <- field.findAllMatchIn(pass)) yield m.group(1)).toSet
  mandatoryFields.subsetOf(fieldSet)
}

def isPassValid2(pass: String): Boolean = {
  val fieldSet = (for (m <- field.findAllMatchIn(pass) if fieldsValidation(m.group(1))(m.group(2))) yield m.group(1)).toSet
  mandatoryFields.subsetOf(fieldSet)
}


val passports = groupPassFields(input, List())
passports.map(isPassValid).count(identity)
passports(4)
passports.map(isPassValid2).count(identity)
