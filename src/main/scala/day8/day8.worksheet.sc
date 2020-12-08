import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromFile("src/main/resources/day8/input.txt").getLines().toList

type Program = Map[Int, Instr]

trait Instr
case class Acc(i: Int) extends Instr
case class Jmp(i: Int) extends Instr
case class Nop(i: Int) extends Instr

trait Termination
case class Loop(acc: Int) extends Termination
case class End(acc: Int) extends Termination
case object SegFault extends Termination

val txtToInstr = Map(
  "acc" -> ((i: Int) => Acc(i)),
  "jmp" -> ((i: Int) => Jmp(i)),
  "nop" -> ((i: Int) => Nop(i))
)

val program: Program = (for {
  s <- input
  constr <- txtToInstr.get(s.take(3))
} yield constr(s.drop(4).toInt)).zipWithIndex.map(_.swap).toMap


def run(program: Program): Termination = {
  val lastIndex = program.size - 1

  @tailrec
  def step(pc: Int, acc: Int, ran: Set[Int]): Termination =
    if (ran.contains(pc)) Loop(acc)
    else program.get(pc) match {
      case None => SegFault
      case Some(instr) => 
        val (pc2, acc2, ran2) = instr match {
          case Acc(i) => (pc + 1, acc + i, ran + pc)
          case Jmp(i) => (pc + i, acc, ran + pc)
          case Nop(_) => (pc + 1, acc, ran + pc)
        }
        if (pc == lastIndex && pc2 == pc + 1) End(acc)
        else step(pc2, acc2, ran2)
      }
  
  step(0, 0, Set())
}

run(program)

def nopJmpFlip(program: Program, i: Int): Option[Program] = program.get(i).flatMap {
  case Jmp(j) => Some(program.updated(i, Nop(j)))
  case Nop(j) => Some(program.updated(i, Jmp(j)))
  case _ => None
}

lazy val alteredPrograms = LazyList.range(0, program.size - 1).flatMap(nopJmpFlip(program, _)).map(run)
alteredPrograms.collectFirst{ case End(acc) => acc }
