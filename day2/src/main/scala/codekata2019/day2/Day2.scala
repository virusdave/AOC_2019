package codekata2019.day2

import cats.syntax.option._
import scala.annotation.tailrec

object Day2 {
  val memory: Array[Int] = in.split(",").map(_.toInt).toArray

  sealed abstract class Op(val opcode: Int) {
    // Returns new IP if not halting
    protected def op(ip: Int): (Unit, Option[Int])
    def run(mem: Array[Int], ip: Int): Option[Int] = {
      m = mem  // Disgusting
      op(ip)._2
    }
    protected var m: Array[Int] = _
    protected def g: Int => Int = m.apply
    protected def s: (Int, Int) => Unit = m.update
  }
  case object Add extends Op(1) {
    def op(ip: Int) = (s(g(ip+3), g(g(ip+1)) + g(g(ip+2))), (ip+4).some)
  }
  case object Mul extends Op(2) {
    def op(ip: Int) = (s(g(ip+3), g(g(ip+1)) * g(g(ip+2))), (ip+4).some)
  }
  case object Halt extends Op(99) {
    def op(ip: Int) = ((), None)
  }
  object Op {
    val opcodes = Seq(Add, Mul, Halt).map { x => x.opcode -> x.run _ }.toMap
  }

  @tailrec
  def mutatingLoop(mem: Array[Int], ip: Option[Int]): Unit = {/*dump(mem);*/ ip match {
    case None =>
    case Some(ip) =>
      mutatingLoop(mem, Op.opcodes(mem(ip))(mem, ip))
  }}
  def dump(mem: Array[Int]) = println(s"Memory: ${mem.mkString(",")}")

  object Part1 {
    val mem = memory.clone()
    mem(1) = 12
    mem(2) = 2
    mutatingLoop(mem, 0.some)
    val solution = mem(0)
  }

  object Part2 {
    val solution = (for {
      x <- 1 until 100
      y <- 1 until 100
    } yield (x, y))
      .find { case (x, y) =>
          val mem = memory.clone()
          mem(1) = x
          mem(2) = y
          mutatingLoop(mem, 0.some)
          mem(0) == 19690720
      }
  }

  def in2:String = "1,9,10,3,2,3,11,0,99,30,40,50"
  def in3:String = "1,1,1,4,99,5,6,0,99"
  def in4:String = "2,4,4,5,99,0"
  // 5110675, (48,47)
  def in:String =
    """1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,5,19,23,2,9,23,27,1,27,5,31,2,31,13,35,1,35,9,39,1,39,10,43,2,43,9,47,1,47,5,51,2,13,51,55,1,9,55,59,1,5,59,63,2,6,63,67,1,5,67,71,1,6,71,75,2,9,75,79,1,79,13,83,1,83,13,87,1,87,5,91,1,6,91,95,2,95,13,99,2,13,99,103,1,5,103,107,1,107,10,111,1,111,13,115,1,10,115,119,1,9,119,123,2,6,123,127,1,5,127,131,2,6,131,135,1,135,2,139,1,139,9,0,99,2,14,0,0""".stripMargin

}
