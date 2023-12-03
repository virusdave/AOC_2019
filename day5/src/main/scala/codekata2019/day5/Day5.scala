package codekata2019.day5

import codekata2019.common._
import codekata2019.common.intcode.machineio._
import enumeratum._
import shapeless.tag
import shapeless.tag.@@
import zio._
import zio.console._

object Day5 {
  sealed trait MemoryTag
  type Memory = Vector[Int] @@ MemoryTag

  type EnvReq = Console with MachineIO
  type ZZZ[A] = ZIO[EnvReq, Error, A]

  case class MachineState(mem: Memory, ip: Int, halted: Boolean = false)

  implicit class _TaggedVectorOps[A, B](in: Vector[A] with tag.Tagged[B]) {
    def at[E >: BadPtr <: Error](index: Int, onError: (Int, Int) => E = BadPtr(_: Int, _: Int)): ZIO[EnvReq, E, A] =
      ZIO.effect(in(index)).orElseFail(onError(index, in.size))
    def replace(index: Int, v: A): ZIO[EnvReq, BadPtr, Vector[A] @@ B] =
      ZIO.effect(tag[B](in.updated(index, v))).orElseFail(BadPtr(index, in.size, s"Writing value $v".some))
  }

  // Move these elsewhere perhaps
  sealed trait Error
  case class BadOpcode(ip: Int, code: Int) extends Error
  case class BadPtr(ptr: Int, memSize: Int, details: Option[String] = None) extends Error
  case class BadArgIndex(index: Int, argSize: Int) extends Error
  case class BadArgMode(mode: Int, opcode: Int, argIndex: Int) extends Error
  case object InputExhausted extends Error


  val initialMemory: Vector[Int] @@ MemoryTag = tag[MemoryTag](in.split(",").map(_.toInt).toVector)


  // Addressing modes
  sealed trait Arg extends EnumEntry {
    def value(mem: Memory): ZZZ[Int]
  }
  object Arg extends Enum[Arg] {

    case class PositionMode(n: Int) extends Arg {
      override def value(mem: Memory): ZZZ[Int] = mem.at(n)
    }

    case class ImmediateMode(n: Int) extends Arg {
      override def value(mem: Memory): ZZZ[Int] = ZIO.succeed(n)
    }

    override def values: IndexedSeq[Arg] = findValues
    // TODO(Dave): Ugh, this is fuckin' ugly.  There's got to be a better way to do this.
    lazy val modes: Map[Int, Int => Arg] = Map(
      0 -> PositionMode,
      1 -> ImmediateMode,
    )
  }


  // Instructions
  type Args = Vector[Arg]
  sealed trait Instruction extends EnumEntry {
    def opcode: Int
    val argSize: Int

    def opSize: Int = argSize + 1  // #args + opcode itself, by default
    def destArg: Option[Int] = Some(argSize)

    /**
      * Decode the `argSize` values following the instruction itself into [[Arg]]s.
      * Assumptions: The last argument is likely to be a "destination".  Despite the
      * name, this is treated as a [[Arg.ImmediateMode]], so that when we write the results of
      * the operation into memory, *where* we write to is directly the value (address)
      * we read in the destination position.  This is because writes to [[Memory]] are
      * inherently writes via pointer (memory address).  Yeah, it's kinda weird like that.
      * Yuck.
      */
    def decodeArgs(code: Int, state: MachineState): ZZZ[Args] =
      ZIO.foreach((1 to argSize).toVector) { idx =>
        if (destArg.contains(idx)) {
          state.mem.at(state.ip + idx).map(Arg.ImmediateMode)
        } else {
          val mode = code / 100 / Math.pow(10, idx - 1).toInt % 10
          for {
            makeArg <- ZIO.fromOption(Arg.modes.get(mode))
              .orElseFail(BadArgMode(mode = mode, opcode = code, argIndex = idx))
            argVal <- state.mem.at(state.ip + idx)
            arg = makeArg(argVal)
            _ <- putStrLn(s"Creating arg ${arg} from opcode ${code}, arg index ${idx}")
          } yield arg
        }
      }

    // Most instructions can just implement `impl`, unless they're implementing some kind of jump, in which
    // case they probably want to override `exec` instead.
    protected[this] def impl(mem: Memory, args: Args): ZZZ[Memory]
    def exec(state: MachineState, args: Args): ZZZ[MachineState] =
      impl(state.mem, args).map(MachineState(_, ip = state.ip + opSize))
  }

  object Instruction extends Enum[Instruction] {
    case object Halt extends Instruction {
      override def opcode: Int = 99
      override val argSize = 0
      override val destArg: Option[Int] = None

      override protected[this] def impl(mem: Memory, args: Args): ZIO[Console, Error, Memory] = ???
      override def exec(state: MachineState, args: Args): ZIO[Console, Error, MachineState] =
        ZIO.succeed(state.copy(halted = true))
    }

    trait ArityMemUpdateOp extends Instruction {
      def reduce(mem: Memory, in: IndexedSeq[Int]): ZZZ[Int]

      override protected[this] def impl(mem: Memory, args: Args): ZZZ[Memory] = {
        for {
          resolvedArgs <- ZIO.foreach(args)(_.value(mem))
          result <- reduce(mem = mem, in = resolvedArgs.dropRight(1))
          _ <- putStrLn(s"Resolved all args to ${resolvedArgs}, reduced to ${result}")
          newMem <- mem.replace(resolvedArgs.last, result)
        } yield newMem
      }
    }

    case object Add extends ArityMemUpdateOp {
      override def opcode: Int = 1
      override val argSize = 3
      override def reduce(mem: Memory, in: IndexedSeq[Int]): ZZZ[Int] = ZIO.succeed(in.sum)
    }

    case object Mul extends ArityMemUpdateOp {
      override def opcode: Int = 2
      override val argSize = 3
      override def reduce(mem: Memory, in: IndexedSeq[Int]): ZZZ[Int] = ZIO.succeed(in.product)
    }

    case object In extends Instruction {
      override def opcode: Int = 3
      override val argSize: Int = 1

      override protected[this] def impl(mem: Memory, args: Args): ZZZ[Memory] = {
        for {
          resolvedArgs <- ZIO.foreach(args)(_.value(mem))
          result <- input().orElseFail(InputExhausted)
          newMem <- mem.replace(resolvedArgs.last, result)
        } yield newMem
      }
    }

    case object Out extends Instruction {
      override def opcode: Int = 4
      override val argSize: Int = 1
      override val destArg: Option[Int] = None

      override protected[this] def impl(mem: Memory, args: Args): ZZZ[Memory] = {
        for {
          resolvedArgs <- ZIO.foreach(args)(_.value(mem))
          _ <- output(resolvedArgs.head)
        } yield mem
      }
    }

    trait ConditionalJump extends Instruction {
      def jumpTo(mem: Memory, in: IndexedSeq[Int]): ZZZ[Option[Int]]

      override val destArg: Option[Int] = None
      override protected[this] def impl(mem: Memory, args: Args): ZZZ[Memory] = ???
      override def exec(state: MachineState, args: Args): ZZZ[MachineState] = {
        for {
          resolvedArgs <- ZIO.foreach(args)(_.value(state.mem))
          maybeDest <- jumpTo(state.mem, resolvedArgs)
        } yield state.copy(ip = maybeDest.getOrElse(state.ip + opSize))
      }
    }

    case object JiT extends ConditionalJump {
      override def opcode: Int = 5
      override val argSize: Int = 2

      override def jumpTo(mem: Memory, in: IndexedSeq[Int]): ZZZ[Option[Int]] =
        if (in(0) > 0) ZIO.succeed(in(1).some) else ZIO.none
    }

    case object JiF extends ConditionalJump {
      override def opcode: Int = 6
      override val argSize: Int = 2

      override def jumpTo(mem: Memory, in: IndexedSeq[Int]): ZZZ[Option[Int]] =
        if (in(0) == 0) ZIO.succeed(in(1).some) else ZIO.none
    }

    case object LT extends ArityMemUpdateOp {
      override def opcode: Int = 7
      override val argSize: Int = 3

      override def reduce(mem: Memory, in: IndexedSeq[Int]): ZZZ[Int] = ZIO.succeed(if (in(0) < in(1)) 1 else 0)
    }

    case object EQ extends ArityMemUpdateOp {
      override def opcode: Int = 8
      override val argSize: Int = 3

      override def reduce(mem: Memory, in: IndexedSeq[Int]): ZZZ[Int] = ZIO.succeed(if (in(0) == in(1)) 1 else 0)
    }

    override def values: IndexedSeq[Instruction] = findValues
  }

  val ops: Map[Int, Instruction] = Instruction.values.map(op => op.opcode -> op).toMap

  private def step(state: MachineState): ZZZ[MachineState] = {
    for {
      // Decode instruction
      code <- state.mem.at(state.ip).mapError(_.copy(details = "Bad IP".some))
      op <- ZIO.getOrFail(ops.get(code % 100)).orElseFail(BadOpcode(state.ip, code))
      // Decode arguments
      args <- op.decodeArgs(code, state)
      _ <- putStrLn(s"Decoded ${op} with args ${args}")
      // Execute the instruction
      newState <- op.exec(state, args)
    } yield newState
  }

  private def dump(state: MachineState) = console.putStrLn(s"Machine state: $state")
  private def run(state: MachineState): ZZZ[MachineState] =
    dump(state) *>
    ZIO.iterate(state)(!_.halted)(
      (step _).andThen(_.tap(dump))
    )

  object Part1 {
    private val mem = initialMemory
    def solution(silent: Boolean): ZIO[ZEnv, Error, MachineState] = {
//      (for {
//        m1 <- mem.replace(1, 12)
//        m2 <- m1.replace(2, 2)
//        out <- run(MachineState(m2, 0))
//      } yield out).shush(silent)
      run(MachineState(mem, 0)).provideCustomLayer(MachineIO.withInputs(Vector(1)))
    }
  }

  object Part2 {
    private val mem = initialMemory
    def solution(silent: Boolean): ZIO[ZEnv, Error, MachineState] = {
      run(MachineState(mem, 0)).shush(silent).provideCustomLayer(MachineIO.withInputs(Vector(5)))
    }
  }

  def in2:String = "1,9,10,3,2,3,11,0,99,30,40,50"
  def in3:String = "1,1,1,4,99,5,6,0,99"
  def in4:String = "2,4,4,5,99,0"
  // 5110675, (48,47)
  def in:String =
    """3,225,1,225,6,6,1100,1,238,225,104,0,1101,9,90,224,1001,224,-99,224,4,224,102,8,223,223,1001,224,6,224,1,223,224,223,1102,26,62,225,1101,11,75,225,1101,90,43,225,2,70,35,224,101,-1716,224,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1101,94,66,225,1102,65,89,225,101,53,144,224,101,-134,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1102,16,32,224,101,-512,224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,1001,43,57,224,101,-147,224,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,36,81,225,1002,39,9,224,1001,224,-99,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1,213,218,224,1001,224,-98,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,102,21,74,224,101,-1869,224,224,4,224,102,8,223,223,1001,224,7,224,1,224,223,223,1101,25,15,225,1101,64,73,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,226,677,224,1002,223,2,223,1005,224,329,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,344,101,1,223,223,108,226,677,224,102,2,223,223,1006,224,359,101,1,223,223,108,226,226,224,1002,223,2,223,1005,224,374,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,389,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,404,1001,223,1,223,107,677,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,677,677,224,102,2,223,223,1006,224,434,101,1,223,223,1107,226,677,224,102,2,223,223,1005,224,449,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,464,101,1,223,223,107,226,677,224,102,2,223,223,1005,224,479,1001,223,1,223,8,677,226,224,102,2,223,223,1005,224,494,1001,223,1,223,1108,226,677,224,102,2,223,223,1006,224,509,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,524,101,1,223,223,1008,226,226,224,1002,223,2,223,1005,224,539,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,554,101,1,223,223,1107,677,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,584,101,1,223,223,1108,677,677,224,102,2,223,223,1005,224,599,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,614,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,629,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,644,101,1,223,223,1007,226,677,224,102,2,223,223,1005,224,659,1001,223,1,223,1108,677,226,224,102,2,223,223,1006,224,674,101,1,223,223,4,223,99,226""".stripMargin

}
