package codekata2019.day9

import codekata2019.common._
import codekata2019.common.intcode.machineio._
import enumeratum._
import shapeless.tag
import shapeless.tag.@@
import zio._
import zio.console._

object Day9 {

  type Number = BigInt
  type Address = Int
  sealed trait MemoryTag

  type Memory = Vector[Number] @@ MemoryTag

  type EnvReq = Console with MachineIO
  type ZZZ[A] = ZIO[EnvReq, Error, A]

  case class MachineState(mem: Memory, ip: Address, relativeBase: Address, halted: Boolean = false)

  implicit class _TaggedIntVectorOps[B](in: Vector[Number] with tag.Tagged[B]) {
    def at[E >: BadPtr <: Error](
                                    index: Address,
                                    onError: (Address, Int) => E = BadPtr(_: Address, _: Int))
    : ZIO[EnvReq, E, Number] =
      (if (index < in.size)
        ZIO.effect(in(index))
      else
        ZIO.succeed(BigInt(0))
        )
        .orElseFail(onError(index, in.size))

    def replace(index: Address, v: Number): ZIO[EnvReq, BadPtr, Vector[Number] @@ B] = {
      ZIO.effect(tag[B](
        (if (index < in.size)
          in
        else
          in.appendedAll(Vector.fill(index - in.size + 1)(BigInt(0)))
          )
          .updated(index, v))).orElseFail(BadPtr(index, in.size, s"Writing value $v".some))
    }
  }

  // Move these elsewhere perhaps
  sealed trait Error
  case class BadOpcode(ip: Address, code: Number) extends Error
  case class BadPtr(ptr: Address, memSize: Int, details: Option[String] = None) extends Error
  case class BadArgIndex(index: Address, argSize: Int) extends Error
  case class BadArgMode(mode: Number, opcode: Number, argIndex: Int) extends Error
  case class BadArgPtr(arg: Arg) extends Error
  case class BadPtrSize(ptr: Number, reason: String) extends Error
  case class BadImmediateModeWrite(arg: Arg.ImmediateMode) extends Error
  case object InputExhausted extends Error


  // Addressing modes
  sealed trait Arg extends EnumEntry {
    def value(state: MachineState): ZZZ[Number]
    def deref(state: MachineState): ZZZ[Address]
  }

  object Arg extends Enum[Arg] {

    case class PositionMode(n: Number) extends Arg {
      override def value(state: MachineState): ZZZ[Number] = deref(state).flatMap(state.mem.at(_))
      override def deref(state: MachineState): ZZZ[Address] = ZIO.effect(n.toInt).orElseFail(BadArgPtr(this))
    }

    case class ImmediateMode(n: Number) extends Arg {
      override def value(state: MachineState): ZZZ[Number] = ZIO.succeed(n)
      override def deref(state: MachineState): ZZZ[Address] = ZIO.fail(BadImmediateModeWrite(this))
    }

    case class RelativeMode(n: Number) extends Arg {
      override def value(state: MachineState): ZZZ[Number] = deref(state).flatMap(state.mem.at(_))
      override def deref(state: MachineState): ZZZ[Address] =
        ZIO.effect(state.relativeBase + n.toInt).orElseFail(BadArgPtr(this))
    }

    override def values: IndexedSeq[Arg] = findValues

    // TODO(Dave): Ugh, this is fuckin' ugly.  There's got to be a better way to do this.
    lazy val modes: Map[Int, Number => Arg] = Map(
      0 -> PositionMode,
      1 -> ImmediateMode,
      2 -> RelativeMode,
    )
  }


  // Instructions
  type Args = Vector[Arg]

  sealed trait Instruction extends EnumEntry {
    def opcode: Int
    val argSize: Int
    def opSize: Int = argSize + 1 // #args + opcode itself, by default

    /**
      * Decode the `argSize` values following the instruction itself into [[Arg]]s.
      * Assumptions: The last argument is likely to be a "destination".  Despite the
      * name, this is treated as a [[Arg.ImmediateMode]], so that when we write the results of
      * the operation into memory, *where* we write to is directly the value (address)
      * we read in the destination position.  This is because writes to [[Memory]] are
      * inherently writes via pointer (memory address).  Yeah, it's kinda weird like that.
      * Yuck.
      */
    def decodeArgs(code: Number, state: MachineState): ZZZ[Args] =
      ZIO.foreach((1 to argSize).toVector) { idx =>
        val mode = (code / 100 / Math.pow(10, idx - 1).toInt % 10).toInt
        for {
          makeArg <- ZIO.fromOption(Arg.modes.get(mode))
            .orElseFail(BadArgMode(mode = mode, opcode = code, argIndex = idx))
          argVal <- state.mem.at(state.ip + idx)
          arg = makeArg(argVal)
          _ <- putStrLn(s"Creating arg ${arg} from opcode ${code}, arg index ${idx}")
        } yield arg
      }

    // Most instructions can just implement `impl`, unless they're implementing some kind of jump, in which
    // case they probably want to override `exec` instead.
    protected def impl(state: MachineState, args: Args): ZZZ[MachineState]

    def exec(state: MachineState, args: Args): ZZZ[MachineState] =
      impl(state, args).map(_.copy(ip = state.ip + opSize))
  }

  object Instruction extends Enum[Instruction] {

    case object Halt extends Instruction {
      override def opcode = 99
      override val argSize = 0

      override protected def impl(state: MachineState, args: Args): ZZZ[MachineState] = ???

      override def exec(state: MachineState, args: Args): ZIO[Console, Error, MachineState] =
        ZIO.succeed(state.copy(halted = true))
    }

    trait ArityMemUpdateOp extends Instruction {
      def reduce(mem: Memory, in: IndexedSeq[Number]): ZZZ[Number]

      override protected def impl(state: MachineState, args: Args): ZZZ[MachineState] = {
        for {
          resolvedArgs <- ZIO.foreach(args)(_.value(state))
          result <- reduce(mem = state.mem, in = resolvedArgs.dropRight(1))
          _ <- putStrLn(s"Resolved all args to ${resolvedArgs}, reduced to ${result}")
          dest <- args.last.deref(state)
          newMem <- state.mem.replace(dest, result)
        } yield state.copy(mem = newMem)
      }
    }

    case object Add extends ArityMemUpdateOp {
      override def opcode = 1
      override val argSize = 3
      override def reduce(mem: Memory, in: IndexedSeq[Number]): ZZZ[Number] = ZIO.succeed(in.sum)
    }

    case object Mul extends ArityMemUpdateOp {
      override def opcode = 2
      override val argSize = 3
      override def reduce(mem: Memory, in: IndexedSeq[Number]): ZZZ[Number] = ZIO.succeed(in.product)
    }

    case object In extends Instruction {
      override def opcode = 3
      override val argSize = 1

      override protected def impl(state: MachineState, args: Args): ZZZ[MachineState] = {
        for {
          result <- input().orElseFail(InputExhausted)
          dest <- args.last.deref(state)
          newMem <- state.mem.replace(dest, result)
        } yield state.copy(mem = newMem)
      }
    }

    case object Out extends Instruction {
      override def opcode = 4
      override val argSize = 1

      override protected def impl(state: MachineState, args: Args): ZZZ[MachineState] = {
        for {
          resolvedArgs <- ZIO.foreach(args)(_.value(state))
          _ <- output(resolvedArgs.head)
        } yield state
      }
    }

    trait ConditionalJump extends Instruction {
      def jumpTo(mem: Memory, in: IndexedSeq[Number]): ZZZ[Option[Address]]

      override protected def impl(state: MachineState, args: Args): ZZZ[MachineState] = ???

      override def exec(state: MachineState, args: Args): ZZZ[MachineState] = {
        for {
          resolvedArgs <- ZIO.foreach(args)(_.value(state))
          maybeDest <- jumpTo(state.mem, resolvedArgs)
        } yield state.copy(ip = maybeDest.getOrElse(state.ip + opSize))
      }
    }

    case object JiT extends ConditionalJump {
      override def opcode = 5
      override val argSize = 2

      override def jumpTo(mem: Memory, in: IndexedSeq[Number]): ZZZ[Option[Address]] =
        if (in(0) > 0) ZIO.effect(in(1).toInt.some).orElseFail(BadPtrSize(in(1), "Jump dest")) else ZIO.none
    }

    case object JiF extends ConditionalJump {
      override def opcode = 6
      override val argSize = 2

      override def jumpTo(mem: Memory, in: IndexedSeq[Number]): ZZZ[Option[Address]] =
        if (in(0) == 0) ZIO.effect(in(1).toInt.some).orElseFail(BadPtrSize(in(1), "Jump dest")) else ZIO.none
    }

    case object LT extends ArityMemUpdateOp {
      override def opcode = 7
      override val argSize = 3

      override def reduce(mem: Memory, in: IndexedSeq[Number]): ZZZ[Number] = ZIO.succeed(if (in(0) < in(1)) 1 else 0)
    }

    case object EQ extends ArityMemUpdateOp {
      override def opcode = 8
      override val argSize = 3

      override def reduce(mem: Memory, in: IndexedSeq[Number]): ZZZ[Number] = ZIO.succeed(if (in(0) == in(1)) 1 else 0)
    }

    case object ARB extends Instruction {
      override def opcode = 9
      override val argSize = 1

      override protected def impl(state: MachineState, args: Args): ZZZ[MachineState] =
        for {
          mod <- args.head.value(state)
        } yield state.copy(relativeBase = state.relativeBase + mod.toInt)
    }

    override def values: IndexedSeq[Instruction] = findValues
  }

  val ops: Map[Int, Instruction] = Instruction.values.map(op => op.opcode -> op).toMap

  private def step(state: MachineState): ZZZ[MachineState] = {
    for {
      // Decode instruction
      code <- state.mem.at(state.ip).mapError(_.copy(details = "Bad IP".some))
      op <- ZIO.getOrFail(ops.get((code % 100).toInt)).orElseFail(BadOpcode(state.ip, code))
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
    private val mem = tag[MemoryTag](in.split(",").map(BigInt.apply).toVector)

    def solution(silent: Boolean): ZIO[ZEnv, Error, MachineState] = {
      run(MachineState(mem = mem, ip = 0, relativeBase = 0))
        .shush(true)
        .provideCustomLayer(MachineIO.withInputs(Vector(1)))
    }
  }

  object Part2 {
    private val mem = tag[MemoryTag](in.split(",").map(BigInt.apply).toVector)

    def solution(silent: Boolean): ZIO[ZEnv, Error, MachineState] = {
      run(MachineState(mem = mem, ip = 0, relativeBase = 0))
        .shush(true)
        .provideCustomLayer(MachineIO.withInputs(Vector(2)))
    }
  }

  def in2: String = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
  def in3: String = "1102,34915192,34915192,7,4,7,99,0"
  def in4: String = "104,1125899906842624,99"

  // 5110675, (48,47)
  def in: String =
    """1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,1,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,0,0,1020,1101,34,0,1004,1101,0,26,1008,1102,1,37,1011,1101,39,0,1018,1102,587,1,1022,1101,1,0,1021,1102,22,1,1012,1101,0,33,1014,1101,24,0,1016,1101,0,752,1029,1101,36,0,1002,1101,35,0,1006,1101,32,0,1009,1102,38,1,1003,1102,584,1,1023,1101,0,20,1001,1102,892,1,1025,1102,29,1,1000,1101,411,0,1026,1102,1,901,1024,1101,0,761,1028,1101,23,0,1017,1102,30,1,1013,1101,0,27,1015,1102,28,1,1005,1101,408,0,1027,1101,25,0,1007,1102,31,1,1019,1101,0,21,1010,109,5,1207,-2,39,63,1005,63,199,4,187,1105,1,203,1001,64,1,64,1002,64,2,64,109,12,21102,40,1,-1,1008,1016,40,63,1005,63,229,4,209,1001,64,1,64,1106,0,229,1002,64,2,64,109,-5,1207,-5,24,63,1005,63,249,1001,64,1,64,1106,0,251,4,235,1002,64,2,64,109,-14,2102,1,6,63,1008,63,32,63,1005,63,271,1106,0,277,4,257,1001,64,1,64,1002,64,2,64,109,2,1202,1,1,63,1008,63,20,63,1005,63,303,4,283,1001,64,1,64,1106,0,303,1002,64,2,64,109,7,2108,34,2,63,1005,63,319,1106,0,325,4,309,1001,64,1,64,1002,64,2,64,109,6,2101,0,-6,63,1008,63,24,63,1005,63,349,1001,64,1,64,1105,1,351,4,331,1002,64,2,64,109,4,21107,41,42,0,1005,1017,369,4,357,1105,1,373,1001,64,1,64,1002,64,2,64,109,5,21101,42,0,-5,1008,1017,41,63,1005,63,397,1001,64,1,64,1106,0,399,4,379,1002,64,2,64,109,9,2106,0,-4,1106,0,417,4,405,1001,64,1,64,1002,64,2,64,109,-20,21108,43,43,0,1005,1011,435,4,423,1105,1,439,1001,64,1,64,1002,64,2,64,109,-15,2102,1,8,63,1008,63,34,63,1005,63,465,4,445,1001,64,1,64,1105,1,465,1002,64,2,64,109,3,1201,6,0,63,1008,63,28,63,1005,63,491,4,471,1001,64,1,64,1106,0,491,1002,64,2,64,109,18,21108,44,46,0,1005,1017,511,1001,64,1,64,1106,0,513,4,497,1002,64,2,64,109,12,1205,-8,527,4,519,1105,1,531,1001,64,1,64,1002,64,2,64,109,-17,1208,-3,32,63,1005,63,553,4,537,1001,64,1,64,1105,1,553,1002,64,2,64,109,-13,1208,10,31,63,1005,63,573,1001,64,1,64,1105,1,575,4,559,1002,64,2,64,109,17,2105,1,7,1105,1,593,4,581,1001,64,1,64,1002,64,2,64,109,-8,2107,19,-7,63,1005,63,615,4,599,1001,64,1,64,1105,1,615,1002,64,2,64,109,4,1206,8,629,4,621,1106,0,633,1001,64,1,64,1002,64,2,64,109,-2,2101,0,-6,63,1008,63,34,63,1005,63,655,4,639,1105,1,659,1001,64,1,64,1002,64,2,64,109,10,1205,0,671,1105,1,677,4,665,1001,64,1,64,1002,64,2,64,109,-21,2107,26,8,63,1005,63,693,1106,0,699,4,683,1001,64,1,64,1002,64,2,64,109,19,1201,-9,0,63,1008,63,30,63,1005,63,719,1105,1,725,4,705,1001,64,1,64,1002,64,2,64,109,9,1206,-6,741,1001,64,1,64,1106,0,743,4,731,1002,64,2,64,109,-5,2106,0,6,4,749,1001,64,1,64,1105,1,761,1002,64,2,64,109,-14,1202,-1,1,63,1008,63,27,63,1005,63,781,1105,1,787,4,767,1001,64,1,64,1002,64,2,64,109,1,21107,45,44,5,1005,1014,807,1001,64,1,64,1105,1,809,4,793,1002,64,2,64,109,8,21101,46,0,0,1008,1017,46,63,1005,63,835,4,815,1001,64,1,64,1106,0,835,1002,64,2,64,109,-26,2108,20,10,63,1005,63,857,4,841,1001,64,1,64,1106,0,857,1002,64,2,64,109,24,21102,47,1,-5,1008,1010,46,63,1005,63,881,1001,64,1,64,1106,0,883,4,863,1002,64,2,64,109,6,2105,1,3,4,889,1001,64,1,64,1105,1,901,4,64,99,21102,27,1,1,21101,915,0,0,1105,1,922,21201,1,29830,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,0,942,0,1105,1,922,21202,1,1,-1,21201,-2,-3,1,21102,1,957,0,1105,1,922,22201,1,-1,-2,1105,1,968,21201,-2,0,-2,109,-3,2106,0,0""".stripMargin

}
