package codekata2019.common.intcode

import scala.{ Console => SConsole }
import zio._
import zio.console.Console

package object machineio {
  type MachineIO = Has[MachineIO.Service]

  object MachineIO {
    case object NoMoreInput

    trait Service {
      def input(): IO[NoMoreInput.type, Int]
      def output(v: BigInt): UIO[Unit]
    }
    object Service {
      def withInputsAndConsole(inputs: Vector[Int], console: Console): UIO[Service] =
        Ref.make(inputs).map { r =>
          new Service {
            override def input(): IO[NoMoreInput.type, Int] =
              r.getAndUpdate(_.drop(1)).map(_.headOption).someOrFail(NoMoreInput)

            override def output(v: BigInt): UIO[Unit] = {
              import SConsole._
              console.get.putStrLn(s"$BOLD${BLUE}Output:${RESET} ${RED}${BOLD}${v}${RESET}")
            }
          }
        }
    }
    def withInputs(inputs: Vector[Int]): ZLayer[Console, Nothing, MachineIO] =
      ZLayer.fromFunctionM(Service.withInputsAndConsole(inputs, _))
  }

  def input(): ZIO[MachineIO, MachineIO.NoMoreInput.type, Int] = ZIO.accessM(_.get.input())
  def output(v: BigInt): URIO[MachineIO, Unit] = ZIO.accessM(_.get.output(v))
}
