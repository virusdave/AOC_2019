package codekata2019.all

import codekata2019.common._
import codekata2019.day11.Puzzle
import zio._
import zio.console._


object All extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = (for {
    p1 <- Puzzle.Part1.solution.shush(silent = false)
    _ <- putStrLn(s" Part 1: ${p1}")
    p2 <- Puzzle.Part2.solution.shush(silent = true)
    _ <- putStrLn(s" Part 2: ${p2}")
  } yield ()).exitCode
}
