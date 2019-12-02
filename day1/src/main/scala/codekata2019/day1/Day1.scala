package codekata2019.day1

//import cats.syntax.monoid._
//import cats.instances.list._
import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty

object Day1 {
  // Convert the input into a sequence of signed integers by splitting on newlines, trimming whitespace,
  // and removing empty entries.
  val inputLines = Day1.in.split("\n").map(_.replaceAll(" ", "")).filter(_.nonEmpty).map(_.toInt)

  object Part1 {
    // Duh.  Just sum the integers
    val solution = inputLines.map(_ / 3 - 2).sum
  }

  /**
   * Tricky, because each "module" needs to be handled independantly, not collectively.
   * Thus, two modules of "6" and "7" require 0 fuel ((6/3-2) + (7/3-2)), whereas a
   * single module of "13" requires some fuel (13/3-2).  This is exacerbated by the
   * "fuel needs more fuel" part.
   */
  object Part2 {
    def fuel(weight: Int) = Math.max(weight / 3 - 2, 0)
    @tailrec
    def fuelForFuel(soFar: Int, lastFuel: Int): Int = {
      if (lastFuel == 0) soFar
      else {
        val newFuel = fuel(lastFuel)
        fuelForFuel(soFar + newFuel, newFuel)
      }
    }
    val solution = inputLines.map(m => fuelForFuel(fuel(m), fuel(m))).sum
  }

  lazy val in: String =
    // Puzzle input goes here.
    """
    """.stripMargin
}
