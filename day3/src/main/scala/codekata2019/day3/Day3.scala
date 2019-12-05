package codekata2019.day3

import cats.instances.all._
import cats.syntax.all._
import breeze.math._
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup, Monoid, Semigroup, Semilattice}

object Day3 {
  val dirMap: Map[Char, Complex] =
    Seq('U', 'L', 'D', 'R').scanLeft(('_', Complex.one)) { case ((_, d), c) => (c, d * i)}.tail.toMap
  val inputs =
    in.lines.filter(_.trim.nonEmpty).map(_.split(',').map { a => dirMap(a.head) -> a.tail.toInt }).toList

  val locs = inputs.map(_.foldLeft(Complex.zero, Seq(Seq.empty[Complex])) { case ((pos, paths), (dir, dist)) =>
    // new pos, add path to paths
    (pos + dist * dir, (1 to dist).map(pos + _ * dir) +: paths)
  }).map { case (pos, rpaths) => rpaths.reverse.flatten.toList }

  object Part1 {
    val solution = {
      val poses = locs.map(_.toSet)
      val crosses = poses.reduce(_.intersect(_))

      // Manhattan distance from origin
      def md(x: Complex): Double = x.real.abs + x.imag.abs

      (crosses.map(md).min, crosses.toSeq)
    }
  }

  object Part2 {
    private val locsToFirstPathOccurance =
      locs.map{ l =>
        // There should be a simpler way to get the Int `meet semilattice` :(
        implicit val minInt: CommutativeSemigroup[Int] = (x: Int, y: Int) => Math.min(x, y)
        // NB: Need to include the origin as the start of the path length, despite excluding it from
        // the candidate intersections!  Thus we'll temporarily include the origin to the paths to
        // get the correct 'min path distance' to each position we reach on the path.
        (Complex.zero +: l)
          .zipWithIndex
          .map(Map(_))
          .tail  // Drop the origin we temporarily added for path lengths
          .combineAll(catsKernelStdCommutativeMonoidForMap(minInt))
      }
    private val intersectionsToSummedMinPathLength = {
      val intersections = locsToFirstPathOccurance.map(_.keys.toSet).reduce(_.intersect(_))
      locsToFirstPathOccurance.map(_.filterKeys(intersections)).combineAll
    }
    val solution = {
      (intersectionsToSummedMinPathLength.minBy(_._2)._2, intersectionsToSummedMinPathLength)
    }
  }

  def in0:String =
    """
      |R8,U5,L5,D3
      |U7,R6,D4,L4
    """.stripMargin
  def in1 =
    """
      |R75,D30,R83,U83,L12,D49,R71,U7,L72
      |U62,R66,U55,R34,D71,R55,D58,R83
      |""".stripMargin
  def in2 =
    """
      |R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
      |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
      |""".stripMargin
  def in =
    """
      |
      |""".stripMargin
}